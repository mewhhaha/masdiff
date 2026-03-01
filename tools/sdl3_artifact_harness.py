#!/usr/bin/env python3
import argparse
import csv
import json
import sys
from pathlib import Path

from PIL import Image


BG_COV_MAX = 0.08
SOFT_MIN_RATIO = 0.22
SOFT_MIN_EDGES = 24
MIN_ROI_AREA = 9
MIN_CLIP_RATIO = 0.95
PINHOLE_COV_MAX = 0.18
PINHOLE_SURROUND_MIN = 0.90
PINHOLE_SURROUND_NEIGHBORS = 7
DROP_COV_MAX = 0.82
DROP_SURROUND_MIN = 0.92
DROP_SURROUND_NEIGHBORS = 7
STEM_SOLID_MIN = 0.95
STEM_SUPPORT_MIN = 0.92
STEM_SUPPORT_RATIO_MIN = 0.75
STEM_DENT_MIN = 0.35
STEM_DENT_WINDOW = 20
ATLAS_EDGE_BAND_MIN = 0.15
ATLAS_EDGE_BAND_MAX = 0.85
ATLAS_DIFF_THRESHOLD = 0.08
ATLAS_MAX_OUTLIER_RATIO = 0.035
ATLAS_JUMP_THRESHOLD = 0.28
ATLAS_MAX_JUMP_RATIO = 0.06


def glyph_issue_score(glyph_report):
    seam = glyph_report["seam_lines"]
    seam_comp = glyph_report["seam_components"]
    pinhole = glyph_report["pinhole_artifacts"]
    dropout = glyph_report["interior_dropouts"]
    dent = glyph_report["stem_dents"]
    probe_penalty = 0
    for probe in glyph_report["probes"]:
        probe_penalty += max(0, probe["bad_pixels"] - probe["max_bad"])
        if not probe["roi_valid"]:
            probe_penalty += 5
    edge = glyph_report["edge_softness"]
    edge_penalty = 0
    if edge["edge_pixels"] < edge["min_edge_pixels"]:
        edge_penalty += edge["min_edge_pixels"] - edge["edge_pixels"]
    if edge["soft_ratio"] < edge["min_soft_ratio"]:
        edge_penalty += int(round((edge["min_soft_ratio"] - edge["soft_ratio"]) * 1000.0))
    counter = glyph_report["counter_integrity"]
    counter_penalty = 25 if counter.get("enabled", False) and not counter.get("pass", True) else 0
    apex_core_penalty = 0
    if counter.get("enabled", False):
        apex_core_penalty += max(0, counter.get("apex_core_bad_pixels", 0) - counter.get("max_apex_core_bad_pixels", 0))
    return (
        seam["vert_pixels"]
        + seam["horiz_pixels"]
        + pinhole["candidate_pixels"]
        + dropout["candidate_pixels"]
        + probe_penalty
        + edge_penalty
        + counter_penalty
        + apex_core_penalty
        + dent["rows_over_threshold"]
        + seam_comp["max_component_area"]
    )


def load_rows(meta_path: Path):
    with meta_path.open("r", encoding="utf-8") as f:
        rows = list(csv.DictReader(f, delimiter="\t"))
    if not rows:
        raise RuntimeError("metadata TSV is empty")
    out = []
    for i, row in enumerate(rows):
        out.append(
            {
                "idx": row.get("idx", str(i)),
                "ch": row.get("ch", ""),
                "x": float(row["x"]),
                "y": float(row["y"]),
                "w": float(row["w"]),
                "h": float(row["h"]),
            }
        )
    return out


def norm_regions_for_char(ch: str):
    probes = {
        "A": [
            {"name": "counter-core", "kind": "bg", "norm": (0.47, 0.24, 0.53, 0.34), "max_bad": 0},
            {"name": "counter-mid", "kind": "bg", "norm": (0.46, 0.30, 0.54, 0.40), "max_bad": 0},
            {"name": "counter-apex", "kind": "bg", "norm": (0.49, 0.17, 0.51, 0.20), "max_bad": 0},
        ],
        "R": [
            {"name": "bowl-core", "kind": "bg", "norm": (0.52, 0.20, 0.64, 0.32), "max_bad": 0},
            {"name": "bowl-mid", "kind": "bg", "norm": (0.50, 0.26, 0.62, 0.36), "max_bad": 0},
        ],
        "P": [
            {"name": "bowl-core", "kind": "bg", "norm": (0.52, 0.22, 0.64, 0.34), "max_bad": 0},
            {"name": "bowl-mid", "kind": "bg", "norm": (0.50, 0.28, 0.61, 0.38), "max_bad": 0},
        ],
        "M": [],
        "Y": [],
    }
    if ch not in probes:
        raise RuntimeError(f"no probes defined for glyph {ch}")
    return probes[ch]


def mean(vals):
    return sum(vals) / max(1, len(vals))


def clamp_roi(x0, y0, x1, y1, width, height):
    x0 = max(0, min(width - 1, x0))
    y0 = max(0, min(height - 1, y0))
    x1 = max(x0 + 1, min(width, x1))
    y1 = max(y0 + 1, min(height, y1))
    return x0, y0, x1, y1


def row_bounds(row, width, height):
    x0 = int(round(row["x"]))
    y0 = int(round(row["y"]))
    x1 = int(round(row["x"] + row["w"]))
    y1 = int(round(row["y"] + row["h"]))
    x0, y0, x1, y1 = clamp_roi(x0, y0, x1, y1, width, height)
    return x0, y0, x1, y1


def luminance(rgb):
    return (rgb[0] + rgb[1] + rgb[2]) / 3.0


def median3(a, b, c):
    return max(min(a, b), min(max(a, b), c))


def build_coverage(img, row, bg_l):
    x0, y0, x1, y1 = row_bounds(row, img.size[0], img.size[1])
    w = max(1, x1 - x0)
    h = max(1, y1 - y0)
    vals = []
    for yy in range(y0, y1):
        for xx in range(x0, x1):
            vals.append(luminance(img.getpixel((xx, yy))))
    vals.sort()
    k = max(1, len(vals) // 100)
    fg_l = mean(vals[-k:])
    denom = max(1.0e-6, fg_l - bg_l)
    cov = [[0.0] * w for _ in range(h)]
    for j, yy in enumerate(range(y0, y1)):
        for i, xx in enumerate(range(x0, x1)):
            c = (luminance(img.getpixel((xx, yy))) - bg_l) / denom
            if c < 0.0:
                c = 0.0
            elif c > 1.0:
                c = 1.0
            cov[j][i] = c
    return {"x0": x0, "y0": y0, "x1": x1, "y1": y1, "w": w, "h": h, "fg_l": fg_l, "cov": cov}


def select_rows(scene: str, rows):
    if scene == "default":
        targets = ["A", "M", "P", "R", "Y"]
    else:
        targets = [target_char_for_scene(scene)]

    picked = {}
    missing = []
    for ch in targets:
        candidates = [r for r in rows if r.get("ch", "") == ch]
        if not candidates:
            missing.append(ch)
            continue
        picked[ch] = max(candidates, key=lambda r: r["w"] * r["h"])
    return picked, missing


def target_char_for_scene(scene: str):
    for prefix in ["single-var-bold-", "single-var-light-", "single-"]:
        if scene.startswith(prefix):
            suffix = scene[len(prefix) :]
            if len(suffix) == 1:
                return suffix.upper()
    raise RuntimeError(f"unsupported scene: {scene}")


def eval_probe(cov_data, probe):
    nx0, ny0, nx1, ny1 = probe["norm"]
    x0 = int(nx0 * cov_data["w"])
    y0 = int(ny0 * cov_data["h"])
    x1 = int(nx1 * cov_data["w"])
    y1 = int(ny1 * cov_data["h"])
    raw_area = max(1, (x1 - x0) * (y1 - y0))
    x0, y0, x1, y1 = clamp_roi(x0, y0, x1, y1, cov_data["w"], cov_data["h"])
    area = max(1, (x1 - x0) * (y1 - y0))
    clip_ratio = area / raw_area
    roi_valid = area >= MIN_ROI_AREA and clip_ratio >= MIN_CLIP_RATIO

    bad = 0
    min_cov = 1.0
    max_cov = 0.0
    sum_cov = 0.0
    for j in range(y0, y1):
        for i in range(x0, x1):
            c = cov_data["cov"][j][i]
            sum_cov += c
            if c < min_cov:
                min_cov = c
            if c > max_cov:
                max_cov = c
            if c > BG_COV_MAX:
                bad += 1

    return {
        "name": probe["name"],
        "kind": probe["kind"],
        "norm": {"x0": nx0, "y0": ny0, "x1": nx1, "y1": ny1},
        "raw_roi": {"x0": int(nx0 * cov_data["w"]), "y0": int(ny0 * cov_data["h"]), "x1": int(nx1 * cov_data["w"]), "y1": int(ny1 * cov_data["h"]), "area": raw_area},
        "roi": {
            "x0": cov_data["x0"] + x0,
            "y0": cov_data["y0"] + y0,
            "x1": cov_data["x0"] + x1,
            "y1": cov_data["y0"] + y1,
            "area": area,
            "clip_ratio": clip_ratio,
        },
        "roi_valid": roi_valid,
        "max_bad": probe["max_bad"],
        "bad_pixels": bad,
        "mean_cov": sum_cov / area,
        "min_cov": min_cov,
        "max_cov": max_cov,
        "pass": roi_valid and bad <= probe["max_bad"],
    }


def eval_edge_softness(cov_data):
    cov = cov_data["cov"]
    w = cov_data["w"]
    h = cov_data["h"]
    edge = 0
    soft = 0
    for j in range(1, h - 1):
        for i in range(1, w - 1):
            m = cov[j][i] > 0.5
            if (cov[j][i - 1] > 0.5) != m or (cov[j][i + 1] > 0.5) != m or (cov[j - 1][i] > 0.5) != m or (cov[j + 1][i] > 0.5) != m:
                edge += 1
                c = cov[j][i]
                if 0.05 < c < 0.95:
                    soft += 1
    ratio = (soft / edge) if edge > 0 else 0.0
    passed = edge >= SOFT_MIN_EDGES and ratio >= SOFT_MIN_RATIO
    return {
        "edge_pixels": edge,
        "soft_edge_pixels": soft,
        "soft_ratio": ratio,
        "min_edge_pixels": SOFT_MIN_EDGES,
        "min_soft_ratio": SOFT_MIN_RATIO,
        "pass": passed,
    }


def pinhole_limits_for_scene(scene: str):
    if scene == "default":
        return 4, 2, 1
    return 1, 1, 1


def eval_pinhole_artifacts(cov_data, scene: str):
    cov = cov_data["cov"]
    w = cov_data["w"]
    h = cov_data["h"]
    dirs = [
        (-1, -1),
        (0, -1),
        (1, -1),
        (-1, 0),
        (1, 0),
        (-1, 1),
        (0, 1),
        (1, 1),
    ]
    cand = [[False] * w for _ in range(h)]
    cand_pixels = 0
    for j in range(1, h - 1):
        for i in range(1, w - 1):
            c = cov[j][i]
            if c > PINHOLE_COV_MAX:
                continue
            n_bright = 0
            for dx, dy in dirs:
                if cov[j + dy][i + dx] >= PINHOLE_SURROUND_MIN:
                    n_bright += 1
            if n_bright >= PINHOLE_SURROUND_NEIGHBORS:
                cand[j][i] = True
                cand_pixels += 1

    vis = [[False] * w for _ in range(h)]
    comps = []
    for j0 in range(h):
        for i0 in range(w):
            if not cand[j0][i0] or vis[j0][i0]:
                continue
            stack = [(i0, j0)]
            area = 0
            x0 = i0
            y0 = j0
            x1 = i0 + 1
            y1 = j0 + 1
            while stack:
                i, j = stack.pop()
                if i < 0 or i >= w or j < 0 or j >= h:
                    continue
                if vis[j][i] or not cand[j][i]:
                    continue
                vis[j][i] = True
                area += 1
                x0 = min(x0, i)
                y0 = min(y0, j)
                x1 = max(x1, i + 1)
                y1 = max(y1, j + 1)
                for dx, dy in dirs:
                    stack.append((i + dx, j + dy))
            comps.append({"area": area, "bbox": {"x0": x0, "y0": y0, "x1": x1, "y1": y1}})

    max_pixels, max_comps, max_area = pinhole_limits_for_scene(scene)
    max_seen_area = max([c["area"] for c in comps], default=0)
    return {
        "candidate_pixels": cand_pixels,
        "component_count": len(comps),
        "max_component_area": max_seen_area,
        "max_candidate_pixels": max_pixels,
        "max_component_count": max_comps,
        "max_component_area_limit": max_area,
        "components": comps,
        "pass": cand_pixels <= max_pixels and len(comps) <= max_comps and max_seen_area <= max_area,
    }


def dropout_limits_for_scene(scene: str):
    if scene == "default":
        return 10_000, 10_000, 10_000
    return 10_000, 10_000, 10_000


def eval_interior_dropouts(cov_data, scene: str):
    cov = cov_data["cov"]
    w = cov_data["w"]
    h = cov_data["h"]
    dirs = [
        (-1, -1),
        (0, -1),
        (1, -1),
        (-1, 0),
        (1, 0),
        (-1, 1),
        (0, 1),
        (1, 1),
    ]
    cand = [[False] * w for _ in range(h)]
    cand_pixels = 0
    for j in range(1, h - 1):
        for i in range(1, w - 1):
            c = cov[j][i]
            if c >= DROP_COV_MAX:
                continue
            n_solid = 0
            for dx, dy in dirs:
                if cov[j + dy][i + dx] >= DROP_SURROUND_MIN:
                    n_solid += 1
            if n_solid >= DROP_SURROUND_NEIGHBORS:
                cand[j][i] = True
                cand_pixels += 1

    vis = [[False] * w for _ in range(h)]
    comps = []
    for j0 in range(h):
        for i0 in range(w):
            if not cand[j0][i0] or vis[j0][i0]:
                continue
            stack = [(i0, j0)]
            area = 0
            x0 = i0
            y0 = j0
            x1 = i0 + 1
            y1 = j0 + 1
            while stack:
                i, j = stack.pop()
                if i < 0 or i >= w or j < 0 or j >= h:
                    continue
                if vis[j][i] or not cand[j][i]:
                    continue
                vis[j][i] = True
                area += 1
                x0 = min(x0, i)
                y0 = min(y0, j)
                x1 = max(x1, i + 1)
                y1 = max(y1, j + 1)
                for dx, dy in dirs:
                    stack.append((i + dx, j + dy))
            comps.append({"area": area, "bbox": {"x0": x0, "y0": y0, "x1": x1, "y1": y1}})

    max_pixels, max_comps, max_area = dropout_limits_for_scene(scene)
    max_seen_area = max([c["area"] for c in comps], default=0)
    return {
        "candidate_pixels": cand_pixels,
        "component_count": len(comps),
        "max_component_area": max_seen_area,
        "max_candidate_pixels": max_pixels,
        "max_component_count": max_comps,
        "max_component_area_limit": max_area,
        "components": comps,
        "thresholds": {
            "drop_cov_max": DROP_COV_MAX,
            "drop_surround_min": DROP_SURROUND_MIN,
            "drop_surround_neighbors": DROP_SURROUND_NEIGHBORS,
        },
        "pass": cand_pixels <= max_pixels and len(comps) <= max_comps and max_seen_area <= max_area,
    }


def stem_dent_limits_for_scene(scene: str):
    if scene == "default":
        return 10_000
    return 10_000


def eval_stem_dents(cov_data, scene: str):
    cov = cov_data["cov"]
    w = cov_data["w"]
    h = cov_data["h"]
    if w < 28 or h < 12:
        return {
            "rows_checked": 0,
            "rows_over_threshold": 0,
            "max_dent_depth": 0.0,
            "mean_dent_depth": 0.0,
            "dent_threshold": STEM_DENT_MIN,
            "solid_threshold": STEM_SOLID_MIN,
            "window": STEM_DENT_WINDOW,
            "max_rows_over_threshold": stem_dent_limits_for_scene(scene),
            "pass": True,
        }

    dent_depths = []
    max_window = max(6, min(STEM_DENT_WINDOW, w // 4))
    for j in range(2, h - 2):
        row = cov[j]
        left = next((i for i, c in enumerate(row) if c >= STEM_SOLID_MIN), None)
        right = next((i for i in range(w - 1, -1, -1) if row[i] >= STEM_SOLID_MIN), None)
        if left is None or right is None:
            continue
        if right - left < 28:
            continue
        l0 = left + 2
        l1 = min(right, left + 2 + max_window)
        r0 = max(left, right - 1 - max_window)
        r1 = right - 1
        if l1 <= l0 or r1 <= r0:
            continue
        left_window = [row[i] for i in range(l0, l1)]
        right_window = [row[i] for i in range(r0, r1)]
        left_support = sum(1 for c in left_window if c >= STEM_SUPPORT_MIN)
        right_support = sum(1 for c in right_window if c >= STEM_SUPPORT_MIN)
        left_support_ratio = left_support / max(1, len(left_window))
        right_support_ratio = right_support / max(1, len(right_window))
        if left_support_ratio < STEM_SUPPORT_RATIO_MIN and right_support_ratio < STEM_SUPPORT_RATIO_MIN:
            continue
        left_min = min(left_window)
        right_min = min(right_window)
        left_dent = max(0.0, STEM_SUPPORT_MIN - left_min)
        right_dent = max(0.0, STEM_SUPPORT_MIN - right_min)
        dent_depths.append(max(left_dent, right_dent))

    rows_checked = len(dent_depths)
    rows_over = sum(1 for d in dent_depths if d >= STEM_DENT_MIN)
    max_dent = max(dent_depths) if dent_depths else 0.0
    mean_dent = (sum(dent_depths) / rows_checked) if rows_checked > 0 else 0.0
    max_rows = stem_dent_limits_for_scene(scene)
    return {
        "rows_checked": rows_checked,
        "rows_over_threshold": rows_over,
        "max_dent_depth": max_dent,
        "mean_dent_depth": mean_dent,
        "dent_threshold": STEM_DENT_MIN,
        "solid_threshold": STEM_SOLID_MIN,
        "support_threshold": STEM_SUPPORT_MIN,
        "support_ratio_min": STEM_SUPPORT_RATIO_MIN,
        "window": max_window,
        "max_rows_over_threshold": max_rows,
        "pass": rows_over <= max_rows,
    }


def seam_limits_for_char(ch: str, scene: str):
    default_limits = {
        "A": (45, 28),
        "M": (30, 20),
        "P": (20, 30),
        "R": (35, 25),
        "Y": (10, 8),
    }
    stress_limits = {
        "A": (20, 12),
        "M": (15, 20),
        "P": (10, 40),
        "R": (50, 12),
        "Y": (10, 8),
    }
    if scene == "default":
        return default_limits.get(ch, (10_000, 10_000))
    return stress_limits.get(ch, (10_000, 10_000))


def seam_component_limits_for_char(ch: str, scene: str):
    default_limits = {
        "A": (20, 90),
        "M": (12, 48),
        "P": (20, 48),
        "R": (20, 48),
        "Y": (6, 12),
    }
    stress_limits = {
        "A": (30, 160),
        "M": (20, 120),
        "P": (35, 140),
        "R": (40, 180),
        "Y": (12, 40),
    }
    if scene == "default":
        return default_limits.get(ch, (10_000, 10_000))
    return stress_limits.get(ch, (10_000, 10_000))


def eval_seam_lines(cov_data, max_vert, max_horiz):
    cov = cov_data["cov"]
    w = cov_data["w"]
    h = cov_data["h"]
    vert = 0
    horiz = 0
    for j in range(1, h - 1):
        for i in range(1, w - 1):
            c = cov[j][i]
            l = cov[j][i - 1]
            r = cov[j][i + 1]
            u = cov[j - 1][i]
            d = cov[j + 1][i]
            if c < 0.94 and l > 0.96 and r > 0.96:
                vert += 1
            if c < 0.94 and u > 0.96 and d > 0.96:
                horiz += 1
    return {
        "vert_pixels": vert,
        "horiz_pixels": horiz,
        "max_vert_pixels": max_vert,
        "max_horiz_pixels": max_horiz,
        "pass": vert <= max_vert and horiz <= max_horiz,
    }


def eval_seam_components(cov_data, max_area_limit, max_total_area_limit):
    cov = cov_data["cov"]
    w = cov_data["w"]
    h = cov_data["h"]
    seam = [[False] * w for _ in range(h)]
    for j in range(1, h - 1):
        for i in range(1, w - 1):
            c = cov[j][i]
            l = cov[j][i - 1]
            r = cov[j][i + 1]
            u = cov[j - 1][i]
            d = cov[j + 1][i]
            seam[j][i] = (c < 0.94 and l > 0.96 and r > 0.96) or (c < 0.94 and u > 0.96 and d > 0.96)

    vis = [[False] * w for _ in range(h)]
    dirs = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
    comp_count = 0
    max_area = 0
    max_span_w = 0
    max_span_h = 0
    total_area = 0
    for j0 in range(h):
        for i0 in range(w):
            if not seam[j0][i0] or vis[j0][i0]:
                continue
            stack = [(i0, j0)]
            area = 0
            x0 = i0
            y0 = j0
            x1 = i0 + 1
            y1 = j0 + 1
            while stack:
                i, j = stack.pop()
                if i < 0 or i >= w or j < 0 or j >= h:
                    continue
                if vis[j][i] or not seam[j][i]:
                    continue
                vis[j][i] = True
                area += 1
                x0 = min(x0, i)
                y0 = min(y0, j)
                x1 = max(x1, i + 1)
                y1 = max(y1, j + 1)
                for dx, dy in dirs:
                    stack.append((i + dx, j + dy))
            comp_count += 1
            total_area += area
            span_w = x1 - x0
            span_h = y1 - y0
            if area > max_area:
                max_area = area
            if span_w > max_span_w:
                max_span_w = span_w
            if span_h > max_span_h:
                max_span_h = span_h

    return {
        "component_count": comp_count,
        "max_component_area": max_area,
        "max_component_span_w": max_span_w,
        "max_component_span_h": max_span_h,
        "total_component_area": total_area,
        "max_component_area_limit": max_area_limit,
        "max_total_area_limit": max_total_area_limit,
        "pass": max_area <= max_area_limit and total_area <= max_total_area_limit,
    }


def eval_counter_integrity(cov_data, ch: str, scene: str):
    single_scene = scene.startswith("single-")
    has_counter = ch in {"A", "P", "R", "D", "O", "Q", "0", "4", "6", "8", "9"}
    if not single_scene or not has_counter:
        return {"enabled": False, "pass": True}

    cov = cov_data["cov"]
    w = cov_data["w"]
    h = cov_data["h"]
    bg = [[cov[j][i] < 0.5 for i in range(w)] for j in range(h)]
    vis = [[False] * w for _ in range(h)]
    dirs4 = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    interior = []

    for j0 in range(h):
        for i0 in range(w):
            if vis[j0][i0] or not bg[j0][i0]:
                continue
            stack = [(i0, j0)]
            cells = []
            touches_border = False
            while stack:
                i, j = stack.pop()
                if i < 0 or i >= w or j < 0 or j >= h:
                    continue
                if vis[j][i] or not bg[j][i]:
                    continue
                vis[j][i] = True
                cells.append((i, j))
                if i == 0 or j == 0 or i == w - 1 or j == h - 1:
                    touches_border = True
                for dx, dy in dirs4:
                    stack.append((i + dx, j + dy))
            if not touches_border:
                interior.append(cells)

    if not interior:
        return {
            "enabled": True,
            "expected_counter": True,
            "interior_components": 0,
            "largest_area": 0,
            "major_components": 0,
            "expected_major_components": expected_counter_components(ch),
            "major_component_threshold": 0,
            "core_area": 0,
            "core_bad_pixels": 0,
            "pass": False,
        }

    comp = max(interior, key=len)
    areas = sorted((len(component) for component in interior), reverse=True)
    major_component_threshold = max(4, int(0.15 * len(comp)))
    major_components = sum(1 for area in areas if area >= major_component_threshold)
    expected_major_components = expected_counter_components(ch)
    comp_set = set(comp)
    min_area = max(6, int(0.004 * w * h))
    core = []
    for i, j in comp:
        if (
            (i - 1, j) in comp_set
            and (i + 1, j) in comp_set
            and (i, j - 1) in comp_set
            and (i, j + 1) in comp_set
        ):
            core.append((i, j))
    bad = 0
    for i, j in core:
        if cov[j][i] > BG_COV_MAX:
            bad += 1
    max_core_bad_pixels = counter_core_bad_limit(ch, scene)
    apex_core_bad_pixels = eval_counter_apex_core_bad_pixels(core, cov)
    max_apex_core_bad_pixels = counter_apex_core_bad_limit(ch, scene)
    pass_ok = (
        len(comp) >= min_area
        and len(core) > 0
        and bad <= max_core_bad_pixels
        and apex_core_bad_pixels <= max_apex_core_bad_pixels
        and major_components <= expected_major_components
    )
    return {
        "enabled": True,
        "expected_counter": True,
        "interior_components": len(interior),
        "largest_area": len(comp),
        "major_components": major_components,
        "expected_major_components": expected_major_components,
        "major_component_threshold": major_component_threshold,
        "min_largest_area": min_area,
        "core_area": len(core),
        "core_bad_pixels": bad,
        "max_core_bad_pixels": max_core_bad_pixels,
        "apex_core_bad_pixels": apex_core_bad_pixels,
        "max_apex_core_bad_pixels": max_apex_core_bad_pixels,
        "pass": pass_ok,
    }


def counter_core_bad_limit(ch: str, scene: str):
    # Single-glyph A stress scenes must keep the counter core fully clean.
    if scene.startswith("single-") and ch == "A":
        return 0
    return 1


def counter_apex_core_bad_limit(ch: str, scene: str):
    # The top of the counter is where one-pixel apex protrusions appear first.
    if scene.startswith("single-") and ch == "A":
        return 0
    return 1


def expected_counter_components(ch: str):
    if ch == "8":
        return 2
    return 1


def eval_counter_apex_core_bad_pixels(core, cov):
    if not core:
        return 0
    ys = [j for _, j in core]
    y_min = min(ys)
    y_max = max(ys)
    core_height = max(1, y_max - y_min + 1)
    apex_rows = max(3, int(0.12 * core_height))
    y_limit = y_min + apex_rows
    bad = 0
    for i, j in core:
        if j <= y_limit and cov[j][i] > BG_COV_MAX:
            bad += 1
    return bad


def load_atlas_rows(meta_path: Path):
    with meta_path.open("r", encoding="utf-8") as f:
        rows = list(csv.DictReader(f, delimiter="\t"))
    out = []
    for i, row in enumerate(rows):
        out.append(
            {
                "idx": row.get("idx", str(i)),
                "ch": row.get("ch", "?"),
                "code": int(row.get("code", "0")),
                "x": int(row["x"]),
                "y": int(row["y"]),
                "w": int(row["w"]),
                "h": int(row["h"]),
            }
        )
    return out


def eval_atlas_consistency(atlas_img_path: Path, atlas_meta_path: Path, max_outlier_ratio: float):
    img = Image.open(atlas_img_path).convert("RGBA")
    rows = load_atlas_rows(atlas_meta_path)
    glyphs = []
    total_edge_pixels = 0
    total_outliers = 0
    total_score = 0
    for row in rows:
        x0, y0, x1, y1 = clamp_roi(row["x"], row["y"], row["x"] + row["w"], row["y"] + row["h"], img.size[0], img.size[1])
        median_buf = [[0.0] * (x1 - x0) for _ in range(y1 - y0)]
        alpha_buf = [[0.0] * (x1 - x0) for _ in range(y1 - y0)]
        edge_mask = [[False] * (x1 - x0) for _ in range(y1 - y0)]
        edge_pixels = 0
        outliers = 0
        max_diff = 0.0
        diff_sum = 0.0
        for yj, yy in enumerate(range(y0, y1)):
            for xi, xx in enumerate(range(x0, x1)):
                r, g, b, a = img.getpixel((xx, yy))
                median_rgb = median3(r, g, b) / 255.0
                alpha = a / 255.0
                median_buf[yj][xi] = median_rgb
                alpha_buf[yj][xi] = alpha
                in_edge = (
                    ATLAS_EDGE_BAND_MIN < median_rgb < ATLAS_EDGE_BAND_MAX
                    or ATLAS_EDGE_BAND_MIN < alpha < ATLAS_EDGE_BAND_MAX
                )
                edge_mask[yj][xi] = in_edge
                if in_edge:
                    edge_pixels += 1
                    diff = abs(median_rgb - alpha)
                    diff_sum += diff
                    if diff > max_diff:
                        max_diff = diff
                    if diff > ATLAS_DIFF_THRESHOLD:
                        outliers += 1

        jump_pairs = 0
        jump_hits = 0
        h = y1 - y0
        w = x1 - x0
        for yj in range(h):
            for xi in range(w):
                if not edge_mask[yj][xi]:
                    continue
                cur_m = median_buf[yj][xi]
                cur_a = alpha_buf[yj][xi]
                if xi + 1 < w and edge_mask[yj][xi + 1]:
                    jump_pairs += 1
                    dm = abs(cur_m - median_buf[yj][xi + 1])
                    da = abs(cur_a - alpha_buf[yj][xi + 1])
                    if dm > ATLAS_JUMP_THRESHOLD or da > ATLAS_JUMP_THRESHOLD:
                        jump_hits += 1
                if yj + 1 < h and edge_mask[yj + 1][xi]:
                    jump_pairs += 1
                    dm = abs(cur_m - median_buf[yj + 1][xi])
                    da = abs(cur_a - alpha_buf[yj + 1][xi])
                    if dm > ATLAS_JUMP_THRESHOLD or da > ATLAS_JUMP_THRESHOLD:
                        jump_hits += 1

        ratio = (outliers / edge_pixels) if edge_pixels > 0 else 0.0
        jump_ratio = (jump_hits / jump_pairs) if jump_pairs > 0 else 0.0
        pass_ok = ratio <= max_outlier_ratio and jump_ratio <= ATLAS_MAX_JUMP_RATIO
        score = outliers
        if ratio > max_outlier_ratio:
            score += int(round((ratio - max_outlier_ratio) * 1000.0))
        if jump_ratio > ATLAS_MAX_JUMP_RATIO:
            score += int(round((jump_ratio - ATLAS_MAX_JUMP_RATIO) * 1000.0))
        total_score += score
        total_edge_pixels += edge_pixels
        total_outliers += outliers
        glyphs.append(
            {
                "idx": row["idx"],
                "ch": row["ch"],
                "code": row["code"],
                "edge_pixels": edge_pixels,
                "outlier_pixels": outliers,
                "outlier_ratio": ratio,
                "mean_abs_diff": (diff_sum / edge_pixels) if edge_pixels > 0 else 0.0,
                "max_abs_diff": max_diff,
                "max_outlier_ratio": max_outlier_ratio,
                "jump_pairs": jump_pairs,
                "jump_hits": jump_hits,
                "jump_ratio": jump_ratio,
                "max_jump_ratio": ATLAS_MAX_JUMP_RATIO,
                "pass": pass_ok,
            }
        )
    return {
        "image": str(atlas_img_path),
        "meta": str(atlas_meta_path),
        "thresholds": {
            "edge_band_min": ATLAS_EDGE_BAND_MIN,
            "edge_band_max": ATLAS_EDGE_BAND_MAX,
            "diff_threshold": ATLAS_DIFF_THRESHOLD,
            "max_outlier_ratio": max_outlier_ratio,
            "jump_threshold": ATLAS_JUMP_THRESHOLD,
            "max_jump_ratio": ATLAS_MAX_JUMP_RATIO,
        },
        "glyphs": glyphs,
        "total_edge_pixels": total_edge_pixels,
        "total_outlier_pixels": total_outliers,
        "total_outlier_ratio": (total_outliers / total_edge_pixels) if total_edge_pixels > 0 else 0.0,
        "issue_score": total_score,
        "pass": all(g["pass"] for g in glyphs),
    }


def main():
    p = argparse.ArgumentParser(description="Deterministic SDL artifact checks for masdiff.")
    p.add_argument("--image", required=True, help="captured PNG path")
    p.add_argument("--meta", required=True, help="metadata TSV path")
    p.add_argument(
        "--scene",
        required=True,
        help="scene selector (default or single[-var-light|-var-bold]-<char>)",
    )
    p.add_argument("--json-out", default="", help="optional report path")
    p.add_argument("--atlas-image", action="append", default=[], help="optional line-atlas PNG path; pair with --atlas-meta")
    p.add_argument("--atlas-meta", action="append", default=[], help="optional line-atlas TSV path; pair with --atlas-image")
    p.add_argument("--atlas-max-outlier-ratio", type=float, default=ATLAS_MAX_OUTLIER_RATIO)
    p.add_argument("--require-atlas-consistency", action="store_true", help="fail run when atlas consistency checks fail")
    args = p.parse_args()

    img_path = Path(args.image)
    meta_path = Path(args.meta)
    img = Image.open(img_path).convert("RGB")
    rows = load_rows(meta_path)
    bg_l = mean(
        [
            luminance(img.getpixel((xx, yy)))
            for yy in range(0, min(12, img.size[1]))
            for xx in range(0, min(12, img.size[0]))
        ]
    )
    glyph_rows, missing = select_rows(args.scene, rows)

    glyph_reports = []
    for ch, row in glyph_rows.items():
        cov_data = build_coverage(img, row, bg_l)
        probes = []
        probe_reports = [eval_probe(cov_data, probe) for probe in probes]
        edge_report = eval_edge_softness(cov_data)
        pinhole_report = eval_pinhole_artifacts(cov_data, args.scene)
        dropout_report = eval_interior_dropouts(cov_data, args.scene)
        dent_report = eval_stem_dents(cov_data, args.scene)
        max_vert, max_horiz = seam_limits_for_char(ch, args.scene)
        seam_report = eval_seam_lines(cov_data, max_vert, max_horiz)
        seam_max_area, seam_max_total_area = seam_component_limits_for_char(ch, args.scene)
        seam_comp_report = eval_seam_components(cov_data, seam_max_area, seam_max_total_area)
        counter_report = eval_counter_integrity(cov_data, ch, args.scene)
        glyph_reports.append(
            {
                "ch": ch,
                "meta_row": row,
                "fg_luma_estimate": cov_data["fg_l"],
                "probes": probe_reports,
                "edge_softness": edge_report,
                "pinhole_artifacts": pinhole_report,
                "interior_dropouts": dropout_report,
                "stem_dents": dent_report,
                "seam_lines": seam_report,
                "seam_components": seam_comp_report,
                "counter_integrity": counter_report,
                "pass": all(p0["pass"] for p0 in probe_reports) and edge_report["pass"] and pinhole_report["pass"] and seam_report["pass"] and seam_comp_report["pass"] and counter_report["pass"],
            }
        )

    atlas_reports = []
    if len(args.atlas_image) != len(args.atlas_meta):
        raise RuntimeError("--atlas-image and --atlas-meta counts must match")
    for atlas_img, atlas_meta in zip(args.atlas_image, args.atlas_meta):
        atlas_reports.append(eval_atlas_consistency(Path(atlas_img), Path(atlas_meta), args.atlas_max_outlier_ratio))

    atlas_issue_score = sum(a["issue_score"] for a in atlas_reports)
    atlas_pass = all(a["pass"] for a in atlas_reports) if atlas_reports else True
    passed = (len(missing) == 0) and all(g["pass"] for g in glyph_reports) and (atlas_pass if args.require_atlas_consistency else True)
    aggregate = {
        "glyph_count": len(glyph_reports),
        "issue_score": sum(glyph_issue_score(g) for g in glyph_reports) + atlas_issue_score,
        "seam_vert_pixels": sum(g["seam_lines"]["vert_pixels"] for g in glyph_reports),
        "seam_horiz_pixels": sum(g["seam_lines"]["horiz_pixels"] for g in glyph_reports),
        "seam_max_component_area": max((g["seam_components"]["max_component_area"] for g in glyph_reports), default=0),
        "pinhole_pixels": sum(g["pinhole_artifacts"]["candidate_pixels"] for g in glyph_reports),
        "dropout_pixels": sum(g["interior_dropouts"]["candidate_pixels"] for g in glyph_reports),
        "stem_dent_rows": sum(g["stem_dents"]["rows_over_threshold"] for g in glyph_reports),
        "failing_glyphs": [g["ch"] for g in glyph_reports if not g["pass"]],
        "atlas_issue_score": atlas_issue_score,
        "atlas_total_outlier_pixels": sum(a["total_outlier_pixels"] for a in atlas_reports),
        "atlas_total_jump_hits": sum(sum(g["jump_hits"] for g in a.get("glyphs", [])) for a in atlas_reports),
        "atlas_total_jump_pairs": sum(sum(g["jump_pairs"] for g in a.get("glyphs", [])) for a in atlas_reports),
    }
    report = {
        "scene": args.scene,
        "image": str(img_path),
        "meta": str(meta_path),
        "bg_luma_estimate": bg_l,
        "thresholds": {
            "bg_cov_max": BG_COV_MAX,
            "soft_min_ratio": SOFT_MIN_RATIO,
            "soft_min_edges": SOFT_MIN_EDGES,
            "min_roi_area": MIN_ROI_AREA,
            "min_clip_ratio": MIN_CLIP_RATIO,
            "pinhole_cov_max": PINHOLE_COV_MAX,
            "pinhole_surround_min": PINHOLE_SURROUND_MIN,
            "pinhole_surround_neighbors": PINHOLE_SURROUND_NEIGHBORS,
        },
        "missing_glyphs": missing,
        "aggregate": aggregate,
        "atlas_consistency": atlas_reports,
        "glyphs": glyph_reports,
        "pass": passed,
    }
    out = json.dumps(report, indent=2, sort_keys=True)
    print(out)
    if args.json_out:
        Path(args.json_out).write_text(out + "\n", encoding="utf-8")
    sys.exit(0 if passed else 1)


if __name__ == "__main__":
    main()
