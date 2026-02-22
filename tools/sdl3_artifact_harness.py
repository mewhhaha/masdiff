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


def seam_limits_for_char(ch: str, scene: str):
    if scene == "default":
        return 10_000, 10_000
    limits = {
        "A": (20, 12),
        "M": (15, 20),
        "P": (10, 40),
        "R": (50, 12),
        "Y": (10, 8),
    }
    return limits.get(ch, (10_000, 10_000))


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


def main():
    p = argparse.ArgumentParser(description="Deterministic SDL artifact checks for masdiff.")
    p.add_argument("--image", required=True, help="captured PNG path")
    p.add_argument("--meta", required=True, help="metadata TSV path")
    p.add_argument(
        "--scene",
        required=True,
        choices=[
            "default",
            "single-a",
            "single-m",
            "single-p",
            "single-r",
            "single-y",
            "single-var-light-a",
            "single-var-light-m",
            "single-var-light-p",
            "single-var-light-r",
            "single-var-light-y",
            "single-var-bold-a",
            "single-var-bold-m",
            "single-var-bold-p",
            "single-var-bold-r",
            "single-var-bold-y",
        ],
    )
    p.add_argument("--json-out", default="", help="optional report path")
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
        probes = [] if args.scene == "default" else norm_regions_for_char(ch)
        probe_reports = [eval_probe(cov_data, probe) for probe in probes]
        edge_report = eval_edge_softness(cov_data)
        pinhole_report = eval_pinhole_artifacts(cov_data, args.scene)
        max_vert, max_horiz = seam_limits_for_char(ch, args.scene)
        seam_report = eval_seam_lines(cov_data, max_vert, max_horiz)
        glyph_reports.append(
            {
                "ch": ch,
                "meta_row": row,
                "fg_luma_estimate": cov_data["fg_l"],
                "probes": probe_reports,
                "edge_softness": edge_report,
                "pinhole_artifacts": pinhole_report,
                "seam_lines": seam_report,
                "pass": all(p0["pass"] for p0 in probe_reports) and edge_report["pass"] and pinhole_report["pass"] and seam_report["pass"],
            }
        )

    passed = (len(missing) == 0) and all(g["pass"] for g in glyph_reports)
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
