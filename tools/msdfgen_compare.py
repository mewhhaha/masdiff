#!/usr/bin/env python3
"""Compare masdiff glyph bitmaps against msdfgen outputs.

This tool expects:
  - masdiff outputs from `msdf-dump` (raw + json per glyph)
  - msdfgen PNGs named as U+XXXX.png in a reference directory

Optionally it can invoke msdfgen to generate the reference images using a
user-supplied command template.
"""

from __future__ import annotations

import argparse
import json
import os
import shlex
import subprocess
import sys
from dataclasses import dataclass
from typing import Iterable, List, Optional, Tuple

from PIL import Image


@dataclass
class GlyphMeta:
    codepoint: int
    width: int
    height: int
    channels: int
    range: int
    format: str
    offset_x: float
    offset_y: float
    pixel_size: int
    units_per_em: int
    scale: float
    translate_x: float
    translate_y: float


def parse_codepoints(text: Optional[str], cps: Optional[str]) -> List[int]:
    if text:
        return sorted(set(ord(ch) for ch in text))
    if not cps:
        return []
    out: List[int] = []
    for part in cps.split(","):
        p = part.strip().upper()
        if p.startswith("U+"):
            p = p[2:]
        if p.startswith("0X"):
            p = p[2:]
        out.append(int(p, 16))
    return sorted(set(out))


def name_for(cp: int) -> str:
    width = 4 if cp <= 0xFFFF else 6
    return f"U+{cp:0{width}X}"


def load_meta(path: str) -> GlyphMeta:
    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)
    return GlyphMeta(
        codepoint=data["codepoint"],
        width=data["width"],
        height=data["height"],
        channels=data["channels"],
        range=data["range"],
        format=data["format"],
        offset_x=data.get("offsetX", 0),
        offset_y=data.get("offsetY", 0),
        pixel_size=data.get("pixelSize", 0),
        units_per_em=data.get("unitsPerEm", 0),
        scale=data.get("scale", 0.0),
        translate_x=data.get("translateX", 0.0),
        translate_y=data.get("translateY", 0.0),
    )


def load_raw(path: str, width: int, height: int, channels: int) -> Image.Image:
    with open(path, "rb") as f:
        data = f.read()
    expected = width * height * channels
    if len(data) != expected:
        raise ValueError(f"raw size mismatch for {path}: got {len(data)} bytes, expected {expected}")
    mode = "RGBA" if channels == 4 else "RGB"
    img = Image.frombytes(mode, (width, height), data)
    if mode != "RGBA":
        img = img.convert("RGBA")
    return img


def load_ref_png(path: str) -> Image.Image:
    return Image.open(path).convert("RGBA")


def median3(a: float, b: float, c: float) -> float:
    return max(min(a, b), min(max(a, b), c))


def decode_sd(img: Image.Image, fmt: str) -> List[List[float]]:
    px = img.load()
    w, h = img.size
    use_alpha = False
    if fmt == "mtsdf":
        # detect alpha usage
        for y in range(h):
            for x in range(w):
                if px[x, y][3] != 255:
                    use_alpha = True
                    break
            if use_alpha:
                break
    sd: List[List[float]] = [[0.0 for _ in range(w)] for _ in range(h)]
    for y in range(h):
        for x in range(w):
            r, g, b, a = px[x, y]
            if fmt == "mtsdf" and use_alpha:
                v = a / 255.0
            else:
                v = median3(r / 255.0, g / 255.0, b / 255.0)
            sd[y][x] = v - 0.5
    return sd


def inside_bbox(sd: List[List[float]]) -> Optional[Tuple[int, int, int, int]]:
    h = len(sd)
    w = len(sd[0]) if h else 0
    minx, miny = w, h
    maxx, maxy = -1, -1
    for y in range(h):
        row = sd[y]
        for x in range(w):
            if row[x] > 0:
                if x < minx:
                    minx = x
                if y < miny:
                    miny = y
                if x > maxx:
                    maxx = x
                if y > maxy:
                    maxy = y
    if maxx < minx or maxy < miny:
        return None
    return (minx, miny, maxx, maxy)


def diff_sd(sd_a: List[List[float]], sd_b: List[List[float]], range_px: int) -> Tuple[float, float, int]:
    bbox_a = inside_bbox(sd_a)
    bbox_b = inside_bbox(sd_b)
    if bbox_a is None or bbox_b is None:
        return (0.0, 0.0, 0)
    ax0, ay0, ax1, ay1 = bbox_a
    bx0, by0, bx1, by1 = bbox_b
    w = min(ax1 - ax0 + 1, bx1 - bx0 + 1)
    h = min(ay1 - ay0 + 1, by1 - by0 + 1)
    if w <= 0 or h <= 0:
        return (0.0, 0.0, 0)

    stable = 1.0 / (2.0 * max(1, range_px))
    total = 0.0
    max_diff = 0.0
    mismatch = 0
    count = 0
    for y in range(h):
        row_a = sd_a[ay0 + y]
        row_b = sd_b[by0 + y]
        for x in range(w):
            da = row_a[ax0 + x]
            db = row_b[bx0 + x]
            diff = abs(da - db)
            total += diff
            if diff > max_diff:
                max_diff = diff
            if abs(da) >= stable and abs(db) >= stable:
                if (da > 0) != (db > 0):
                    mismatch += 1
            count += 1
    mean_diff = total / count if count else 0.0
    return (max_diff, mean_diff, mismatch)


def run_msdfgen(cmd_template: str, msdfgen_bin: str, fmt: str, font: str, size: int, range_px: int, cp: int, out_path: str) -> None:
    cmd = cmd_template.format(
        bin=msdfgen_bin,
        format=fmt,
        font=font,
        out=out_path,
        size=size,
        range=range_px,
        codepoint=cp,
        codepoint_hex=f"0x{cp:X}",
        codepoint_u=f"U+{cp:04X}",
    )
    args = shlex.split(cmd)
    subprocess.run(args, check=True)


def run_msdfgen_match(msdfgen_bin: str, fmt: str, font: str, meta: GlyphMeta, out_path: str, yflip: bool, range_override: Optional[int]) -> None:
    range_px = range_override if range_override is not None else meta.range
    scale = meta.scale
    if scale == 0 and meta.units_per_em > 0 and meta.pixel_size > 0:
        scale = meta.pixel_size / meta.units_per_em
    translate_x = meta.translate_x
    translate_y = meta.translate_y
    if translate_x == 0 and translate_y == 0 and scale != 0:
        translate_x = -meta.offset_x / scale
        translate_y = -meta.offset_y / scale

    args = [
        msdfgen_bin,
        fmt,
        "-font",
        font,
        f"0x{meta.codepoint:X}",
        "-o",
        out_path,
        "-dimensions",
        str(meta.width),
        str(meta.height),
        "-pxrange",
        str(range_px),
        "-noemnormalize",
        "-scale",
        f"{scale:.8f}",
        "-translate",
        f"{translate_x:.8f}",
        f"{translate_y:.8f}",
    ]
    if yflip:
        args.append("-yflip")
    subprocess.run(args, check=True)


def main() -> int:
    parser = argparse.ArgumentParser(description="Compare masdiff vs msdfgen glyph outputs.")
    parser.add_argument("--text", help="text to compare (default: masdiff)")
    parser.add_argument("--codepoints", help="comma-separated hex codepoints (e.g. 0041,0062)")
    parser.add_argument("--format", choices=["msdf", "mtsdf"], default="msdf")
    parser.add_argument("--our-dir", required=True, help="directory with masdiff raw+json outputs")
    parser.add_argument("--ref-dir", required=True, help="directory with msdfgen PNG outputs")
    parser.add_argument("--range", type=int, default=12, help="px range (for stability threshold)")
    parser.add_argument("--fail-max", type=float, default=None, help="fail if max diff exceeds this")
    parser.add_argument("--fail-mean", type=float, default=None, help="fail if mean diff exceeds this")
    parser.add_argument("--generate", action="store_true", help="run msdfgen to generate reference PNGs")
    parser.add_argument("--match-masdiff", action="store_true", help="match msdfgen output to masdiff glyph dimensions/offsets")
    parser.add_argument("--msdfgen", default="msdfgen", help="msdfgen binary path")
    parser.add_argument("--font", default=None, help="font path for msdfgen")
    parser.add_argument("--size", type=int, default=128, help="size for msdfgen")
    parser.add_argument("--yflip", dest="yflip", action="store_true", help="apply -yflip to msdfgen output")
    parser.add_argument("--no-yflip", dest="yflip", action="store_false", help="disable -yflip in msdfgen output")
    parser.set_defaults(yflip=True)
    parser.add_argument(
        "--cmd",
        default="{bin} {format} -font {font} {codepoint_hex} -o {out} -dimensions {size} {size} -pxrange {range} -autoframe",
        help="msdfgen command template",
    )
    args = parser.parse_args()

    cps = parse_codepoints(args.text or "masdiff", args.codepoints)
    if not cps:
        print("no codepoints provided")
        return 1

    if args.generate:
        if not args.font:
            print("--font is required with --generate")
            return 1
        os.makedirs(args.ref_dir, exist_ok=True)
        for cp in cps:
            out_path = os.path.join(args.ref_dir, name_for(cp) + ".png")
            if args.match_masdiff:
                meta_path = os.path.join(args.our_dir, name_for(cp) + ".json")
                if not os.path.exists(meta_path):
                    print(f"missing masdiff meta for {name_for(cp)}")
                    return 1
                meta = load_meta(meta_path)
                run_msdfgen_match(args.msdfgen, args.format, args.font, meta, out_path, args.yflip, args.range)
            else:
                run_msdfgen(args.cmd, args.msdfgen, args.format, args.font, args.size, args.range, cp, out_path)

    max_diff_all = 0.0
    mean_sum = 0.0
    mean_count = 0
    mismatch_sum = 0

    for cp in cps:
        name = name_for(cp)
        ref_path = os.path.join(args.ref_dir, name + ".png")
        meta_path = os.path.join(args.our_dir, name + ".json")
        raw_path = os.path.join(args.our_dir, name + ".raw")
        if not os.path.exists(ref_path):
            print(f"missing reference: {ref_path}")
            return 1
        if not os.path.exists(meta_path) or not os.path.exists(raw_path):
            print(f"missing masdiff output for {name}")
            return 1

        meta = load_meta(meta_path)
        our_img = load_raw(raw_path, meta.width, meta.height, meta.channels)
        ref_img = load_ref_png(ref_path)

        sd_ref = decode_sd(ref_img, args.format)
        sd_our = decode_sd(our_img, args.format)
        max_diff, mean_diff, mismatch = diff_sd(sd_ref, sd_our, args.range)

        max_diff_all = max(max_diff_all, max_diff)
        mean_sum += mean_diff
        mean_count += 1
        mismatch_sum += mismatch

        print(f"{name}: max {max_diff:.6f} mean {mean_diff:.6f} mismatches {mismatch}")

    mean_all = mean_sum / mean_count if mean_count else 0.0
    print(f"overall: max {max_diff_all:.6f} mean {mean_all:.6f} mismatches {mismatch_sum}")

    if args.fail_max is not None and max_diff_all > args.fail_max:
        return 1
    if args.fail_mean is not None and mean_all > args.fail_mean:
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
