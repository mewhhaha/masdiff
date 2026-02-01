#!/usr/bin/env python3
"""Compare MSDF/MTSDF images against a reference (e.g., msdfgen output).

Usage examples:
  python tools/msdfgen_diff.py --ref ref.png --test test.png
  python tools/msdfgen_diff.py --ref ref.png --test atlas.rgba --test-raw --width 256 --height 256 --channels 4 --test-crop 64,64,128,128
"""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from typing import Optional, Tuple

from PIL import Image


@dataclass
class ImageSpec:
    path: str
    raw: bool
    width: Optional[int]
    height: Optional[int]
    channels: Optional[int]
    crop: Optional[Tuple[int, int, int, int]]


def parse_crop(value: str) -> Tuple[int, int, int, int]:
    parts = value.split(",")
    if len(parts) != 4:
        raise argparse.ArgumentTypeError("crop must be x,y,w,h")
    return tuple(int(p.strip()) for p in parts)  # type: ignore[return-value]


def load_image(spec: ImageSpec) -> Image.Image:
    if spec.raw:
        if spec.width is None or spec.height is None or spec.channels is None:
            raise ValueError("raw input requires --width/--height/--channels")
        mode = "RGBA" if spec.channels == 4 else "RGB"
        with open(spec.path, "rb") as f:
            data = f.read()
        expected = spec.width * spec.height * spec.channels
        if len(data) != expected:
            raise ValueError(f"raw size mismatch: got {len(data)} bytes, expected {expected}")
        img = Image.frombytes(mode, (spec.width, spec.height), data)
    else:
        img = Image.open(spec.path).convert("RGBA")

    if spec.crop is not None:
        x, y, w, h = spec.crop
        img = img.crop((x, y, x + w, y + h))

    return img


def compare_images(ref: Image.Image, test: Image.Image, diff_out: Optional[str], threshold: int) -> int:
    if ref.size != test.size:
        raise ValueError(f"size mismatch: ref {ref.size} vs test {test.size}")

    ref_px = ref.load()
    test_px = test.load()
    width, height = ref.size

    max_diff = 0
    total_diff = 0
    count = width * height * 4
    over = 0

    diff_img = Image.new("RGBA", (width, height)) if diff_out else None
    diff_px = diff_img.load() if diff_img else None

    for y in range(height):
        for x in range(width):
            r0, g0, b0, a0 = ref_px[x, y]
            r1, g1, b1, a1 = test_px[x, y]
            dr = abs(r0 - r1)
            dg = abs(g0 - g1)
            db = abs(b0 - b1)
            da = abs(a0 - a1)
            local_max = max(dr, dg, db, da)
            max_diff = max(max_diff, local_max)
            total_diff += dr + dg + db + da
            if local_max > threshold:
                over += 1
            if diff_px is not None:
                diff_px[x, y] = (dr, dg, db, 255)

    mean_diff = total_diff / count if count else 0
    print(f"max diff: {max_diff}")
    print(f"mean diff: {mean_diff:.4f}")
    print(f"pixels over threshold ({threshold}): {over}")

    if diff_img is not None and diff_out:
        diff_img.save(diff_out)
        print(f"wrote diff: {diff_out}")

    return 1 if over > 0 else 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Compare MSDF/MTSDF images to a reference.")
    parser.add_argument("--ref", required=True, help="reference image path (PNG or raw)")
    parser.add_argument("--test", required=True, help="test image path (PNG or raw)")
    parser.add_argument("--ref-raw", action="store_true", help="reference is raw RGB/RGBA")
    parser.add_argument("--test-raw", action="store_true", help="test is raw RGB/RGBA")
    parser.add_argument("--width", type=int, help="raw width (for raw inputs)")
    parser.add_argument("--height", type=int, help="raw height (for raw inputs)")
    parser.add_argument("--channels", type=int, choices=[3, 4], help="raw channels (3 or 4)")
    parser.add_argument("--ref-crop", type=parse_crop, help="crop reference: x,y,w,h")
    parser.add_argument("--test-crop", type=parse_crop, help="crop test: x,y,w,h")
    parser.add_argument("--diff-out", help="write diff image to path")
    parser.add_argument("--threshold", type=int, default=1, help="per-channel diff threshold")
    args = parser.parse_args()

    ref_spec = ImageSpec(
        path=args.ref,
        raw=args.ref_raw,
        width=args.width,
        height=args.height,
        channels=args.channels,
        crop=args.ref_crop,
    )
    test_spec = ImageSpec(
        path=args.test,
        raw=args.test_raw,
        width=args.width,
        height=args.height,
        channels=args.channels,
        crop=args.test_crop,
    )

    ref_img = load_image(ref_spec)
    test_img = load_image(test_spec)
    return compare_images(ref_img, test_img, args.diff_out, args.threshold)


if __name__ == "__main__":
    sys.exit(main())
