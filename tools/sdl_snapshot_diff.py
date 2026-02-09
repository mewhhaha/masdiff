#!/usr/bin/env python3
import argparse
import struct
import sys
import zlib
from pathlib import Path


def read_tga(path: Path):
    data = path.read_bytes()
    if len(data) < 18:
        raise SystemExit("tga: header too short")
    id_len = data[0]
    color_map_type = data[1]
    image_type = data[2]
    if color_map_type != 0:
        raise SystemExit("tga: color maps not supported")
    if image_type != 2:
        raise SystemExit(f"tga: unsupported image type {image_type}")

    width = data[12] | (data[13] << 8)
    height = data[14] | (data[15] << 8)
    bpp = data[16]
    desc = data[17]
    pixel_bytes = bpp // 8
    if pixel_bytes not in (3, 4):
        raise SystemExit(f"tga: unsupported bpp {bpp}")

    offset = 18 + id_len
    expected = width * height * pixel_bytes
    if len(data) < offset + expected:
        raise SystemExit("tga: unexpected data length")

    img = data[offset:offset + expected]
    top_left = (desc & 0x20) != 0
    out = bytearray(width * height * 4)

    for y in range(height):
        src_y = y if top_left else (height - 1 - y)
        src_row = src_y * width * pixel_bytes
        dst_row = y * width * 4
        for x in range(width):
            i = src_row + x * pixel_bytes
            b = img[i]
            g = img[i + 1]
            r = img[i + 2]
            a = img[i + 3] if pixel_bytes == 4 else 255
            j = dst_row + x * 4
            out[j:j + 4] = bytes((r, g, b, a))

    return width, height, bytes(out)


def write_png(path: Path, width: int, height: int, rgba: bytes):
    def chunk(tag: bytes, payload: bytes) -> bytes:
        length = struct.pack(">I", len(payload))
        crc = zlib.crc32(tag + payload) & 0xFFFFFFFF
        return length + tag + payload + struct.pack(">I", crc)

    stride = width * 4
    raw = bytearray()
    for y in range(height):
        raw.append(0)
        start = y * stride
        raw.extend(rgba[start:start + stride])

    sig = b"\x89PNG\r\n\x1a\n"
    ihdr = struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0)
    comp = zlib.compress(bytes(raw), 9)
    out = sig + chunk(b"IHDR", ihdr) + chunk(b"IDAT", comp) + chunk(b"IEND", b"")
    path.write_bytes(out)


def compare(ref_rgba: bytes, test_rgba: bytes, width: int, height: int, threshold: int, diff_out: Path | None):
    if len(ref_rgba) != len(test_rgba):
        raise SystemExit("rgba length mismatch")
    total = 0
    max_diff = 0
    mismatch = 0
    diff = bytearray(len(ref_rgba))
    for i, (a, b) in enumerate(zip(ref_rgba, test_rgba)):
        d = abs(a - b)
        diff[i] = d
        total += d
        if d > max_diff:
            max_diff = d
        if d > threshold:
            mismatch += 1
    count = len(ref_rgba)
    mean = (total / count) if count else 0.0
    print(f"max diff: {max_diff}")
    print(f"mean diff: {mean:.4f}")
    print(f"mismatch bytes (> {threshold}): {mismatch}")
    if diff_out is not None:
        write_png(diff_out, width, height, bytes(diff))
        print(f"wrote diff: {diff_out}")
    if mismatch > 0:
        raise SystemExit(1)


def main() -> int:
    parser = argparse.ArgumentParser(description="Compare SDL TGA screenshots against a reference snapshot.")
    parser.add_argument("--ref", required=True, help="reference .tga path")
    parser.add_argument("--test", required=True, help="test .tga path")
    parser.add_argument("--diff-out", help="write diff png to path")
    parser.add_argument("--threshold", type=int, default=1, help="per-channel diff threshold")
    args = parser.parse_args()

    ref_path = Path(args.ref)
    test_path = Path(args.test)
    if not ref_path.exists():
        raise SystemExit(f"missing ref: {ref_path}")
    if not test_path.exists():
        raise SystemExit(f"missing test: {test_path}")

    rw, rh, ref_rgba = read_tga(ref_path)
    tw, th, test_rgba = read_tga(test_path)
    if rw != tw or rh != th:
        raise SystemExit(f"size mismatch: ref={rw}x{rh} test={tw}x{th}")

    diff_out = Path(args.diff_out) if args.diff_out else None
    compare(ref_rgba, test_rgba, rw, rh, args.threshold, diff_out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
