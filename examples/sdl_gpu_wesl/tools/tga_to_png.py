#!/usr/bin/env python3
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
        raw.append(0)  # filter type 0
        start = y * stride
        raw.extend(rgba[start:start + stride])

    sig = b"\x89PNG\r\n\x1a\n"
    ihdr = struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0)
    comp = zlib.compress(bytes(raw), 9)
    out = sig + chunk(b"IHDR", ihdr) + chunk(b"IDAT", comp) + chunk(b"IEND", b"")
    path.write_bytes(out)


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: tga_to_png.py input.tga [output.png]")
        return 2
    in_path = Path(sys.argv[1])
    out_path = Path(sys.argv[2]) if len(sys.argv) > 2 else in_path.with_suffix(".png")
    width, height, rgba = read_tga(in_path)
    write_png(out_path, width, height, rgba)
    print(f"wrote {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
