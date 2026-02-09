#!/usr/bin/env python3
import argparse
import struct
import zlib
from collections import deque
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

    img = data[offset : offset + expected]
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
            out[j : j + 4] = bytes((r, g, b, a))

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
        raw.extend(rgba[start : start + stride])

    sig = b"\x89PNG\r\n\x1a\n"
    ihdr = struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0)
    comp = zlib.compress(bytes(raw), 9)
    out = sig + chunk(b"IHDR", ihdr) + chunk(b"IDAT", comp) + chunk(b"IEND", b"")
    path.write_bytes(out)


def connected_components(mask: bytearray, width: int, height: int):
    n = width * height
    seen = bytearray(n)
    comps = []
    for i in range(n):
        if mask[i] == 0 or seen[i]:
            continue
        q = deque([i])
        seen[i] = 1
        pixels = []
        x0 = width
        y0 = height
        x1 = -1
        y1 = -1
        while q:
            p = q.popleft()
            pixels.append(p)
            y = p // width
            x = p - y * width
            if x < x0:
                x0 = x
            if y < y0:
                y0 = y
            if x > x1:
                x1 = x
            if y > y1:
                y1 = y
            if x > 0:
                nidx = p - 1
                if mask[nidx] and not seen[nidx]:
                    seen[nidx] = 1
                    q.append(nidx)
            if x + 1 < width:
                nidx = p + 1
                if mask[nidx] and not seen[nidx]:
                    seen[nidx] = 1
                    q.append(nidx)
            if y > 0:
                nidx = p - width
                if mask[nidx] and not seen[nidx]:
                    seen[nidx] = 1
                    q.append(nidx)
            if y + 1 < height:
                nidx = p + width
                if mask[nidx] and not seen[nidx]:
                    seen[nidx] = 1
                    q.append(nidx)
        comps.append((pixels, x0, y0, x1, y1))
    return comps


def find_holes_in_component(
    pixels: list[int],
    x0: int,
    y0: int,
    x1: int,
    y1: int,
    width: int,
    min_hole_area: int,
    max_hole_area: int,
):
    rw = x1 - x0 + 1
    rh = y1 - y0 + 1
    rn = rw * rh
    region_fg = bytearray(rn)
    for p in pixels:
        gy = p // width
        gx = p - gy * width
        lx = gx - x0
        ly = gy - y0
        region_fg[ly * rw + lx] = 1

    outside = bytearray(rn)
    q: deque[int] = deque()

    for x in range(rw):
        i0 = x
        i1 = (rh - 1) * rw + x
        if region_fg[i0] == 0 and outside[i0] == 0:
            outside[i0] = 1
            q.append(i0)
        if region_fg[i1] == 0 and outside[i1] == 0:
            outside[i1] = 1
            q.append(i1)
    for y in range(rh):
        i0 = y * rw
        i1 = y * rw + (rw - 1)
        if region_fg[i0] == 0 and outside[i0] == 0:
            outside[i0] = 1
            q.append(i0)
        if region_fg[i1] == 0 and outside[i1] == 0:
            outside[i1] = 1
            q.append(i1)

    while q:
        i = q.popleft()
        y = i // rw
        x = i - y * rw
        if x > 0:
            j = i - 1
            if region_fg[j] == 0 and outside[j] == 0:
                outside[j] = 1
                q.append(j)
        if x + 1 < rw:
            j = i + 1
            if region_fg[j] == 0 and outside[j] == 0:
                outside[j] = 1
                q.append(j)
        if y > 0:
            j = i - rw
            if region_fg[j] == 0 and outside[j] == 0:
                outside[j] = 1
                q.append(j)
        if y + 1 < rh:
            j = i + rw
            if region_fg[j] == 0 and outside[j] == 0:
                outside[j] = 1
                q.append(j)

    hole_mask = bytearray(rn)
    for i in range(rn):
        if region_fg[i] == 0 and outside[i] == 0:
            hole_mask[i] = 1

    holes = []
    for hole_pixels, hx0, hy0, hx1, hy1 in connected_components(hole_mask, rw, rh):
        area = len(hole_pixels)
        if min_hole_area <= area <= max_hole_area:
            global_pixels = []
            for li in hole_pixels:
                ly = li // rw
                lx = li - ly * rw
                gx = x0 + lx
                gy = y0 + ly
                global_pixels.append((gx, gy))
            holes.append((area, hx0 + x0, hy0 + y0, hx1 + x0, hy1 + y0, global_pixels))
    return holes


def main() -> int:
    parser = argparse.ArgumentParser(description="Detect small enclosed seam holes in SDL text snapshot.")
    parser.add_argument("--image", required=True, help="input screenshot (.tga)")
    parser.add_argument("--luma-threshold", type=int, default=220, help="foreground luma threshold")
    parser.add_argument("--alpha-threshold", type=int, default=220, help="foreground alpha threshold")
    parser.add_argument("--min-component-area", type=int, default=100, help="ignore tiny fg components")
    parser.add_argument("--min-hole-area", type=int, default=6, help="minimum enclosed-hole area to flag")
    parser.add_argument("--max-hole-area", type=int, default=400, help="maximum enclosed-hole area to flag")
    parser.add_argument("--debug-out", help="optional PNG mask output")
    args = parser.parse_args()

    image_path = Path(args.image)
    if not image_path.exists():
        raise SystemExit(f"missing image: {image_path}")

    width, height, rgba = read_tga(image_path)
    n = width * height
    fg = bytearray(n)
    luma = bytearray(n)
    for i in range(n):
        j = i * 4
        r = rgba[j]
        g = rgba[j + 1]
        b = rgba[j + 2]
        a = rgba[j + 3]
        yv = (77 * r + 150 * g + 29 * b) >> 8
        luma[i] = yv
        if yv >= args.luma_threshold and a >= args.alpha_threshold:
            fg[i] = 1

    comps = connected_components(fg, width, height)
    comps = [c for c in comps if len(c[0]) >= args.min_component_area]
    if not comps:
        print("seam-guard: no foreground components found")
        return 1

    suspicious = []
    suspicious_pixels = set()
    for pixels, x0, y0, x1, y1 in comps:
        holes = find_holes_in_component(
            pixels,
            x0,
            y0,
            x1,
            y1,
            width,
            args.min_hole_area,
            args.max_hole_area,
        )
        for area, hx0, hy0, hx1, hy1, hpx in holes:
            suspicious.append((area, hx0, hy0, hx1, hy1))
            for p in hpx:
                suspicious_pixels.add(p)

    if args.debug_out:
        out = bytearray(n * 4)
        for i in range(n):
            yv = luma[i]
            j = i * 4
            out[j] = yv
            out[j + 1] = yv
            out[j + 2] = yv
            out[j + 3] = 255
        for gx, gy in suspicious_pixels:
            i = gy * width + gx
            j = i * 4
            out[j] = 255
            out[j + 1] = 32
            out[j + 2] = 32
            out[j + 3] = 255
        write_png(Path(args.debug_out), width, height, bytes(out))
        print(f"seam-guard: wrote debug overlay: {args.debug_out}")

    if suspicious:
        print(f"seam-guard: suspicious enclosed holes: {len(suspicious)}")
        for area, x0, y0, x1, y1 in suspicious[:20]:
            print(f"  area={area} bbox=({x0},{y0})-({x1},{y1})")
        if len(suspicious) > 20:
            print(f"  ... and {len(suspicious) - 20} more")
        return 1

    print("seam-guard: ok (no suspicious enclosed holes)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
