# CLI Reference

This file documents all shipped executables in `masdiff`.

## Common Environment Variables

- `MASDIFF_BACKEND`: `native`, `process`, or `oracle` (where supported). Default: `native`.
- `MSDFGEN_BIN`: path/binary name for `msdfgen` when process backend is used. Default: `msdfgen`.

## `generate-inter-mtsdf-fixtures`

Generates deterministic Inter fixture PNGs and a TSV manifest.

```bash
cabal run generate-inter-mtsdf-fixtures
```

Environment:

- `MASDIFF_BACKEND` (default `native`; use `process` for oracle comparison)
- `MSDFGEN_BIN` (used when backend is `process`)
- `MTSDF_OUT` (default `out/reference/inter-mtsdf`)
- `MTSDF_DIM` (default `64`)
- `MTSDF_PXRANGE` (default `8.0`)
- `MTSDF_CLEAN` (`true/false`, default `false`)

Outputs:

- `manifest.tsv` with header metadata and one row per glyph fixture.
- PNGs under `<MTSDF_OUT>/<font-case>/U+XXXX.png`.

## `masdiff`

Single-glyph generator with msdfgen-like subset CLI.

```bash
cabal run masdiff -- mtsdf -font <font.ttf> <char|code> [options]
cabal run masdiff -- mtsdf -varfont <font.ttf?axis=val&...> <char|code> [options]
```

Options:

- `-dimensions <w> <h>` (currently requires `w == h`)
- `-pxrange <range>`
- `-seed <n>`
- `-autoframe`
- `-overlapfix`
- `-o <output>`
- `-format <png|rgba>`
- `-printmetrics`

Examples:

```bash
cabal run masdiff -- \
  mtsdf -font assets/Inter/static/Inter_24pt-Regular.ttf A \
  -dimensions 64 64 -pxrange 8.0 -o out/A.png -format png -printmetrics
```

## `masdiff-validate`

Loads a TSV manifest and validates generated output against manifest PNGs using strict diff gates.

```bash
cabal run masdiff-validate -- [options]
```

Options:

- `--manifest <path>` (or `-m`, default `out/reference/inter-mtsdf/manifest.tsv`)
- `--max-cases <n>`
- `--verbose`

## `masdiff-parity`

Compares native backend vs process backend across the built-in Inter oracle subset.

```bash
cabal run masdiff-parity -- [options]
```

Options:

- `--max-cases <n>`
- `--require-exact`
- `--verbose`

Notes:

- This command exercises both backends internally and expects `msdfgen` to be available on `PATH` for process runs.
- Oracle parity is intentionally limited to static fonts and the baseline variable setting (`wght=400`, `opsz=14`) because some `msdfgen` builds ignore variable-axis changes.

## `masdiff-text-render`

Renders a text line by generating glyph MTSDF, shading to grayscale, and compositing.

```bash
cabal run masdiff-text-render -- \
  --text <text> (-font <font.ttf> | -varfont <font.ttf?axis=val&...>) [options]
```

Options:

- `-o <output.png>` default `final.png`
- `--dim <n>` output glyph size, default `32`
- `--gen-dim <n>` generation size, default `96`
- `--pxrange <x>` default `6.0`
- `--screen-px-range <x>` default auto
- `--gap <n>` default `2`
- `--space <n>` default `12`
- `--border <n>` default `20`
- `--seed <n>` default `1`
- `--fallback-threshold <x>` default `0.0`
- `--alpha-fallback` (default on)
- `--no-alpha-fallback`
- `--no-overlap-fix` (default mode)
- `--overlap-fix`
- `--verbose`

## `masdiff-atlas`

Builds one or more atlas pages from a text string and writes atlas metadata TSV.

```bash
cabal run masdiff-atlas -- \
  --text "PACK MY BOX" \
  -font assets/Inter/static/Inter_24pt-Regular.ttf \
  --out-prefix out/atlas/inter24 \
  --gen-dim 64 \
  --pxrange 8.0 \
  --atlas-w 1024 \
  --atlas-h 1024 \
  --padding 1 \
  --jobs 8 \
  --verbose
```

Options:

- `--text <text>` (required)
- `-font <font.ttf>` or `-varfont <font.ttf?axis=val&...>` (required)
- `--out-prefix <path>` default `out/atlas/atlas`
- `--gen-dim <n>` default `64`
- `--pxrange <x>` default `8.0`
- `--seed <n>` default `1`
- `--atlas-w <n>` default `1024`
- `--atlas-h <n>` default `1024`
- `--padding <n>` default `1`
- `--jobs <n>` default `1`
- `--overlap-fix` / `--no-overlap-fix`
- `--verbose`

Outputs:

- `<out-prefix>.page-000.png`, `<out-prefix>.page-001.png`, ...
- `<out-prefix>.tsv` (glyph -> page/rect/metrics table)
