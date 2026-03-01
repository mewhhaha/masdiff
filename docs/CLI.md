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

Compares native output against selected oracle sources with profile-based parity gates.

```bash
cabal run masdiff-parity -- [options]
```

Options:

- `--max-cases <n>`
- `--profile pr|nightly|full`
- `--oracle process|msdfgl|both`
- `--manifest <path>`
- `--json-out <path>`
- `--allow-missing-oracle`
- `--require-oracle`
- `--require-exact`
- `--verbose`

Examples:

```bash
cabal run masdiff-parity -- --profile pr --manifest out/reference/inter-mtsdf/manifest.tsv --verbose
cabal run masdiff-parity -- --profile nightly --oracle both --manifest out/reference/inter-mtsdf/manifest.tsv --json-out out/parity-nightly.json --allow-missing-oracle --verbose
```

Notes:

- `--profile pr` is the strict default for the stable corpus and keeps exact parity requirements.
- `--profile nightly` and `--profile full` expand coverage and use shape/metrics thresholds for broader cases.
- `--oracle process` uses `msdfgen` via `MSDFGEN_BIN`/`PATH`; `--oracle msdfgl` reads oracle artifacts from `--manifest`; `--oracle both` enables both sources.
- When process var-axis support is unavailable, `masdiff-parity` instantiates variable fonts through `python3` + `fontTools.varLib.instancer` and compares native/process using that shared static instance.
- `--require-exact` remains available for explicit full-pixel exact checks.
- Use `--manifest` for oracle input when using `--oracle msdfgl` or `--oracle both`.
- `--allow-missing-oracle` permits sparse oracle coverage; `--require-oracle` enforces complete oracle availability.
- `just oracle-msdfgl` can override its manifest with `MASDIFF_MSDFGL_MANIFEST` (default: `out/reference/msdfgl-mtsdf/manifest.tsv`).

Parity contract:

- `strict` gate requires:
  - `MSDF.Compare.strictGate` image diff pass (`maxCh/p99/mean`),
  - `metrics_max_delta <= 1.0e-6`.
- `coverage` gate requires:
  - `shape_diff_ratio <= 0.002`,
  - `metrics_max_delta <= 2.0e-2`,
  - `alpha_median_delta <= 0.005`.
- `pr` profile: all rows are `strict`.
- `nightly` / `full` profiles:
  - `strict` only on stable static oracle subset (`interOracleFontCases`) at `dim=64`, `pxrange=8.0`,
  - all other rows use `coverage`.
- If process oracle var-axis support is unavailable, variable rows are instantiated to static TTF and that shared instantiated source is used for both native/process comparisons (no variable-row skip path).

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
