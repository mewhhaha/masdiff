# masdiff

Pure Haskell MSDF generator for TrueType fonts. It parses `glyf/loca/cmap` outlines and produces per‑glyph MSDF bitmaps plus metrics and kerning (legacy `kern` and GPOS pair adjustments).

## Features

- Pure Haskell pipeline (no external CLI/tools).
- TrueType outline parsing (simple + composite glyphs).
- MSDF raster generation per glyph (packed RGB bytes).
- Kerning support via `kern` and GPOS pair adjustment (lookup type 2).

## Usage

```haskell
import MSDF.Generated (generateMSDF)

main :: IO ()
main = do
  atlas <- generateMSDF "path/to/font.ttf"
  print (msdfFontName atlas)
```

### Custom config

```haskell
import MSDF.Generated (generateMSDFWithConfig)
import MSDF.MSDF (defaultMSDFConfig, GlyphSet(..))

main :: IO ()
main = do
  let cfg = defaultMSDFConfig { cfgPixelSize = 24, cfgGlyphSet = GlyphSetCodepoints [65,66,67] }
  atlas <- generateMSDFWithConfig cfg "path/to/font.ttf"
  print (length (msdfGlyphs atlas))
```

## Tests

```sh
cabal test msdf-tests
```

## Limitations

- TrueType outlines only (no CFF).
- Variable fonts use default axis values.
- GPOS support is limited to Pair Adjustment lookups (format 1/2).

## Assets and licensing

The repository includes the Inter font for test coverage under its own license (OFL). See `assets/Inter/OFL.txt`.

## License

MIT. See `LICENSE`.
