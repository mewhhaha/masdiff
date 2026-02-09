# MTSDF snapshots

This folder holds raw RGBA snapshots used by `msdf-tests`.

Update snapshots:

```
MSDF_UPDATE_SNAPSHOTS=1 cabal test msdf-tests
```

On mismatch, the current render is written to `out/snapshots/...` alongside a
`.diff.rgba` file containing per-channel absolute differences.

Notes:
- Files are raw RGBA (width x height x 4). Convert to PNG using your preferred tool.
- Snapshot cases are defined in `tests/MSDFTests.hs`.
