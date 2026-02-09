# SDL render snapshots

These snapshots are captured from the SDL_gpu demo in headless mode and stored
as raw `.tga` screenshots.

Update snapshot (requires a working SDL_GPU backend; a window will open briefly):

```
MSDF_UPDATE_SDL_SNAPSHOT=1 MSDF_RUN_SDL_SNAPSHOT=1 cabal test msdf-tests
```

Or run the helper directly:

```
tools/sdl_render_snapshot.sh update
```

Compare snapshot:

```
MSDF_RUN_SDL_SNAPSHOT=1 cabal test msdf-tests
```

The snapshot helper now also runs a seam guard on the live SDL screenshot and
fails if it detects suspicious tiny enclosed holes (common seam artifact shape).
To bypass this intentionally (for debugging only), set `MSDF_ALLOW_SEAM=1`.

If SDL_GPU is unavailable, the snapshot check is skipped with a warning.
To force headless mode (may fail without a backend), set `MSDF_SDL_HEADLESS=1`.

On mismatch, the current screenshot and diff image are written to:

- `out/snapshots/render/demo_live_text.tga`
- `out/snapshots/render/demo_live_text.diff.png`
- `out/snapshots/render/demo_live_text.seam.png`
