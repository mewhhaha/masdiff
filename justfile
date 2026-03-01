sdl3:
    cd examples/sdl3-spirdo-text && if [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.cache" ] && [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.tar" ]; then CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal update; fi && CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" SDL_AUDIODRIVER="${SDL_AUDIODRIVER:-dummy}" SDL_VIDEODRIVER="${SDL_VIDEODRIVER:-x11}" MASDIFF_SDL_GEN_BACKEND="${MASDIFF_SDL_GEN_BACKEND:-gpu}" MASDIFF_SDL_GEN_STRICT=1 MASDIFF_SDL_GEN_SHADER=flat MASDIFF_SDL_FAST_PATH="${MASDIFF_SDL_FAST_PATH:-0}" MASDIFF_SDL_GPU_BATCH="${MASDIFF_SDL_GPU_BATCH:-1}" MASDIFF_SDL_FONT="${MASDIFF_SDL_FONT:-roboto-flex}" MASDIFF_SDL_OVLP="${MASDIFF_SDL_OVLP:-1}" MASDIFF_SDL_PRESENT_HEAL="${MASDIFF_SDL_PRESENT_HEAL:-1}" MASDIFF_SDL_PXRANGE="${MASDIFF_SDL_PXRANGE:-7}" MASDIFF_SDL_UV_INSET="${MASDIFF_SDL_UV_INSET:-0.25}" MASDIFF_SDL_DEBUG="${MASDIFF_SDL_DEBUG:-0}" cabal run sdl3-spirdo-text

sdl3-fast:
    MASDIFF_SDL_FAST_PATH=1 just sdl3

sdl3-batch:
    cd examples/sdl3-spirdo-text && if [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.cache" ] && [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.tar" ]; then CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal update; fi && CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" SDL_AUDIODRIVER="${SDL_AUDIODRIVER:-dummy}" SDL_VIDEODRIVER="${SDL_VIDEODRIVER:-x11}" MASDIFF_SDL_GEN_BACKEND="${MASDIFF_SDL_GEN_BACKEND:-gpu}" MASDIFF_SDL_GPU_BATCH=1 MASDIFF_SDL_FONT="${MASDIFF_SDL_FONT:-roboto-flex}" MASDIFF_SDL_OVLP="${MASDIFF_SDL_OVLP:-1}" MASDIFF_SDL_PRESENT_HEAL="${MASDIFF_SDL_PRESENT_HEAL:-1}" MASDIFF_SDL_PXRANGE="${MASDIFF_SDL_PXRANGE:-7}" MASDIFF_SDL_UV_INSET="${MASDIFF_SDL_UV_INSET:-0.25}" MASDIFF_SDL_DEBUG="${MASDIFF_SDL_DEBUG:-0}" cabal run sdl3-spirdo-text

sdl3-probe:
    cd examples/sdl3-spirdo-text && if [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.cache" ] && [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.tar" ]; then CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal update; fi && CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" SDL_AUDIODRIVER="${SDL_AUDIODRIVER:-dummy}" SDL_VIDEODRIVER="${SDL_VIDEODRIVER:-x11}" MASDIFF_SDL_GEN_BACKEND="${MASDIFF_SDL_GEN_BACKEND:-gpu}" MASDIFF_SDL_GPU_BATCH=0 MASDIFF_SDL_FONT="${MASDIFF_SDL_FONT:-roboto-flex}" MASDIFF_SDL_OVLP="${MASDIFF_SDL_OVLP:-1}" MASDIFF_SDL_PRESENT_HEAL="${MASDIFF_SDL_PRESENT_HEAL:-1}" MASDIFF_SDL_PXRANGE="${MASDIFF_SDL_PXRANGE:-7}" MASDIFF_SDL_UV_INSET="${MASDIFF_SDL_UV_INSET:-0.25}" MASDIFF_SDL_DEBUG="${MASDIFF_SDL_DEBUG:-1}" MASDIFF_SDL_PIPELINE_PROBE=1 MASDIFF_SDL_GEN_SHADER="${MASDIFF_SDL_GEN_SHADER:-flat}" cabal run sdl3-spirdo-text

sdl3-harness:
    ./tools/run_sdl3_artifact_harness.sh

sdl3-ab:
    ./tools/sdl3_ab_compare.sh

oracle-pr:
    CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run masdiff-parity -- --profile pr --oracle process --require-exact

oracle-nightly:
    CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run masdiff-parity -- --profile nightly --oracle process --json-out out/oracle/nightly-process.json
    ./tools/run_msdfgl_oracle_gate.sh

oracle-msdfgl:
    ./tools/run_msdfgl_oracle_gate.sh
