sdl3:
    cd examples/sdl3-spirdo-text && if [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.cache" ] && [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.tar" ]; then CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal update; fi && CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" SDL_AUDIODRIVER="${SDL_AUDIODRIVER:-dummy}" MASDIFF_SDL_GEN_BACKEND="${MASDIFF_SDL_GEN_BACKEND:-gpu}" MASDIFF_SDL_DEBUG="${MASDIFF_SDL_DEBUG:-0}" cabal run sdl3-spirdo-text

sdl3-probe:
    cd examples/sdl3-spirdo-text && if [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.cache" ] && [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.tar" ]; then CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal update; fi && CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" SDL_AUDIODRIVER="${SDL_AUDIODRIVER:-dummy}" MASDIFF_SDL_GEN_BACKEND="${MASDIFF_SDL_GEN_BACKEND:-gpu}" MASDIFF_SDL_DEBUG="${MASDIFF_SDL_DEBUG:-1}" MASDIFF_SDL_PIPELINE_PROBE=1 MASDIFF_SDL_GEN_SHADER="${MASDIFF_SDL_GEN_SHADER:-flat}" cabal run sdl3-spirdo-text

sdl3-harness:
    ./tools/run_sdl3_artifact_harness.sh
