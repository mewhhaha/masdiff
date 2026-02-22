sdl3:
    cd examples/sdl3-spirdo-text && if [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.cache" ] && [ ! -f "$PWD/.cabal/packages/hackage.haskell.org/01-index.tar" ]; then CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal update; fi && CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" SDL_AUDIODRIVER="${SDL_AUDIODRIVER:-dummy}" cabal run sdl3-spirdo-text

sdl3-harness:
    ./tools/run_sdl3_artifact_harness.sh
