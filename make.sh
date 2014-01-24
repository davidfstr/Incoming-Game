ELM_PATH=`which elm`
ELM_BIN_PATH=`dirname "$ELM_PATH"`
ELM_RUNTIME_PATH="$ELM_BIN_PATH/../ghc-*/lib/Elm-*/share/elm-runtime.js"

elm -r elm-runtime.js Main.elm
cp -r assets build/assets
cp $ELM_RUNTIME_PATH build/elm-runtime.js
