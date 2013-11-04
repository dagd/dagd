#!/usr/bin/env bash
runhaskell ./Web/Dagd/Main.hs &

while true; do
  while inotifywait -e modify ./Web/Dagd/*.hs; do
    kill `pgrep -f 'prog "./Web/Dagd/Main.hs"'`
    echo "Killed, maybe"
    runhaskell ./Web/Dagd/Main.hs &
  done
done
