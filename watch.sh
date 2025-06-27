#!/usr/bin/env bash

inotifywait -e close_write,moved_to,create -m . |
while read -r directory events filename; do
  if [ "$filename" = "test.yal" ]; then
    cmake --build build -t yalc && ./build/cmd/yalc --verbose exe,sorted --dump sorted --file test.yal | wl-copy
  fi
done

# inotifywait -q -m -e close_write test.yal |
# while read -r filename event; do
#     cmake --build build -t yalc && ./build/cmd/yalc --verbose --verbose-parser --verbose-name-res test.yal --file | wl-copy
# done
