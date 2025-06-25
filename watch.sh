#!/usr/bin/env bash

inotifywait -e close_write,moved_to,create -m . |
while read -r directory events filename; do
  if [ "$filename" = "test.yal" ]; then
    cmake --build build -t yalc && ./build/cmd/yalc --verbose --verbose-parser test.yal --file | wl-copy
  fi
done
