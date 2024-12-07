#!/usr/bin/env sh
inotifywait -m -e close_write --format '%w%f' *.sql | while read file; do clear; rm -f aoc.db; duckdb aoc.db < $file; done
