#!/usr/bin/env sh

# Useful for development: open a terminal and run `./watch.sh` to print the
# output of a file when it is saved (which is continuously for my IDE).
inotifywait -m -e close_write --format '%w%f' *.fsx | while read file; do echo "==="; dotnet fsi $file; done
