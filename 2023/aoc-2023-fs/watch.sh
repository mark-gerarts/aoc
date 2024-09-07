#!/usr/bin/env sh
inotifywait -m -e close_write --format '%w%f' *.fsx | while read file; do dotnet fsi $file; done
