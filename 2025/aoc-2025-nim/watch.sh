#!/usr/bin/env sh
# Compiles files on save, killing previous compilation processes if any are
# still running.

LAST_PID=""

cleanup() {
  if [ -n "$LAST_PID" ] && kill -0 "$LAST_PID" 2>/dev/null; then
    kill "$LAST_PID"
  fi
  exit 0
}
trap cleanup INT TERM

inotifywait -m -e close_write --format '%w%f' *.nim | while read -r file; do
  echo "==="

  if [ -n "$LAST_PID" ] && kill -0 "$LAST_PID" 2>/dev/null; then
    echo "Stopping previous compilation (PID: $LAST_PID)..."
    kill "$LAST_PID" 2>/dev/null
    wait "$LAST_PID" 2>/dev/null
  fi

  nim c -r "$file" &

  LAST_PID=$!
done
