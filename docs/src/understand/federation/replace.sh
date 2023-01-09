#!/usr/bin/env sh

set -x

for f in *.rst; do
    if [ "$f" = "index.rst" ]; then
       continue;
    fi
    pandoc -f rst -t commonmark_x < "$f" > "${f%.rst}.md"
    rm "$f"
done
