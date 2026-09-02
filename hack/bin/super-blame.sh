#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF
Usage: $0 [-h] <file> [file ...]

List commits that changed the given file(s) in chronological order,
annotated with the release in which each commit landed on master.

A commit's release is the oldest chart/X.Y.0 tag on master whose
tagged commit is not older than the commit itself.

Options:
  -h  Show this help
EOF
  exit 0
}

if [ $# -eq 0 ]; then
  echo "Usage: $0 [-h] <file> [file ...]" >&2
  exit 1
fi

if [ "$1" = "-h" ]; then
  usage
fi

releases_tmp=$(mktemp)
trap 'rm -f "$releases_tmp"' EXIT

while read -r tag; do
  commit=$(git rev-list -1 "$tag")
  ts=$(git log -1 --format="%ct" "$commit")
  echo "${ts} ${tag}"
done < <(git tag --merged master 'chart/*.0' | sort -V) > "$releases_tmp"

for file in "$@"; do
  if [ ! -f "$file" ]; then
    echo "Error: file not found: $file" >&2
    exit 1
  fi

  echo "=== $file ==="

  git log --follow --format="%H %ct %ai %s" -- "$file" | while read -r commit commit_ts rest; do
    release=""
    while read -r ts tag; do
      if [ "$ts" -ge "$commit_ts" ]; then
        release="$tag"
        break
      fi
    done < <(sort -n "$releases_tmp")

    if [ -n "$release" ]; then
      echo "$commit $rest [$release]"
    else
      echo "$commit $rest [unreleased]"
    fi
  done

  echo
done
