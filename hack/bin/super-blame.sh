#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF
Usage: $0 [-h] <file>[@<lines>] [file ...]

List commits that changed the given file(s) in chronological order,
annotated with the release in which each commit landed on master.

A commit's release is the oldest chart/X.Y.0 tag on master whose
tagged commit is not older than the commit itself.

Line ranges limit output to commits that changed those lines.
Syntax: file.hs@1,15-18  (single lines and ranges, comma-separated)
If no line ranges are given, all commits for the file are shown.

Options:
  -h  Show this help
EOF
  exit 0
}

if [ $# -eq 0 ]; then
  echo "Usage: $0 [-h] <file>[@<lines>] [file ...]" >&2
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
  date=$(git log -1 --format="%ai" "$commit")
  echo "${ts} ${tag} ${date}"
done < <(git tag --merged master 'chart/*.0' | sort -V) > "$releases_tmp"

annotate() {
  while read -r commit_ts rest; do
    commit=$(echo "$rest" | awk '{print $4}')
    release=""
    release_date=""
    while read -r ts tag rdate; do
      if [ "$ts" -ge "$commit_ts" ]; then
        release="$tag"
        release_date="$rdate"
        break
      fi
    done < <(sort -n "$releases_tmp")
    if [ -n "$release" ]; then
      echo "$rest [$release, $release_date]"
    else
      echo "$rest [unreleased]"
    fi
  done
}

for arg in "$@"; do
  file="${arg%@*}"
  lines="${arg##*@}"
  if [ "$file" = "$arg" ]; then
    lines=""
  fi

  if [ ! -f "$file" ]; then
    echo "Error: file not found: $file" >&2
    exit 1
  fi

  echo "=== $arg ==="

  if [ -z "$lines" ]; then
    git log --follow --format="%ct %ai %H %s" -- "$file" | annotate
  else
    commits_tmp=$(mktemp)
    IFS=',' read -ra ranges <<< "$lines"
    for r in "${ranges[@]}"; do
      if [[ "$r" == *"-"* ]]; then
        start="${r%-*}"
        end="${r#*-}"
      else
        start="$r"
        end="$r"
      fi
      git log -L "${start},${end}:${file}" -s --format="%ct %ai %H %s" >> "$commits_tmp"
    done
    # Each line-range query may return the same commit. `sort -u`
    # deduplicates and sorts by timestamp (field 1).
    sort -u -k1,1n "$commits_tmp" | annotate
    rm -f "$commits_tmp"
  fi

  echo
done
