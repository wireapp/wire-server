#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

package=${1:-all}
pattern=${2:-}

opts=""

if [[ "$package" != "all" ]]; then
  opts="$opts -C services/$package"
fi

if [[ -n "$pattern" ]]; then
  if [[ "$package" == "all" ]]; then
    echo -e "\e[31mGlobal pattern not supported\e[0m" >&2
    exit 1
  fi
  opts="$opts i-$pattern"
else
  opts="$opts i"
fi

exec make $opts
