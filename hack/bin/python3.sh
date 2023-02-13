#!/usr/bin/env sh

# You can use this as wrapper when configurating your editor's linter integration

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

export PYTHONPATH="$TOP_LEVEL/hack/python"
exec "$TOP_LEVEL/.env/bin/python3" "$@"
