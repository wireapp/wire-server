#!/bin/sh

set -e

setup_js_jsonlines=$(mktemp /tmp/setup_inline_XXXXXXX.json)
cat ./setup.js | python3 -c '
import sys, json;
print(json.dumps(sys.stdin.read().splitlines()))
' > $setup_js_jsonlines

jq --slurpfile setup_inline "$setup_js_jsonlines" -f ./update.jq $1
