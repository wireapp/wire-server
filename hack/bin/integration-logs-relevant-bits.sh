#!/usr/bin/env bash
set -euo pipefail

USAGE="
Filter out garbage from logs (but keep colors and highlight problems).

(Adapted from logfilter.sh by Stefan Matting)

To use it pipe log output from integration-test.sh into this tool.

Usage: logs | $0 [options]
"

red_color="\x1b\[;1m\x1b\[31m"
problem_markers="Failures:|FAILED|ExitFailure""\
|\^+""\
|FAIL""\
|tests failed""\
|Test suite failure""\
|""$red_color""error:""\
|""$red_color""\
|Test suite .+ failed"

exit_usage() {
    echo "$USAGE"
    exit 1
}

cleanup() {
    # replace backspaces with newlines
    # remove "Progress" lines
    # Remove blank lines
    sed 's/\x08\+/\n/g' |
        sed '/^Progress [0-9]\+/d' |
        sed '/^\s\+$/d'
}

grepper() {
    # print 10 lines before/after for context
    rg "$problem_markers" --color=always -A 10 -B 10
}

cleanup | grepper
