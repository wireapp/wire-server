#!/usr/bin/env bash

USAGE="Write version to chart and subcharts (if any). Usage: $0 <chartname> <semantic version>"
chart=${1:?$USAGE}
version=${2:?$USAGE}

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
CHARTS_DIR="$TOP_LEVEL/.local/charts"
tempfile=$(mktemp)

# (sed usage should be portable for both GNU sed and BSD (Mac OS) sed)

function update_chart(){
    chart_file=$1
    sed -e "s/^version: .*/version: $target_version/g" "$chart_file" > "$tempfile" && mv "$tempfile" "$chart_file"
}

function write_versions() {
    target_version=$1

    # update chart version
    update_chart Chart.yaml

    # update all dependencies, if any
    if [ -a requirements.yaml ]; then
        sed -e "s/  version: \".*\"/  version: \"$target_version\"/g" requirements.yaml > "$tempfile" && mv "$tempfile" requirements.yaml
        for dep in $(helm dependency list | grep -v NAME | awk '{print $1}'); do
            if [ -d "$CHARTS_DIR/$dep" ] && [ "$chart" != "$dep" ]; then
                (cd "$CHARTS_DIR/$dep" && write_versions "$target_version")
            fi
        done
    fi
}

cd "$CHARTS_DIR/$chart" && write_versions "$version"
