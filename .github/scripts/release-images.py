#!/usr/bin/env python3
"""Lists the wire-server images the packaged charts deploy at TAG: every image
whose tag make release-chart stamped with the version, found in the values files
under CHARTS_DIR (the umbrella chart, its local subcharts and the other charts).
Prints one repository per line, without tag, sorted; writes images=<space
separated> to GITHUB_OUTPUT when set."""
import os
import sys

import yaml

PREFIX = "quay.io/wire/"


def die(message):
    print(f"::error::{message}", file=sys.stderr)
    sys.exit(1)


def stamped(node, tag, found):
    if isinstance(node, dict):
        repository = node.get("repository")
        if isinstance(repository, str) and repository.startswith(PREFIX) and node.get("tag") == tag:
            found.add(repository)
        if node.get("tag") == tag:
            for value in node.values():
                if isinstance(value, str) and value.startswith(PREFIX):
                    found.add(value)
        for value in node.values():
            stamped(value, tag, found)
    elif isinstance(node, list):
        for value in node:
            stamped(value, tag, found)


def main():
    charts_dir = os.environ.get("CHARTS_DIR", ".local/charts")
    tag = os.environ.get("TAG") or die("TAG is required")
    if not os.path.isdir(charts_dir):
        die(f"{charts_dir} does not exist: package the charts first")
    found = set()
    files = 0
    for root, dirs, names in os.walk(charts_dir):
        dirs[:] = sorted(d for d in dirs if d != "charts")
        if "values.yaml" in names:
            files += 1
            path = os.path.join(root, "values.yaml")
            with open(path, encoding="utf-8") as handle:
                try:
                    stamped(yaml.safe_load(handle), tag, found)
                except yaml.YAMLError as error:
                    die(f"{path}: {error}")
    if not files:
        die(f"no values.yaml under {charts_dir}")
    for required in ("quay.io/wire/brig", "quay.io/wire/nginz", "quay.io/wire/wire-server-enterprise"):
        if required not in found:
            die(f"{required} is not stamped with {tag} in {charts_dir}: the charts were not packaged at that version")
    images = sorted(found)
    print("\n".join(images))
    output = os.environ.get("GITHUB_OUTPUT")
    if output:
        with open(output, "a", encoding="utf-8") as handle:
            handle.write(f"images={' '.join(images)}\n")
            handle.write(f"count={len(images)}\n")


if __name__ == "__main__":
    main()
