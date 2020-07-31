#!/usr/bin/env bash

set -euo pipefail

command -v sed  >/dev/null 2>&1 || { echo >&2 "sed is not installed, aborting."; exit 1; }

BASE_COMMIT=$1
TARGET_COMMIT=$2
FORMATTING_COMMAND='make formatf'
USAGE="
USAGE: $0 BASE_COMMIT TARGET_COMMIT
    BASE_COMMIT:
    TARGET_COMMIT:

Rebase a branch onto changes created by an automated formatter. The script
will keep the (linear) history of the branch intact and make the commits appear
as if the changes had been applied onto the newly-formatted version all along.

TODO: explain base commit etc.

INSTRUCTIONS:
1. Make a copy of your branch (or be prepared to salvage it from reflog).
2. Find out what the base commit is.
3. Rebase onto the base commit yourself.
4. Make sure the formatting tool is installed with the correct version and settings (i.e. stack install ormolu).
5. Run this script.

"

if [ -z "$BASE_COMMIT" || -z "$TARGET_COMMIT" || -z "$FORMATTING_COMMAND" ]
then
  echo "$USAGE" 1>&2
  exit 1
fi

echo "Running the script now. This might take a while..."

# The idea behind the sscript is ... TODO

set -x

# edit every commit Ci, adding f(Ci) and r(F(Ci))
git rebase $BASE_COMMIT~1 --exec "$FORMATTING_COMMAND && git commit -am "format" && git revert HEAD --no-edit"

# drop last commit
git reset HEAD~1 --hard

# now for every Ci, squash with the previous and next commit (i.e. r(f(C(i-1))) and f(Ci))
# - in sequence editor, squash lines 3, 6, 9, ... and fixup lines 4, 7, 10, ...
# - in commit message editor, drop first 9 lines (removing the commit message of the revert commit)
GIT_SEQUENCE_EDITOR='sed -i -e "3~3s/pick/squash/" -e "4~3s/pick/fixup/"' \
  GIT_EDITOR='sed -i "1,9d"' \
  git rebase --interactive $BASE_COMMIT

# rebase onto TARGET_COMMIT.
# Annoyingly, we still have this first "format" commit that should already be
# part of the TARGET_COMMIT. So we drop it.
GIT_SEQUENCE_EDITOR='sed -i "1s/pick/drop/"' \
  git rebase --interactive $BASE_COMMIT --onto $TARGET_COMMIT

echo "Done."
echo "Please check that the history looks as it should and all expected commits are there."
