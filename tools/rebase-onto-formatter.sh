#!/usr/bin/env bash

set -euo pipefail

echo "INSTRUCTIONS:"
echo "1. Make a copy of your branch (or be prepared to salvage it from reflog)."
echo "2. Find out the what the base commit is (in our case ...)"
echo "3. Rebase onto the base commit yourself."
echo "4. Make sure the formatting tool is installed with the correct version and settings (i.e. stack install ormolu)."
echo "5. Run this script."
echo ""
echo "Running the script now. This will take a while."
[[ "$(read -e -p 'Continue? [y/N]> '; echo $REPLY)" == [Yy]* ]] || exit 0

set -x

# TODO: check existence of tools

# TODO: just show usage/instructions if arguments are not supplied?
BASE_COMMIT=$1
TARGET_COMMIT=$2
FORMATTING_COMMAND='make formatf'

# edit every commit Ci, adding f(Ci) and r(F(Ci))
git rebase $BASE_COMMIT~1 --exec "$FORMATTING_COMMAND && git commit -am "format" && git revert HEAD --no-edit"

# drop last commit
git reset HEAD~1 --hard

# now for every Ci, squash with the previous and next commit (i.e. r(f(C(i-1))) and f(Ci))
# in sequence editor, squash lines 3, 6, 9, ... and fixup lines 4, 7, 10, ...
# in commit message editor, drop drop first 9 lines
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
