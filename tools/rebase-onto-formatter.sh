#!/usr/bin/env bash

set -euo pipefail

command -v sed  >/dev/null 2>&1 || { echo >&2 "sed is not installed, aborting."; exit 1; }

BASE_COMMIT=${1:-}
TARGET_COMMIT=${2:-}
FORMATTING_COMMAND='make formatf'
USAGE="
USAGE: $0 BASE_COMMIT TARGET_COMMIT

    BASE_COMMIT:
        A commit that contains the changes to formatting version and
        config already from TARGET_COMMIT, but not the automatically
        applied formatting changes.  Must be the first commit on the
        branch you are about to rebase (not the one returned by
        git-merge-base). It will be removed from the resulting branch.
    TARGET_COMMIT:
        The commit introducing the formatting that you want to rebase onto.

Rebase a branch onto changes created by an automated formatter. The script
will keep the (linear) history of the branch intact and make the commits appear
as if the changes had been applied onto the newly-formatted version all along.

INSTRUCTIONS:
1. Make a copy of your branch (or be prepared to salvage it from reflog).
   $ git branch mybranch-backup
2. Find out what the base commit is.
3. Rebase onto the base commit yourself.
   $ git rebase \$BASE_COMMIT
4. Make sure the formatting tool is installed with the correct version and settings.
   $ stack install ormolu
5. Run this script.
   $ $0 \$BASE_COMMIT \$TARGET_COMMIT

"

if [ -z "$BASE_COMMIT" ] || [ -z "$TARGET_COMMIT" ] || [ -z "$FORMATTING_COMMAND" ]
then
  echo "$USAGE" 1>&2
  exit 1
fi

echo "Running the script now. This might take a while..."

# The general idea is the following:
#
# We have a branch consisting of commits C1, C2, ... on top of our BASE_COMMIT C0.
# Also, from C0 an automated formatting change f was made on some branch (e.g. develop).
#
#  C0 ----> C1 ----> C2 ----> ... ----> Cn
#  |
#  f
#  |
#  v
#  C0'
#
# Now, how do we obtain versions of our commits operating on the formatted code (let's call them Ci')?
#
#  C0 ----> C1 ----> C2 ----> ... ----> Cn
#  |
#  f
#  |
#  v
#  C0' ---> C1' ---> C2' ---> ... ----> Cn'
#
# One useful thing is that since f is defined by an automated tool,
# we know f applied at every commit Ci, resulting in a hypothetical Ci'.
#
#  C0 ----> C1 ----> C2 ----> ... ----> Cn
#  |        |        |                  |
#  f        f        f                  f
#  |        |        |                  |
#  v        v        v                  v
#  C0'      C1'      C2'                Cn'
#
# And we can also get its inverse g (applied at Ci') by reverting the commit.
#
#  C0 ----> C1 ----> C2 ----> ... ----> Cn
#  |^       |^       |^                 |^
#  f|       f|       f|                 f|
#  |g       |g       |g                 |g
#  v|       v|       v|                 v|
#  C0'      C1'      C2'                Cn'
#
# Finally, we can get from C(i-1)' to Ci' by composing three arrows:
# - g at C(i-1)
# - Ci
# - f at C1
#
#  C0 ----> C1 ----> C2 ----> ... ----> Cn
#  |^       |^       |^                 |^
#  f|       f|       f|                 f|
#  |g       |g       |g                 |g
#  v|       v|       v|                 v|
#  C0' ---> C1' ---> C2' ---> ... ----> Cn'

set -x

# edit every commit Ci, adding new commits representing f at Ci and it's inverse g
git rebase "$BASE_COMMIT"~1 --exec "$FORMATTING_COMMAND && git commit -am format && git revert HEAD --no-edit"

# drop last commit (do not revert formatting at the end of the branch)
git reset HEAD~1 --hard

# now for every Ci, squash with the previous and next commit (i.e. g at C(i-1) and f at Ci).
# However, we want to use Ci's commit message and author.
# To do this, we run the following command after each group of these 3 commits:
# Ci=$(git rev-parse HEAD~1); git reset --soft HEAD~3; git commit --reuse-message $Ci
# We do an interactive rebase, but instead of editing the commit sequence manually,
# we use sed for that, inserting an `exec` command after every 3 commits.
# shellcheck disable=SC2016
GIT_SEQUENCE_EDITOR='sed -i -e "4~3s/^\(pick \S* format\)$/\1\nexec Ci=\$(git rev-parse HEAD~1); git reset --soft HEAD~3; git commit --reuse-message \$Ci/"' \
  git rebase --interactive "$BASE_COMMIT"

# rebase onto TARGET_COMMIT.
# Annoyingly, we still have this first "format" commit that should already be
# part of the TARGET_COMMIT. So we drop it.
GIT_SEQUENCE_EDITOR='sed -i "1s/pick/drop/"' \
  git rebase --interactive "$BASE_COMMIT" --onto "$TARGET_COMMIT"

echo "Done."
echo "Please check that the history looks as it should and all expected commits are there."
