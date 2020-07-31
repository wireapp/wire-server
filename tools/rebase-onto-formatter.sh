#!/usr/bin/env bash

set -euo pipefail

command -v sed  >/dev/null 2>&1 || { echo >&2 "sed is not installed, aborting."; exit 1; }

BASE_COMMIT=${1:-}
TARGET_COMMIT=${2:-}
FORMATTING_COMMAND='make formatf'
USAGE="
USAGE: $0 TARGET_COMMIT BASE_COMMIT

    TARGET_COMMIT:
        The commit introducing the formatting that you want to rebase onto.
    BASE_COMMIT:
        A commit very similar to TARGET_COMMIT, just that the automated formatting changes are not applied yet.
        It has to include changes to formatting version and config already.

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
   $ $0 \$TARGET_COMMIT \$BASE_COMMIT

"

if [ -n "$BASE_COMMIT" ] || [ -n "$TARGET_COMMIT" ] || [ -n "$FORMATTING_COMMAND" ]
then
  echo "$USAGE" 1>&2
  exit 1
fi

echo "Running the script now. This might take a while..."

# The general idea is the following:
#
# We have a branch consisting of changes C1, C2, ... on top of our BASE_COMMIT 0.
# Also, from the base commit, a formatting change `f(0)` was made.
#
# ```
#  0 --- C1 ---> 1 --- C2 ---> 2 --- C3 ---> ... --- Cn ---> n
#  |
#  f
#  |
#  v
#  0'
# ```
#
# Now, how do we obtain versions of our changes operating on the formatted code (let's call them Ci')?
#
# ```
#  0 --- C1 ---> 1 --- C2 ---> 2 --- C3 ---> ... --- Cn ---> n
#  |
#  f
#  |
#  v
#  0' -- C1' --> 1' -- C2' --> 2' -- C3' --> ... --- Cn' --> n'
# ```
#
# One useful thing is that since `f` is defined by an automated tool,
# we know `f(i)` on every commit `i`.
#
# ```
#  0 --- C1 ---> 1 --- C2 ---> 2 --- C3 ---> ... --- Cn ---> n
#  |             |             |                             |
#  f             f             f                             f
#  |             |             |                             |
#  v             v             v                             v
#  0' -- C1' --> 1' -- C2' --> 2' -- C3' --> ... --- Cn' --> n'
# ```
#
# And we can also get its inverse `r(f(i))` by reverting the commit.
#
# So we can obtain `Ci' by composing `r(f(i-1)), `Ci` and `f(i)`.

set -x

# edit every commit i, adding f(i) and r(f(i))
git rebase $BASE_COMMIT~1 --exec "$FORMATTING_COMMAND && git commit -am "format" && git revert HEAD --no-edit"

# drop last commit
git reset HEAD~1 --hard

# now for every i, squash with the previous and next commit (i.e. r(f(i-1)) and f(i))
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
