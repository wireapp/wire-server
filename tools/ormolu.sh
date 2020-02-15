#!/bin/bash

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )/.."

stack exec -- which ormolu >/dev/null || ( echo "please run stack install ormolu."; exit 1 )

if [ "`git status -s src/ test/ | grep -v \?\?`" != "" ]; then
    echo "working copy not clean."
    if [ "$1" == "-f" ]; then
        echo "running with -f.  this will mix ormolu and other changes."
    else
        echo "run with -f if you want to force mixing ormolu and other changes."
        exit 1
    fi
fi

export LANGUAGE_EXTS=$(perl -ne '$x=1 if /default-extensions:/?1:(/^[^-]/?0:$x); print "--ghc-opt -X$1 " if ($x && /^- (.+)/);' package-defaults.yaml)

for i in `git ls-files | grep '\.hs$'`; do
    echo -n $i
    if grep -q '{-#\s*LANGUAGE CPP\s*#-}' $i; then
        echo "  *** ignored: -XCPP"
    else
        echo
        stack exec -- ormolu --mode inplace --check-idempotency ${LANGUAGE_EXTS} $i || echo -e "*** ormolu failed on $i\n"
    fi
done




# TODO: use getopts and avoid dirty-working-copy test if not needed.

# TODO: have test param that makes the script pass if idempotency is reached, and fail if ormolu made changes.  (--mode check?)

# TODO: isolate or remove CPP flag; consider going for the module name matching /CPP$/ and enforcing this as a convention.
