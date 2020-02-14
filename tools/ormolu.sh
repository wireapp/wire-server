#!/bin/bash

set -e

# TODO: change to stack project root directory.


if [ "`git status -s src/ test/ | grep -v \?\?`" != "" ]; then
    echo "working copy not clean."
    if [ "$1" == "-f" ]; then
        echo "running with -f.  THIS WILL DESTROY YOUR UNCOMMITTED CHANGES."
        git reset --hard HEAD
    else
        echo "run with -f if you want to force changing the uncommitted files."
        echo "WARNING: THIS WILL DESTROY YOUR UNCOMMITTED CHANGES."
        exit 1
    fi
fi

for i in `git ls-files | grep '\.hs$'`; do
    echo -n $i
    if grep -q '{-#\s*LANGUAGE CPP\s*#-}' $i; then
        echo "  *** ignored: -XCPP"
    else
        echo
        stack exec -- ormolu $i || echo -e "*** ormolu failed on $i\n"
    fi
done


# TODO: we may need to set the language extensions from package-defaults on the ormolu command line.
