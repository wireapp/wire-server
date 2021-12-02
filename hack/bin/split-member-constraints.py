#!/usr/bin/env python3

# This script splits all polysemy `Members` constraints into multiple `Member`
# constraints. The intended usage is to find redundant effects in function
# signatures.
#
# Example usage:
#
# $ git status # make sure working directory is clean
# $ fd -ehs services/galley/src -x hack/bin/split-member-constraints.py '{}' '{}'
# $ WIRE_STACK_OPTIONS="$WIRE_STACK_OPTIONS --ghc-options='-Wredundant-constraints -Wwarn'" make
# $ git reset --hard
#
# Now you can scroll back to find a list of redundant constraint warnings and
# fix them, but note that the line numbers are no longer accurate.

import re
import sys

def make_constraint(e):
    e = e.strip()
    if ' ' in e:
        e = '(' + e + ')'
    return f'Member {e} r'

def f(m):
    effects = re.split(r'\s*,\s*', m.group(1))
    constraints = ', '.join(make_constraint(e) for e in effects)
    return f'({constraints})'

code = open(sys.argv[1]).read()
print(re.sub(r"Members\s+'\[\s*([^\]]*)\s*\]\s+r", f, code, flags=re.MULTILINE),
    file=open(sys.argv[2], 'w'), end='')
