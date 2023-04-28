#!/usr/bin/env python3
#
# This script behaves as ./dist/integration
# but runs it in a ghcid session

import sys
import subprocess

iargs = "[" + (", ".join(["\"" + a.replace('"','\\"') + "\"" for a in sys.argv[1:]])) + "]"
ghci_expr = f'Testlib.Run.mainI {iargs}'
args = ['ghcid', '--command', 'cabal repl integration', '--test', ghci_expr]
try:
    subprocess.run(args, check=False)
except KeyboardInterrupt:
    pass
