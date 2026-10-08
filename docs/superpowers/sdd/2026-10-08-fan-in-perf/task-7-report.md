# Task 7 report
Status: DONE. Brief code used verbatim; no compile fixes needed.
TDD: spec + cabal modules added first; build failed (Cabal error, Terminal module missing); then impl added -> green.
Tests: 50 examples, 0 failures (8 new Terminal). No warnings. ormolu clean.
Non-TTY: renderStatusLine False -> "x\n", no ESC (tested incl. formatStatus output).
