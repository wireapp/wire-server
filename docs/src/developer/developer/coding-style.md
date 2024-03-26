
# The wire-server Coding Style Guide

This document contains a collection of rules as they emerge from
day-to-day discussions.  If you are unhappy with anything, open a PR
and trigger a discussion.  Coding style PRs of course should be
reviewed by more or less the entire team if feasible.

These rules are by no means enforced consistently at the time of
writing them down, but they are offer a way to avoid debate when
reviewing current pull requests: if there is more than one way of
doing things, whoever can point to a rule in this document wins the
argument.

Where feasible and if there is time, rules should be enforced
automatically.

### Integer literals

Integer literals are spelled `50_000` (not `50 * 1000` and not
`50000`).  (NB: ghc allows to write things like `40_0000`, which makes
sense eg. in Chinese.  This is illegal in this code base for obvious
reasons.)

### Libraries and dependencies

Instead of the standard `Prelude`, we use the module `Imports` from
`/libs/imports/`.  (Exception: see below.)

`/integration` has some restrictions on what it may depend on:

- `/libs/imports` (there is some ongoing debate whether that exception should be removed, though.)
- `/libs/wire-api` (compiling wire-api takes longer, and there was a small majority in favor of testing serialization here, not (only) in golden tests.)

`string-conversions` is forbidden for production code, since some of
the transformations throw impure exceptions (notably `ByteString` to
`Text`).  It's ok to use it in test code.


also go through the C&D history and pick details on that, and other decisions.


### Module boundaries

All modules must have explicit export lists.  Reasons:

- This is good documentation;
- it helps with identifying dead code (if it's not exported and not used, we get a compiler warning); and
- prevents us from using functions that aren't part of a module's interface by accident: if we *want* to use them, we can always add them, but then we are transparent about changing the interface.
