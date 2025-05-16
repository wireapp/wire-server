# Coding conventions

## On the topic of `cs`

**TL;DR**:
use `cs` only in test-suites, *don’t* use it in production code

In wire we use all types of Strings;

- `String ~ [Char]` (`base` itself still does many things with `String`, also we use it in the `/integration` test suite)
- `Text` in both its strict and lazy versions
- `ByteString` in both its strict and lazy versions

`ByteString` is literally a pointer to an Array of Bytes; there’s no inherent encoding that makes it safe to
convert from and to `String` and `Text` which are nowadays typically `utf8` encoded; that means that using
`cs :: ConvertibleStrings a b => a -> b` is not a safe operation; the encoding between a given `ByteString`
and a `String` or `Text` can be different; e.g. we could decode a `ByteString` as ASCII-Chars or as utf8, just
to name a few.

There’s another inherent problem to `cs` in that context, namely **readability**; a `TL.fromStict` immediately tells
you what the code does; `cs`, however, says nothing; you know there’s *some* conversion going on but not which.

We have hence decided to not use the error-prone and hard-to-read `cs` in production code, i.e., in all libraries
and services, and instead only allow for use in test suites in general and `integration/` more specifically.

As a consequence we also decided to drop `cs` from `Imports`.

## Source Notes

We're using [*Source
Notes*](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/coding-style#2-using-notes)
to explain bigger contexts that don't fit into code related comments.
