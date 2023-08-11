# Testing the wire-server Haskell code base

Every package in this repository should have one or more directories
named `test` or `test*`.  Ideally, there are at least unit tests
(eg. in `test/unit`) for libraries, and and at least unit and
integration tests (eg. `test/integration`) for packages with
executables (`/services`).

See also: {ref}`run-wire-server-integration-tests-inside-kubernetes-using-helm`

## General rule

Write as much pure code as possible.  If you write a function that has
an effect only in a small part of its implementation, write a pure
function instead, and call if from an effectful function.

## Unit tests

All data types that are serialized ([`ToByteString`](https://hackage.haskell.org/package/amazonka-core-1.6.1/docs/Network-AWS-Data-ByteString.html#t:ToByteString), [`ToByteString`](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:ToJSON), swagger, cassandra, ...) should have roundtrip quickcheck tests like [here](https://github.com/wireapp/wire-server/blob/develop/libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs#L235).
All pure functions `f` that do something interesting should have a couple of tests of the form `shouldBe (f <args>) <result>` to cover corner cases.
If code is refactored, all the effects should be mocked with polysemy or mtl, the old implementation should be moved to the test suite, and there should be quickcheck tests running the new implementation against the old and comparing results ([example](https://github.com/wireapp/wire-server/blob/develop/services/gundeck/test/unit/MockGundeck.hs)).

## Integration tests

- All new rest API end-points need to be called a few times to cover as much of the corner cases as feasible.
- We have machinery to set up scaffolding teams and modify context such as server configuration files where needed.  Look through the existing code for inspiration.
- Test only semantics, not syntax: do not post verbatim json to an end-point, but use the Haskell data types to render the json that you post.
