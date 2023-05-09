How to use:

- `make ci suite=new ` - build all services and run only this test suite
- `make ci package=galley suite=new` - build only galley then run only this test suite

See Makefile for full docs on `make ci`.

You can use `--include PATTERN` to only include tests that contain the substring `PATTERN`. This flag can be specified multiple times. The argument can also be provided with the `TEST_INCLUDE` env vars, where multiple values are comma-separated.

You can use `--exclude PATTERN` to exclude all tests that contain the substring `PATTERN`. This flag can be specified multiple times. The argument can also be provided with the `TEST_EXCLUDE` env vars, where multiple values are comma-separated.

To develop a new test in a fast-loading ghci session:

1. Run `make cr` to build the whole project and start all services
   OR run `make cr package=galley` to build galley and start all services
   OR run `./services/run-services` to just start all services without rebuilding
2.`TEST_INCLUDE=testFederationDomain make devtest` to start a ghcid session that re-runs the test after each succesful build of the test suite

Original design guidelines / goals:

- Don't import `wire-api` or any other internal Haskell libraries. This goal of this test suite is test against the public API interface.
- Make input of functions as generic as possible, i.e. use `MakesValue` as much as possible. That way users can provide values without binding or converting.
- Make it as easy to debug tests as possible
  * Add the `HasCallStack` constraint to any function
  * Prefer to make a function monadic and throw a helpful error message rather than keeping it pure (purity has little value in tests)
  * e.g. show full request/responses by default upon exceptions (use `bindResponse`)
  * all notifications received on websockets are shown in case of failure
  * always show JSON in a prettified way (see `prettyJSON`)
- Do not import unit testing libraries such as `HUnit` or `hspec`. The equivalent functions they would provide are provided by the integration package and are better integrated, for example avoiding using `liftIO`, and providing better error messages: `shouldMatch` shows formatted JSON in case of errors.  (integration is built on top of tasty, but you don't need to know this to use it.)
- If you import other libraries, make sure they are well integrated into the test suite. If not then write adapters that do.
- Avoid introducing custom operators. They are hard to remember.
- Use `String` type for strings everywhere. Don't make the user convert between all the different string types.
- Don't use `OverloadedStrings`. This creates type ambiguities for string literals
