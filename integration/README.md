How to use:

To run the whole testsuite:

1. `make c` - build the whole project without unit test
2. `make ci package=integration` - build test suite, start services and run test suite


To interactively develop and run tests:

1. `make cr` - build the whole project and start all services
2. `make devtest test=TESTNAME` to start a ghcid that re-runs the test after each succesful build


Original design guidelines / goals:

- Don't import `wire-api` or any other internal Haskell libraries. This goal of this test suite is test against the public API interface.
- Make input of functions as generic as possible, i.e. use `ProducesJSON` as much as possible. That way users can provide values without binding or converting.
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
