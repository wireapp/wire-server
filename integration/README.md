Original design guidelines / goals:

- Don't import `wire-api` or any other internal Haskell libraries. This goal of this test suite is test against the public API interface.
- Make input of functions as generic as possible, i.e. use `ProducesJSON` as much as possible. That way users can provide values without binding or converting.
- Make it as easy to debug tests as possible
  * Add the `HasCallStack` constraint to any function
  * Prefer to make a function monadic and throw helpfull error message rather than keeping it pure (purity has little value in tests)
  * e.g. show full request/responses by default upon exceptions (use `bindResponse`)
  * all notifications received on websocket are shown in case of failure
  * always show JSON in a prettified way (see `prettyJSON`)
- unit testing libraries such as `HUnit` or `hspec` are not imported. The equivalent functions they provided that are better integrated, for example avoiding using `liftIO`, and providing better error messages: `shouldMatch` shows formatted JSON in case of errors.
- `aeson-lens` is not be imported. Equivalent functions such as `setField` (for nested updates) are integrated into the library for better error messages
- If you import other libraries, make sure they are well integrated into the test suite. If not then write adapters that do.
- Avoid introducing custom operators. They are hard to remember.
- Use `String` type for strings everywhere. Don't make the user convert between all the different string types.
- Don't use `OverloadedStrings`. This creates type ambiguities for string literals
