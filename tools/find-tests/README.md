This is a tool for identifying tests listed in an audit requirements
document maintained by Wire QA.  The only thing you need to know to
run this:

* input:
    * the name of a file in the wire-server code base
    * the name of a test case (the function) in that module

* output (maybe a json object?):
    * the code, including comments and line numbers, that is currently wrapped in “BSI test tags” (-- @..., -- END) (those may not be needed any more if we use package ghc for parsing; as the keywords in tags are maintained elsewhere in bsi-mapping).
    * a way to find the test output of a run of the test suite.  (challenge: test "description" testFun doesn’t mention testFun in the logs, only "description".)
    * the url linking to the code on github.com/wireapp/wire-server (optional).

The Haskell code must follow these rules:

1. the function must be declared on top level (column 1).
2. the order must be comment, signature, fundecl, in that order.
3. no empty lines between comment, signature, fundecl.
