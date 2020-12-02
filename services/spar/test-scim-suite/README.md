## Scim Test Suite

The scripts in this directory allow to run the [SCIM Test Suite](https://github.com/AzureAD/SCIMReferenceCode/wiki/Test-Your-SCIM-Endpoint) provided by Microsoft (a [Postman](https://www.postman.com/) collection) against the SCIM implementation in spar.

How to run:
```sh
./services/start-services-only.sh
./services/spar/test-scim-suite/runsuite.sh
```

This will download the Postman collection, patch it and then call `newman` (the cli tool from Postman) to run the tests.

It is also possible to run the tests from the test suite manually:

1. Run `nix-shell`.
2. Run `make collection`.
3. In Postman "Import" the collection `/tmp/scim_test_suite.json`
4. Create an environment in Postman with these variables

```
Server: "localhost"
Port:   ":8088"
Api:    "scim/v2"
```

4. Run the steps from the suite

When running tests multiple times you have to manually reset the variable `wire_setup_complete` (See `setup.js`).

Note:
When running test manually be aware of timeout settings in the services config, which are typically set to only a few seconds.
If you are not fast enough you might get different results than running the suite with `newman`.
