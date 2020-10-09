The scripts in this directory allow to run the SCIM Test Suite (a [Postman](https://www.postman.com/) collection) provided here
https://github.com/AzureAD/SCIMReferenceCode/wiki/Test-Your-SCIM-Endpoint
against the SCIM implementation in spar.

How to run:
```sh
./services/start-services-only.sh
cd services/spar/test-scim-suite/
nix-shell
make
```

This will download the Postman collection, patch it and use `newman` to run.

It's also possible to run the tests from the suite manually in Postman:

1. Run `make collection`.
2. In Postman "Import" the collection `/tmp/scim_test_suite.json`
3. Create an environment with these varibles

```
Server: "localhost"
Port:   ":8088"
Api:    "scim/v2"
```

4. Run the steps from the suite

When running tests multiple times you have to manually reset the variable `wire_setup_complete` (See `setup.js`).

Note:
When running test manually be aware of timeout settings in the services config, which are typically set to only a few seconds.
If you are not fast enough manully you might get different results than running the suite with `newman`.
