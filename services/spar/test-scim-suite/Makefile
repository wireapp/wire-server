.DEFAULT_GOAL := local

.PHONY: clean
clean:
	rm -f /tmp/scim_test_suite.json

/tmp/3b5c4b838ec66cacd53b.json:
	wget --no-check-certificate https://www.getpostman.com/collections/3b5c4b838ec66cacd53b -O /tmp/3b5c4b838ec66cacd53b.json

/tmp/scim_test_suite.json: /tmp/3b5c4b838ec66cacd53b.json setup.js update.jq
	./mk_collection.sh /tmp/3b5c4b838ec66cacd53b.json > /tmp/scim_test_suite.json

local: /tmp/scim_test_suite.json
	SCIM_TEST_SUITE_SPAR_HOST=localhost \
	SCIM_TEST_SUITE_SPAR_PORT=8088 \
	SCIM_TEST_SUITE_BRIG_HOST=localhost \
	SCIM_TEST_SUITE_BRIG_PORT=8082 \
	./run.sh /tmp/scim_test_suite.json --folder "User tests" --folder "User tests with garbage"

collection: /tmp/scim_test_suite.json
