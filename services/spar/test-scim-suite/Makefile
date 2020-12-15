collection: /tmp/scim_test_suite.json

.PHONY: clean
clean:
	rm -f /tmp/scim_test_suite.json

/tmp/3b5c4b838ec66cacd53b.json:
	wget --no-check-certificate https://www.getpostman.com/collections/3b5c4b838ec66cacd53b -O /tmp/3b5c4b838ec66cacd53b.json

/tmp/scim_test_suite.json: /tmp/3b5c4b838ec66cacd53b.json setup.js update.jq
	./mk_collection.sh /tmp/3b5c4b838ec66cacd53b.json > /tmp/scim_test_suite.json
