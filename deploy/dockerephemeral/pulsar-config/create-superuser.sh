#!/usr/bin/env sh

# Create a superuser in pulsar-manager.
# username: pulsar
# password: walter-frosch
CSRF_TOKEN=$(curl http://pulsar-manager:7750/pulsar-manager/csrf-token)
curl -v -H "X-XSRF-TOKEN: $CSRF_TOKEN" \
  -H "Cookie: XSRF-TOKEN=$CSRF_TOKEN;" \
  -H "Content-Type: application/json" \
  -X PUT http://pulsar-manager:7750/pulsar-manager/users/superuser \
  -d '{"name": "pulsar", "password": "walter-frosch", "description": "test", "email": "username@test.org"}'
