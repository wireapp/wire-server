LANG := en_US.UTF-8
SHELL := /usr/bin/env bash

# GCM test with a real push token.
# Usage: 'make test-gcm PORT=... TOKEN=...'
test-gcm:
	$(eval UID := CCFA0BBB-87CF-47CD-BA28-6DCD8E2D32F3)
	$(eval KEY := MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA=)
	@echo "Registering client"
	@curl -s -XPUT 'http://localhost:${PORT}/i/clients/1' \
	     -d '{"mackey": "${KEY}", "enckey": "${KEY}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json'
	@echo "Registering push token"
	@curl -s -XPOST 'http://localhost:${PORT}/push/tokens' \
	     -d '{"client":"1", "transport": "GCM", "app": "test", "token": "${TOKEN}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json' > /dev/null
	@echo "Push plain"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
		-d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${UID}", "payload": [{"type":"test"}], "native_encrypt": false}]'
	@echo "Push encrypted"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
	     -d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${UID}", "payload": [{"type":"test"}]}]'

# APNS test with a real push token.
# Usage: 'make test-apns PORT=... TOKEN=...'
test-apns:
	$(eval UID := B69D8444-0D78-404F-9BF8-0D663F83447A)
	$(eval KEY := MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA=)
	@echo "Registering client"
	@curl -s -XPUT 'http://localhost:${PORT}/i/clients/1' \
	     -d '{"mackey": "${KEY}", "enckey": "${KEY}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json'
	@echo "Registering push token"
	@curl -s -XPOST 'http://localhost:${PORT}/push/tokens' \
	     -d '{"client":"1", "transport": "APNS_SANDBOX", "app": "test", "token": "${TOKEN}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json' > /dev/null
	@echo "Push plain"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
		-d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${UID}", "payload": [{"type":"test"}], "native_aps": {"sound": "default", "loc_key": "test_plain", "loc_args": []}, "native_encrypt": false}]'
	@echo "Push encrypted"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
	     -d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${UID}", "payload": [{"type":"test"}], "native_aps": {"sound": "default", "loc_key": "test_cipher", "loc_args": []}}]'

# APNS VoIP test with a real VoIP push token.
# Usage: 'make test-apns-voip PORT=... VOIP_TOKEN=...'
test-apns-voip:
	$(eval UID := ADA059E6-D7CC-437D-BA55-ED485E76C1F2)
	$(eval KEY := MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA=)
	@echo "Registering client"
	@curl -s -XPUT 'http://localhost:${PORT}/i/clients/1' \
	     -d '{"mackey": "${KEY}", "enckey": "${KEY}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json'
	@echo "Registering push token"
	@curl -s -XPOST 'http://localhost:${PORT}/push/tokens' \
	     -d '{"client":"1", "transport": "APNS_VOIP_SANDBOX", "app": "test", "token": "${VOIP_TOKEN}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json' > /dev/null
	@echo "Push plain"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
		-d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${UID}", "payload": [{"type":"test"}], "native_encrypt": false}]'
	@echo "Push encrypted"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
	     -d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${UID}", "payload": [{"type":"test"}]}]'

# APNS preference test with two push tokens.
# Usage: 'make test-apns-preference PORT=... TOKEN=... VOIP_TOKEN=... PREFERENCE=...'
test-apns-preference:
	$(eval UID := EE7F89FC-D92A-46FD-BB57-4C36BA8731A9)
	$(eval KEY := MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA=)
	@echo "Registering client"
	@curl -s -XPUT 'http://localhost:${PORT}/i/clients/1' \
	     -d '{"mackey": "${KEY}", "enckey": "${KEY}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json'
	@echo "Registering push token"
	@curl -s -XPOST 'http://localhost:${PORT}/push/tokens' \
	     -d '{"client":"1", "transport": "APNS_SANDBOX", "app": "test", "token": "${TOKEN}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json' > /dev/null
	@echo "Registering VoIP push token"
	@curl -s -XPOST 'http://localhost:${PORT}/push/tokens' \
	     -d '{"client":"1", "transport": "APNS_VOIP_SANDBOX", "app": "test", "token": "${VOIP_TOKEN}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json' > /dev/null
	@echo "Push plain (${PREFERENCE})"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
		-d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${UID}", "payload": [{"type":"test"}], "native_aps": {"loc_key":"plain", "preference":"${PREFERENCE}"}, "native_encrypt": false}]'
	@echo "Push encrypted (${PREFERENCE})"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
	     -d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${UID}", "payload": [{"type":"test"}], "native_aps":{"loc_key":"cipher", "preference":"${PREFERENCE}"}}]'

# APNS VoIP fallback test
# Usage: 'make test-apns-fallback PORT=... TOKEN=... VOIP_TOKEN=...'
# Note: The origin ($ORIG) of the push must be different from the
#       recipient ($UID) in order for the fallback to be scheduled,
#       since we never schedule fallback notifications for other
#       devices of the originator (which would be undsirable).
test-apns-fallback:
	$(eval UID := 31DC3586-77FD-4262-B1D3-6E489F3548F1)
	$(eval ORIG := D9AC666B-2A0B-4D01-8B97-A1FE33131769)
	$(eval KEY := MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA=)
	@echo "Registering client"
	@curl -s -XPUT 'http://localhost:${PORT}/i/clients/1' \
	     -d '{"mackey": "${KEY}", "enckey": "${KEY}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json'
	@echo "Registering APNS push token"
	@curl -s -XPOST 'http://localhost:${PORT}/push/tokens' \
	     -d '{"client":"1", "transport": "APNS_SANDBOX", "app": "test", "token": "${TOKEN}"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json' > /dev/null
	@echo "Registering VOIP push token"
	@curl -s -XPOST 'http://localhost:${PORT}/push/tokens' \
	     -d '{"client":"1", "transport": "APNS_VOIP_SANDBOX", "app": "test", "token": "${VOIP_TOKEN}", "fallback": "APNS_SANDBOX"}' \
	     -H 'Z-User: ${UID}' \
	     -H 'Z-Connection: 1' \
	     -H 'Content-Type: application/json' > /dev/null
	@echo "Push plain"
	@curl -s -XPOST 'http://localhost:${PORT}/i/push' \
		-d '[{"recipients": [{"user_id":"${UID}", "route": "native"}], "origin": "${ORIG}", "payload": [{"type":"test"}], "native_encrypt": false}]'
