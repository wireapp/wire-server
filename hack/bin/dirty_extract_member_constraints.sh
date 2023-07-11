#!/bin/bash

# `Members` constraints and already defined constrain sets including `Member` constraints
# are not considered here and need to be done manually.

# FUTUREWORK: extract this from cabal.project instead of copying it here.
export TARGETS="integration libs/bilge/ libs/brig-types/ libs/cargohold-types/ libs/cassandra-util/ libs/extended/ libs/dns-util/ libs/deriving-swagger2/ libs/galley-types/ libs/gundeck-types/ libs/hscim/ libs/http2-manager/ libs/imports/ libs/jwt-tools/ libs/metrics-core/ libs/metrics-wai/ libs/polysemy-wire-zoo/ libs/ropes/ libs/schema-profunctor/ libs/sodium-crypto-sign/ libs/ssl-util/ libs/tasty-cannon/ libs/types-common/ libs/types-common-aws/ libs/types-common-journal/ libs/wai-utilities/ libs/wire-api/ libs/wire-api-federation/ libs/wire-message-proto-lens/ libs/zauth/ services/background-worker/ services/brig/ services/cannon/ services/cargohold/ services/federator/ services/galley/ services/gundeck/ services/proxy/ services/spar/ tools/db/assets/ tools/db/auto-whitelist/ tools/db/billing-team-member-backfill/ tools/db/find-undead/ tools/db/inconsistencies/ tools/db/migrate-sso-feature-flag/ tools/db/move-team/ tools/db/repair-handles/ tools/db/service-backfill/ tools/fedcalls/ tools/rex/ tools/stern/"

for i in $(echo $TARGETS); do
    find services/galley/src/ -name '*.hs' | xargs perl -ne 'if (/\bMember\b/ && !/^import/) {s/^\s*\w+\s+::\s+//; s/^([\s\(]*)Member/Member/; s/((\) =>)|(=>)|,).*//g; print; if ($i = 1) {} else {print "\n"; $i=1}} else {if ($i=0) {} else {print "\n"; $i=0}}' | uniq > $i/members.dump
done
