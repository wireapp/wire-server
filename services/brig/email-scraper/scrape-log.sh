#!/bin/bash

# given a team id and a list of domains, generate two files:
# inside.csv contains email and uid of all users with an email address
# under any of those domain in the team, and outside.csv contains all
# such users *outside* the team.
#
# the first is just for double-checking; the second is required in
# situations where those users are to be onboarded into the team, but
# have accidentally created a personal (non-team) user and need to be
# assisted in joining the team.
#
# scanning 1M users takes a little over a minute on a bad internet
# connection.
#
# you need to define these before calling this script:
#
# export CASS_IP=...
# export BRIG_IP=...
# export DOMAINS=...
# export TEAM_ID=...

# you need to run these two lines manually first:
# ssh -L 127.0.0.1:9042:${CASS_IP}:9042 ubuntu@{BRIG_IP}
# stack exec brig-email-scraper -- --cassandra-host-brig 127.0.0.1 --cassandra-port-brig 9042 --cassandra-keyspace-brig brig --domains ${DOMAINS} > scraper.log

echo "userid,email,teamid,activated" > inside.csv
echo "userid,email,teamid,activated" > outside.csv
grep '\@' scraper.log | perl -pe '/I, \(([^,]+),"([^"]+)","([^"]+)","([^"]+)"/ && print "$1,$2,$3,$4\n"' | grep ${TEAM_ID} >> inside.csv
grep '\@' scraper.log | perl -pe '/I, \(([^,]+),"([^"]+)","([^"]+)","([^"]+)"/ && print "$1,$2,$3,$4\n"' | grep -v ${TEAM_ID} >> outside.csv
