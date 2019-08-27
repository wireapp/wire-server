#!/usr/bin/env bash

# this is a very hacky bash script designed to show teams and team members for small installations and small amounts of data.
# Such a tool should instead be implemented in haskell, a docker image could be provided to make it easier to run this.
# the only reason for this bash script is its relative portability: haskell or docker need not be installed on the target machine.


## run the following from a cassandra database instance

# cqlsh -e "use galley; paging off; select team,name,creator from team;" > teams.txt
# cqlsh -e "use brig; paging off; select id,email,handle,name,team from user;" > users.txt

## The following operates on the two files users.txt and teams.txt and attempts to do a join operation.

function print_team_user() {
    local tid=$1
    echo "=> users"
    head -10 users.txt | grep email | grep handle
    cat users.txt | grep ".*|.*|.*|.*|\ *$tid"
    echo ""
    echo ""
}

while read -r team; do
    # skip header line
    echo $team | grep "creator" &> /dev/null && continue
    tid=$(echo $team | awk '{print $5}')
    # filter out empty lines
    if [[ -n $tid ]] ; then
        users=$(cat users.txt | grep $tid)
        # filter out teams with no users, if any
        if [[ -n $users ]]; then
            echo "------- Team: $tid"
            head -10 teams.txt | grep creator
            echo " $team"
            print_team_user "$tid"
        fi
    fi
done <teams.txt

echo "---------users without a team:"
print_team_user "null"
