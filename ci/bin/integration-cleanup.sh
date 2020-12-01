#!/usr/bin/env bash

# Script to delete any helm releases prefixed with `test-` older than 20 minutes deemed inactive
#
# Motivation: cleanup of old test clusters that were not deleted (e.g. by the CI system, because it broke)

date_seconds() {
  d=$1
  if [[ $OSTYPE =~ darwin* ]]; then
    date -j -f "%b %d %H:%M:%S %Y" "$date_str" +"%s"
  else 
    date -d "$d" +"%s"
  fi
}

date_now=$(date +"%s")

# Since helm only shows information as hard-to-parse text pending https://github.com/kubernetes/helm/pull/2950
# we are stuck with brittle grep/awk magic.

IFS=$'\n'
for release in $(helm list | grep test-); do
  name=$(echo "$release" | awk '{print $1}')
  date_str=$(echo "$release" | awk '{print $4" "$5" "$6" "$7}')
  date_s=$(date_seconds "$date_str")
  diff=$(( $date_now - $date_s ))
  echo "debug, this should hold: $name at $date_str = $date_s = $date_now + $diff"
  if [ $diff -ge 1200 ]; then
    echo "test release '$name' older than 20 min; deleting..."
    helm delete --purge "$name"
  fi

done
unset IFS
