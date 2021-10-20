#!/bin/bash
sed 's| =/= |\n|' | {
    IFS= read first
    IFS= read second
    exec wdiff -n -w $'\033[30;41m' -x $'\033[0m' \
         -y $'\033[30;42m' -z $'\033[0m' \
         <(echo "$first") <(echo "$second")
}
