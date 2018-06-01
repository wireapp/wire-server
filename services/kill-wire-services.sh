#!/bin/sh

for i in wireserver/gundeck wireserver/cannon wireserver/brig wireserver/cargohold wireserver/galley; do
    echo $i=`docker ps -q --filter="ancestor=$i"`
    docker kill `docker ps -q --filter="ancestor=$i"` >/dev/null 2>&1
done
docker ps
