#!/usr/bin/env bash

function cleanup() {
  printf 'cleaning up ...'
  docker kill $(cat DOCKER_ID)
  rm DOCKER_ID
  printf done
}

trap cleanup EXIT

docker run -d --cidfile DOCKER_ID -p 4568:4568 airdock/fake-sqs 
sleep 2
curl http://localhost:4568 -d "Action=CreateQueue&QueueName=integration-team-events.fifo" 

while true
do
  sleep 10
done
