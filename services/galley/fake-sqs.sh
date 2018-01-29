#!/usr/bin/env bash

function cleanup() {
  printf 'cleaning up ...'
  docker kill $(cat DOCKER_ID)
  rm DOCKER_ID
  printf done
}

trap cleanup EXIT

docker run -d --cidfile DOCKER_ID -p 4568:4568 airdock/fake-sqs 
while true
do
  curl --silent http://localhost:4568 -d "Action=CreateQueue&QueueName=integration-team-events.fifo"
  if [ $? -eq 0 ]; then
    echo "Queue created, all set!"
    break
  else
    echo "Waiting for the queue to be created..."
  fi
  sleep 0.5
done

while true
do
  sleep 10
done
