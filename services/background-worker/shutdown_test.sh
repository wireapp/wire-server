#! /usr/bin/env bash

# This script emulates a Kubernetes shutdown signalling sequence for background-worker.
# NOTE: It will send signals to any currently running process called "background-work"

# Testing setup
# This script was written for the following testing methodology.
# All commands are from the wire-server repo root unless otherwise noted.
# 1) Setup local databases. `docker-compose -f deploy/dockerephemeral/docker-compose.yaml build && ./deploy/dockerephemeral/run.sh`
# 2) Run Wire services. `make cr`
# 2) Kill the running background-worker services. `pkill -9 background-work`
# 3) From the `services/background-worker` directory, run the service with the integration config. `cabal run background-worker -- -c background-worker.integration.yaml`
# 4) Run the `fill_rabbit.sh` script with the desired message.
# 5) Run this script and ensure that background worker exits gracefully.

progress-bar() {
  local duration=${1}
    already_done() { for ((done=0; done<elapsed; done++)); do printf "▇"; done }
    remaining() { for ((remain=elapsed; remain<duration; remain++)); do printf " "; done }
    percentage() { printf "| %s%%" $(( ((elapsed)*100)*100/(duration)/100 )); }
    clean_line() { printf "\r"; }
  for (( elapsed=1; elapsed<=duration; elapsed++ )); do
      already_done; remaining; percentage
      sleep 1
      clean_line
  done
  clean_line
}

echo "Sending SIGTERM"
pkill -SIGTERM background-work

echo "Sleeping for 30 seconds"
progress-bar 30

echo ""
echo "Sending SIGKILL"
pkill -SIGKILL background-work
if [ $? -eq 1 ]
then
  echo "Graceful shutdown, background-worker had already shutdown"
else
  echo "Forced shutdown, background-worker was forcably shutdown"
  exit 1
fi

# progress-bar
# 
# The MIT License (MIT)
# 
# Copyright (c) 2013 @edouard_lopez
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
