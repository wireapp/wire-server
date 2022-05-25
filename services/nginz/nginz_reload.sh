#!/usr/bin/env bash

{
  echo "Starting nginx"
  nginx "$@" && exit 1
} &

nginx_pid=$!

trap "kill -QUIT $nginx_pid" EXIT

watches=${WATCH_PATHS:-"/etc/wire/nginz/upstreams"}

# only react on changes to upstreams.conf
cfg=upstreams.conf

echo "Setting up watches for ${watches[@]}"

{
  echo "nginx PID: $nginx_pid"
  inotifywait -m -e moved_to -e modify,move,create,delete -m --format '%f' \
  ${watches[@]} | while read file; do \
    if [ $file == $cfg ]; then \
        echo "Config file update detected"; \
        nginx -t "$@"; \
        if [ $? -ne 0 ]; then \
            echo "ERROR: New configuration is invalid!!"; \
        else \
            echo "New configuration is valid, reloading nginx"; \
            nginx -s reload "$@" ; \
        fi; \
    fi; \
  done

  echo "inotifywait failed, killing nginx"

  kill -TERM $nginx_pid
} &

wait $nginx_pid || exit 1
