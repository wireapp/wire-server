#!/usr/bin/env bash
Echo "Running integration tests ..."
Echo "Note that helm will only print the logs of the tests after they all finish..."
helm test -n fed1 wire-server --logs --timeout=500s
