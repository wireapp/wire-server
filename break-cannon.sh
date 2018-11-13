#!/bin/bash

# this fails:
curl http://localhost:8083/i/bulkpush -XPOST -H "content-type: application/json" -d@./break-cannon.json

# this works fine:
curl http://localhost:8083/i/bulkpush --http1.0 -XPOST -H "content-type: application/json" -d@./break-cannon.json
