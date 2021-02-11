Install python dependencies and generate python files from proto:

```
poetry install
poetry run python -m grpc_tools.protoc -I./proto --python_out=. --grpc_python_out=. proto/router.proto
```

Run services locally:

```
../../services/start-services-only.sh
```

Run example python client to make a grpc request to federator (which will redirect it to brig):

```
poetry run python client.py
```

expected output should be:

```
starting client...
request:  path: "users/by-handle"
query {
  key: "handle"
  value: "alice"
}

response:  httpResponse {
  responseStatus: 404
  responseBody: "Handle not found."
}
```
