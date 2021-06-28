Install python dependencies and generate python files from proto:

```
poetry install
poetry run python -m grpc_tools.protoc -I../../../libs/wire-api-federation/proto --python_out=. --grpc_python_out=. ../../../libs/wire-api-federation/proto/router.proto
```

Run services locally:

```
export INTEGRATION_USE_NGINZ=1
../../../services/start-services-only.sh
```

Run example python client to make a grpc request to federator (which will redirect it to brig):

```
poetry run python client.py
```

expected output should be:

```
starting client...
request:  path: "federation/get-user-by-handle"
body: "\"pyaewrqxggbtbvzdsubkl\""
originDomain: "python.example.com"

json reponse:
 {
  "handle": "pyaewrqxggbtbvzdsubkl",
  "qualified_id": {
    "domain": "example.com",
    "id": "0f3880e6-5fb5-4c3d-963d-8ea36e15717c"
  },
  "accent_id": 0,
  "picture": [],
  "legalhold_status": "no_consent",
  "name": "\u7985\u9a71\ud4d1\u7485",
  "id": "0f3880e6-5fb5-4c3d-963d-8ea36e15717c",
  "assets": []
}
```
