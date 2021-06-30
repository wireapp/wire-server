import grpc
import json

from router_pb2 import *
import router_pb2_grpc

# make sure to run ./services/start-services-only.sh first to have federator and brig up!
print("starting client...")

channel = grpc.insecure_channel('localhost:8098') # public-facing federator port
stub = router_pb2_grpc.InwardStub(channel)

def handle_search(handle):
    return Request(
            path="federation/get-user-by-handle".encode("utf-8"),
            body=('"' + handle + '"').encode("utf-8"),
            originDomain="python.example.com",
            component=Brig)

req = handle_search("pyaewrqxggbtbvzdsubkl")

print("request: ", req)

response = stub.call(req)
try:
    jsonresponse = json.dumps(json.loads(response.body.decode('utf-8')), indent=2)
    print("json reponse:\n", jsonresponse)
except:
    print("non-json response:\n", response)
