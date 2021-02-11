import grpc

from router_pb2 import *
import router_pb2_grpc

# make sure to run ./services/start-services-only.sh first to have federator and brig up!
print("starting client...")

channel = grpc.insecure_channel('localhost:8098') # public-facing federator port
stub = router_pb2_grpc.RouteToInternalStub(channel)

def handle_search(handle):
    param = QueryParam(key="handle".encode("utf-8"), value=handle.encode("utf-8"))
    return LocalCall(
            path="users/by-handle".encode("utf-8"),
            query=[param],
            method=GET,
            component=Brig)

req = handle_search("alice")

print("request: ", req)

response = stub.call(req)

print("response: ", response)
