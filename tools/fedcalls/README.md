our swaggger docs contain information about which end-points call
which federation end-points internally.  this command line tool
extracts that information from the swagger json and converts it into a
dot file.

make sure you're running against a brig that actually contains the
relevant tags:

```
curl -q http://localhost:8082/api/swagger.json | jq . | grep x-wire-makes-federated-call-to
```

generate the dot file:

```
curl -q http://localhost:8082/api/swagger.json | ./fedcalls > fedcalls.dot
dot -Tpng /tmp/x.dot > /tmp/x.png
```

further reading and playing:
- `./example.png`
- https://sketchviz.com/new
- https://graphviz.org/doc/info/lang.html
- `/libs/wire-api/src/Wire/API/MakesFederatedCall.hs`
