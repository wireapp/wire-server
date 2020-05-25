# Running integration tests using Kubernetes

## Locally

Make sure you have docker containers of your local build. in the top-level directory run:
```
make docker-services
```

Set up a local `kind` cluster. This will automatically set the `context` of
`kubectl` to point to the local cluster
```
./setup-kind.sh
```

Now follow the instructions for *Real cluster* as from here on they're
identical


## Real cluster

Make sure you are pointed to the right cluster
```
$ kubectl config current-context
$ kubectl cluster-info
```

Now set up the (in-memory) backing services:
```
./setup-backing-services.sh
```

After that, install `wire-server`.
```
./setup-wire-server.sh
```

Note that you can rerun this script after a `make docker-services`


## Locally but only with backing services in Kubernetes

This is useful when doing interactive development. Not implemented yet.
