# Developer how-to's

The following assume you have a working developer environment with all the dependencies listed in [./dependencies.md](./dependencies.md) available to you.

## How to look at the swagger docs / UI locally

Terminal 1:
* Set up backing services: `./deploy/dockerephemeral/run.sh`

Terminal 2:
* Compile all services: `make services`
* Run services including nginz: `export INTEGRATION_USE_NGINZ=1; ./services/start-services-only.sh`

Open your browser at:

- http://localhost:8080/api/swagger-ui for the swagger 2.0 endpoints (in development as of Feb 2021 - more endpoints will be added here as time goes on)
- http://localhost:8080/swagger-ui/ for the old swagger 1.2 API (old swagger, endpoints will disappear from here (and become available in the previous link) as time progresses)

Swagger json (for swagger 2.0 endpoints) is available under http://localhost:8080/api/swagger.json

## How to run federation tests across two backends

Requirements:

* `helm` and `kubectl` available on your PATH
* access to a kubernetes cluster via e.g. a `~/.kube/config` file.

Using the latest code on the `develop` branch:

First, find the latest CI-compiled code made available as docker images:

```
# Run all commands from the top directory of wire-server
make latest-tag
```

Output might be

```
./hack/bin/find-latest-docker-tag.sh
latest tag for brig:
2.104.11
latest tag for nginz:
2.104.11
```

Let's assume the tags are the same(*) for both, then export an environment variable:

```
export DOCKER_TAG=2.104.11
```

Now run the following, which will create two full installations of wire-server on the kubernetes cluster you've configured to connect to, and should take ~10 minutes.

```
make kube-integration-setup
```

Next, you can choose to either run all integration tests, which includes running the multi-backend integration tests, and takes another ~10 minutes. Or you can instead choose to *only* run the multi-backend tests.

Run all integration tests (takes ~10 minutes) with: `make kube-integration-tests` - these tests run entirely on kubernetes.

Run only the multi-backend tests:

1. ensure you have compiled brig-integration: `make -C services/brig fast`
2. ensure you have `telepresence` installed (see developer dependencies documentation)
3. Run the actual tests, (takes half a minute): 

```
./services/brig/federation-tests.sh test-$USER
```

Note that this runs your *locally* compiled `brig-integration`, so this allows to easily change test code locally with the following process:

1. change code under `services/brig/test/integration/Federation/`
2. recompile: `make -C services/brig fast`
3. run `./services/brig/federation-tests.sh test-$USER` again.

For a development process on multi-backend tests where you can easily change the main code and test it:

TODO this process is in development (update this section after it's confirmed to work):

1. have `buildah` available on your system
2. compile docker images: `make buildah-docker`. This will take some time the very first time but should be quick on subsequent times.
3. for iterations, e.g. you only work on brig, run `make buildah-docker-brig`
4. TODO upload images by setting `BUILDAH_PUSH=1`
5. TODO debug this procress further as some images (e.g. nginz) are missing. Implement re-tagging development tags as your user tag? TODO force `imagePullPolicy=Always` ?

## Troubleshooting

`make latest-tag` gives different tags for brig and nginz:

* maybe CI hasn't finished, or failed. Look at concourse (`kubernetes-dev` pipeline)
