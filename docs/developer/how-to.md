# Developer how-to's

The following assume you have a working developer environment with all the dependencies listed in [./dependencies.md](./dependencies.md) available to you.

<!-- vim-markdown-toc GFM -->

* [How to look at the swagger docs / UI locally](#how-to-look-at-the-swagger-docs--ui-locally)
* [How to run federation tests across two backends](#how-to-run-federation-tests-across-two-backends)
    * [(1) Inspect/change the multi-backend test code](#1-inspectchange-the-multi-backend-test-code)
    * [(2) Decide on code version](#2-decide-on-code-version)
        * [(A) Use the latest compiled code from `develop`](#a-use-the-latest-compiled-code-from-develop)
            * [Troubleshooting](#troubleshooting)
        * [(B) Use code from your pull request](#b-use-code-from-your-pull-request)
        * [(C) Use your local code](#c-use-your-local-code)
    * [(3) Run multi-backend tests](#3-run-multi-backend-tests)
        * [Run all integration tests on kubernetes](#run-all-integration-tests-on-kubernetes)
        * [Run only the multi-backend tests](#run-only-the-multi-backend-tests)

<!-- vim-markdown-toc -->

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

The process consists of:

1. Inspect/change the multi-backend tests
2. Decide on code to use by means of using docker images made available by CI, or making docker images available yourself.
3. Run multi-backend test half-locally half-on-kubernetes or fully on kubernetes

### (1) Inspect/change the multi-backend test code

Refer to `services/brig/test/integration/API/Federation.hs` for the current multi-backend tests.

*Note that they only run if `INTEGRATION_FEDERATION_TESTS` is set to `1`. This is currently configured to be OFF when running regular brig integration tests (e.g. via `make -C services/brig integration`) but is by default ON when running tests on kubernetes or on CI, or when using the `services/brig/federation-tests.sh` script.*

### (2) Decide on code version

Decide which code you would like to use for these tests by setting the `DOCKER_TAG` environment variable. The following options are detailed in the subsections below.

* (A) latest develop
* (B) latest commit on a given PR branch
* (C) local code

#### (A) Use the latest compiled code from `develop`

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

##### Troubleshooting

`make latest-tag` gives different tags for brig and nginz:

* maybe CI hasn't finished, or failed. Look at concourse (`kubernetes-dev` pipeline)

#### (B) Use code from your pull request

*Note: CI already runs multi-backend federation integration tests on your PR, so this section may not be often useful in practice. This is still documented for completeness and to help understand the relation between source code and compiled docker images on CI.*

Check CI for the latest tag that has been created on your PR (expect this to take at least 30-60 minutes from the last time you pushed to your branch). Example:

Look at a successful job in the `wire-server-pr` pipeline from a job build matching your desired PR and commit hash. Then, find the actual docker tag used.

![concourse-pr-version-circled](https://user-images.githubusercontent.com/2112744/114410146-69b34000-9bab-11eb-863c-106fb661ca82.png)

```
# PR 1438 commit 7a183b2dbcf019df1af3d3b97604edac72eca762 translates to
export DOCKER_TAG=0.0.1-pr.3684
```

#### (C) Use your local code

FUTUREWORK: this process is in development (update this section after it's confirmed to work):

1. have `buildah` available on your system
2. compile docker images: `make buildah-docker`. This will take some time the very first time but should be quick on subsequent times.
3. for iterations, e.g. you only work on brig, run `make buildah-docker-brig`
4. Upload images to quay.io by being logged in to quay, setting `BUILDAH_PUSH=1`, and overriding the image tags with an otherwise-unused tag, such as your `$USER`.
5. Run the tests as before by `export DOCKER_TAG=$USER`.

NOTE: debug this process further as some images (e.g. nginz) are missing from the default buildah steps.
* Implement re-tagging development tags as your user tag?
* Force `imagePullPolicy=Always` in this mode?

### (3) Run multi-backend tests

Once you have chosen the code to test and set `DOCKER_TAG` accordingly, run the following, which will create two full installations of wire-server on the kubernetes cluster you've configured to connect to, and should take ~10 minutes.

```
make kube-integration-setup
```

Next, you can choose to either run all integration tests, which also includes running the multi-backend integration tests by default. Or you can instead choose to *only* run the multi-backend tests.

#### Run all integration tests on kubernetes

* takes ~10 minutes to run
* test output is delayed until all tests have run. You will have to scroll the output to find the relevant multi-backend test output.
* tests run entirely on kubernetes.
* includes running the federation multi-backend tests by default (see also section (1))

```
make kube-integration-test
```

#### Run only the multi-backend tests

* runs faster (~ half a minute)
* test output is shown dynamically as tests run
* business logic code runs on kubernetes, but the test executable runs on your local computer (which connects using `telepresence`)

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
