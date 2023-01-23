# Developer how-to's

The following assume you have a working developer environment with all the dependencies listed in [./dependencies.md](./dependencies.md) available to you.

If you want to deploy to the CI kubernetes cluster (how-tos below), you need to set the `KUBECONFIG` env var, where `$cailleach_repo` is replaced by your local checkout of the `cailleach` repository.
```
export KUBECONFIG=$cailleach_repo/environments/kube-ci/kubeconfig.dec
```
Check that this file exists by running `ls $KUBECONFIG`.


## How to look at the swagger docs / UI locally

Terminal 1:
* Set up backing services: `./deploy/dockerephemeral/run.sh`

Terminal 2:
* Compile all services: `make c`
* Run services including nginz: `./services/start-services-only.sh`.

Open your browser at:

- [http://localhost:8080/api/swagger-ui](http://localhost:8080/api/swagger-ui) for the swagger 2.0 endpoints (in development as of Feb 2021 - more endpoints will be added here as time goes on)
- [http://localhost:8080/swagger-ui](http://localhost:8080/swagger-ui) for the old swagger 1.2 API (old swagger, endpoints will disappear from here (and become available in the previous link) as time progresses). Run `make -C services/nginz integration-test/conf/nginz/zwagger-ui` once to get JS libraries needed (they are not included in the repo).

Swagger json (for swagger 2.0 endpoints) is available under [http://localhost:8080/api/swagger.json](http://localhost:8080/api/swagger.json)

## How to run federation tests across two backends

Requirements:

* `helm` and `kubectl` available on your PATH
* access to a kubernetes cluster via e.g. a `~/.kube/config` file.

The process consists of:

1. Inspect/change the multi-backend tests
2. Deploy two backends to kubernetes cluster
3. Run multi-backend test half-locally half-on-kubernetes or fully on kubernetes
4. Teardown

### 1. Inspect/change the multi-backend test code

Refer to `services/brig/test/integration/API/Federation/End2End.hs` for the current multi-backend tests.

*Note that they only run if `INTEGRATION_FEDERATION_TESTS` is set to `1`. This is currently configured to be OFF when running regular brig integration tests (e.g. via `make -C services/brig integration`) but is by default ON when running tests on kubernetes or on CI, or when using the `services/brig/federation-tests.sh` script.*

### 2. Deploy two backends to kubernetes cluster

Decide which code you would like to deploy. The following options are detailed in the subsections below.

* 2.1 Deploy the the latest compiled code from `develop`
* 2.2 Deploy code from your pull request
* 2.3 Deploy your local code to a kind cluster

#### 2.1 Deploy the the latest compiled code from `develop`

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
export NAMESPACE="myname"
make kube-integration-setup
```

This will create two full installations of wire-server on the kubernetes cluster you've configured to connect to, and should take ~10 minutes. The namespaces will be `$NAMESPACE` and `$NAMESPACE-fed2`.


##### Troubleshooting

`make latest-tag` gives different tags for brig and nginz:

* maybe CI hasn't finished, or failed. Look at concourse (`kubernetes-dev` pipeline)

#### 2.2 Deploy code from your pull request

*Note: CI already runs multi-backend federation integration tests on your PR, so this section may not be often useful in practice. This is still documented for completeness and to help understand the relation between source code and compiled docker images on CI.*

Check CI for the latest tag that has been created on your PR (expect this to take at least 30-60 minutes from the last time you pushed to your branch). Example:

Look at a successful job in the `wire-server-pr` pipeline from a job bruild matching your desired PR and commit hash. Then, find the actual docker tag used.

![concourse-pr-version-circled](https://user-images.githubusercontent.com/2112744/114410146-69b34000-9bab-11eb-863c-106fb661ca82.png)

```
# PR 1438 commit 7a183b2dbcf019df1af3d3b97604edac72eca762 translates to
export DOCKER_TAG=0.0.1-pr.3684
export NAMESPACE="myname"
make kube-integration-setup
```

#### 2.3 Deploy your local code to a kind cluster

This can be useful to get quicker feedback while working on multi-backend code or configuration (e.g. helm charts) than to wait an hour for CI. This allows you to test code without uploading it to github and waiting an hour for CI.

FUTUREWORK: this process is in development (update this section after it's confirmed to work):

##### Run tests in kind

0. Create a local kind cluster with `make kind-cluster`
1. Upload images in docker-daemon running inside kind with `make kind-upload-images`

   *Note:* First time all the images need to be uploaded. When working on one
   service it can be selectively uploaded using `make kind-upload-image-<name>`
   (e.g. `make kind-upload-image-brig`).
2. Install wire-server using `make kind-integration-setup`.
3. Run tests using `make kind-integration-test`.
4. Run end2end integration tests: `make kind-integration-e2e`.



#### 2.4 Deploy your local code to a kubernetes cluster

This sections describes how partially update a release with a local build of a service, in this example `brig`.

Start by deploying a published release (see 2.1 or 2.2).

```
export NAMESPACE=$USER
export DOCKER_TAG=2.116.32
make kube-integration-setup
```

Then build and push the `brig` image by running

```
#FUTUREWORK
```

To update the release with brig's local image run
```
./hack/bin/set-chart-image-version.sh "$DOCKER_TAG_LOCAL_BUILD" brig
./hack/bin/integration-setup-federation.sh
```


## 3 Run multi-backend tests

### Run all integration tests on kubernetes

* takes ~10 minutes to run
* test output is delayed until all tests have run. You will have to scroll the output to find the relevant multi-backend test output.
* tests run entirely on kubernetes.
* includes running the federation multi-backend tests by default (see also section (1))

```
make kube-integration-test
```

### Run only the multi-backend tests

* runs faster (~ half a minute)
* test output is shown dynamically as tests run
* business logic code runs on kubernetes, but the test executable runs on your local computer (which connects using `telepresence`)

1. ensure you have compiled brig-integration: `make -C services/brig fast`
2. ensure you have `telepresence` installed (see developer dependencies documentation)
3. Run the actual tests, (takes half a minute):

```
./services/brig/federation-tests.sh "$NAMESPACE"
```

Note that this runs your *locally* compiled `brig-integration`, so this allows to easily change test code locally with the following process:

1. change code under `services/brig/test/integration/Federation/`
2. recompile: `make -C services/brig fast`
3. run `./services/brig/federation-tests.sh test-$USER` again.

### Run selected integration tests on kuberentes

To run selective tests from brig-integration:

```
helm -n $NAMESPACE get hooks $NAMESPACE-wire-server | yq '.' | jq -r 'select(.metadata.name | contains("brig-integration"))' > /tmp/integration-pod

# optional: edit the test pattern /tmp/integration-pod

kubectl apply -n $NAMESPACE -f /tmp/integration-pod
```

## 4 Teardown

To destroy all the resources on the kubernetes cluster that have been created run

```
./hack/bin/integration-teardown-federation.sh
```

Note: Simply deleting the namespaces is insufficient, because it leaves some resources (of kind ClusterRole, ClusterRoleBinding) that cause problems when redeploying to the same namespace via helm.
