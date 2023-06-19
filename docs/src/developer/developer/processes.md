# Internal processes

The following processes only apply to Wire employees working on this code base.

## Inspect helm charts before releasing them

`make charts` will create a partial copy of the `./charts` folder under `./.local/charts/` and set helm chart versions as well as docker tags.

`make chart-<chart-name>` can be used to copy & version-pin a single chart before its release.

*See the `CHARTS` variable in the top-level Makefile for the current default list of charts.*

(run-wire-server-integration-tests-inside-kubernetes-using-helm)=

## Run wire-server integration tests inside kubernetes using helm

You need kubectl, helm, and a configured kubernetes cluster

```sh
# for "whatever wire-server source code, usually latest develop, I don't care much"
export DOCKER_TAG=$(./hack/bin/find-latest-docker-tag.sh)
echo "$DOCKER_TAG"
# or, if you wish to test specific wire-server source code, e.g. a particular PR already built by CI:
export DOCKER_TAG=<desired tag used on quay.io.>

# The following can take ~15 minutes and will set up an ephemeral kubernetes namespace and run all integration tests.
make kube-integration
```

When you're done, you can run

```
make kube-integration-teardown
```

## Upload Helm charts to our S3 mirror

Ideally, only CI will do this after commits are merged to develop or master respectively. If this needs to be done manually:

Ensure you are either on develop or master, and your git working tree is clean.

```sh
export HELM_SEMVER=<desired helm version> # must be a semantic version
export DOCKER_TAG=<desired docker tag for docker images of brig, galley, etc>

# Upload all charts inside the makefile list:
make upload-charts

# Upload a single chart, e.g. wire-server
make upload-wire-server
```
