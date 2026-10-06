# Pinned federation backend fixtures

Local federation tests use pinned backend binaries to verify interoperability
with released Wire Server versions. A pinned fixture must use the service
configuration from the same release as its container images. Do not use the
current development configuration as the baseline for an older binary.

## Choosing the release

Before creating a fixture:

1. Identify the release in which the federation API version was finalized.
2. Treat that finalized release as the stable baseline for the version.
3. Record the exact chart tag and image tag used by the fixture.

The tag is the source of truth for the service configuration. Create a
temporary worktree at that tag, for example:

```bash
git worktree add --detach ../wire-server-chart-<release> chart/<release>
```

## Building the fixture

Use the tagged service integration configurations as the baseline. The usual
inputs are:

```text
services/cargohold/cargohold.integration.yaml
services/proxy/proxy.integration.yaml
services/brig/brig.integration.yaml
services/spar/spar.integration.yaml
services/background-worker/background-worker.integration.yaml
services/gundeck/gundeck.integration.yaml
services/federator/federator.integration.yaml
services/galley/galley.integration.yaml
services/cannon/cannon.integration.yaml
```

Use the configurations for the services that are started by the local
federation Compose fixture. Other integration configurations in the release
tag are not part of this fixture.

Adapt the tagged configurations for the local Docker environment only:

- container hostnames and ports;
- unique Cassandra keyspaces and database names;
- RabbitMQ vhosts and queue names;
- local AWS-compatible service endpoints;
- local certificate, key, and secret paths;
- federation domains and DNS records;
- Redis topology and connection mode.

Do not add configuration introduced after the selected release merely because
it exists in the current development configuration.

## Registering the version

Update all shared local-test wiring for the new version:

- add the `federation-vN.yaml` Compose overlay and its configuration directory;
- add the version to `deploy/dockerephemeral/run.sh`;
- create its RabbitMQ vhost and queues;
- add required DynamoDB, S3, SES, SNS, and SQS resources;
- add DNS SRV records for the new version to every CoreDNS fixture;
- add the backend and RabbitMQ settings to `services/integration.yaml`;
- add the domain and queue cleanup handling to the integration test harness;
- include the version in the parameterized federation tests.

The version should be optional through its `ENABLE_FEDERATION_VN` environment
variable, so tests can run against one pinned version or several versions.

## Validation checklist

Run the following checks before running the integration tests:

```bash
docker compose \
  -f deploy/dockerephemeral/docker-compose.yaml \
  -f deploy/dockerephemeral/federation-v0.yaml \
  -f deploy/dockerephemeral/federation-v1.yaml \
  -f deploy/dockerephemeral/federation-v2.yaml \
  -f deploy/dockerephemeral/federation-vN.yaml \
  config --quiet

bash -n deploy/dockerephemeral/run.sh
sh -n deploy/dockerephemeral/init.sh deploy/dockerephemeral/init_vhosts.sh

git diff --check
```

Also verify that the new fixture has no stale identifiers from the source
version, all images use the selected release tag, all DNS records and queues
are present, and every enabled container becomes healthy.

Finally, run the federation API-version and cross-backend smoke test with only
the new version enabled, then run the broader federation use-case tests with
the required version combinations.

## Starting and testing one pinned version

Replace `3` below with the pinned federation version being tested:

```bash
ENABLE_FEDERATION_V3=1 ./deploy/dockerephemeral/run.sh
```

After the containers are healthy, run the API-version and cross-backend smoke
test:

```bash
ENABLE_FEDERATION_V3=1 \
TEST_INCLUDE=testFederationAPIVersionLegacySmoke \
make ci-safe package=integration
```
