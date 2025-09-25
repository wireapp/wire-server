# End-to-End Testing

This directory contains an end-to-end test script (`test-e2e.sh`) that validates the complete workflow of the `entreprise-provisioning` CLI tool.

## Test Script Overview

The `test-e2e.sh` script performs a complete end-to-end test:

1. **Creates a team admin user** via Brig's internal API (`/i/users`)
2. **Logs in** to obtain an access token (`POST /login`)
3. **Enables the channels feature** for the team via Galley's internal API (`PATCH /i/teams/{tid}/features/channels`)
4. **Creates user groups** via Brig's API (`POST /v12/user-groups`)
5. **Runs the CLI tool** with a generated input file
6. **Verifies the results** by parsing the JSON output

## Prerequisites

### 1. Infrastructure Services (Docker)

Start the infrastructure services (databases, queues, etc.):

```bash
cd deploy/dockerephemeral
./run.sh
```

This starts:
- DynamoDB (port 4567)
- SQS (port 4568)
- S3/MinIO (port 4570)
- Cassandra (port 9042)
- Elasticsearch (port 9200)
- PostgreSQL (port 5432)
- RabbitMQ (ports 5671, 15671, 15672)
- Redis (port 6379)
- And more...

### 2. Wire Server Services

The Wire server services (brig, galley, etc.) need to be running on their default ports:

- **Brig**: localhost:8082
- **Galley**: localhost:8085
- **Cannon**: localhost:8083
- **Cargohold**: localhost:8084
- **Gundeck**: localhost:8086
- **Spar**: localhost:8088
- **Nginz**: localhost:8080

You can start these services using one of the following methods:

#### Option A: Using integration test setup

```bash
# From the wire-server root directory
make services
```

#### Option B: Using cabal directly

Start each service in a separate terminal:

```bash
# Terminal 1: Brig
cabal run brig -- -c services/brig/brig.integration.yaml

# Terminal 2: Galley
cabal run galley -- -c services/galley/galley.integration.yaml

# Terminal 3: Cannon (optional for this test)
cabal run cannon -- -c services/cannon/cannon.integration.yaml

# etc...
```

#### Option C: Using the integration test runner

The integration test infrastructure automatically starts services:

```bash
# This will start all services needed for integration tests
make integration
```

### 3. Build the CLI Tool

```bash
cd tools/entreprise-provisioning
cabal build
```

## Running the Test

Once all prerequisites are met:

```bash
cd tools/entreprise-provisioning
./test-e2e.sh
```

### Verbose Output

Enable verbose output to see detailed progress:

```bash
VERBOSE=1 ./test-e2e.sh
```

### Custom Service URLs

Override the default service URLs:

```bash
BRIG_INTERNAL=http://localhost:8082 \
GALLEY_INTERNAL=http://localhost:8085 \
./test-e2e.sh
```

## Expected Output

When successful, you should see:

```
[INFO] Starting end-to-end test for entreprise-provisioning

[INFO] === Step 1: Creating team admin ===
[INFO] Creating team admin user: abc12345@example.com
[INFO] Created user: <user-uuid> on team: <team-uuid>

[INFO] === Step 2: Logging in ===
[INFO] Logging in as abc12345@example.com
[INFO] Logged in successfully

[INFO] === Step 3: Enabling channels feature ===
[INFO] Enabling channels feature for team <team-uuid>
[INFO] Channels feature enabled

[INFO] === Step 4: Creating user groups ===
[INFO] Creating user group: Engineering
[INFO] Created user group: <group1-uuid>
[INFO] Creating user group: Marketing
[INFO] Created user group: <group2-uuid>
[INFO] Creating user group: Sales
[INFO] Created user group: <group3-uuid>

[INFO] === Step 5: Creating input JSON ===
[INFO] Input file: /tmp/tmp.XXXXXXXXXX

[INFO] === Step 6: Running entreprise-provisioning CLI ===
[INFO] Using CLI binary: <path-to-binary>
[INFO] CLI tool completed successfully

[INFO] === Step 7: Verifying results ===
[INFO] Output is valid JSON
[INFO] All 3 groups present in results
[INFO] Total channels: 8
[INFO] Successful channels: 8
[INFO] Failed channels: 0
[INFO] Successful associations: 3

[INFO] === Test Summary ===
[INFO] Team ID: <team-uuid>
[INFO] User ID: <user-uuid>
[INFO] User Groups Created: 3
[INFO] - Engineering (<group1-uuid>): 3 channels
[INFO] - Marketing (<group2-uuid>): 2 channels
[INFO] - Sales (<group3-uuid>): 3 channels
[INFO] Total Channels Created: 8/8
[INFO] Successful Associations: 3/3

[INFO] ✓ All tests passed!
```

## Test Scenario

The test creates the following scenario:

### User Groups and Channels

1. **Engineering** group with 3 channels:
   - general
   - backend
   - frontend

2. **Marketing** group with 2 channels:
   - general
   - campaigns

3. **Sales** group with 3 channels:
   - general
   - deals
   - customers

Total: 3 user groups, 8 channels

## Troubleshooting

### Services Not Running

If you see:
```
[ERROR] Failed to create team admin (HTTP XXX)
```

Check that the infrastructure and Wire services are running:

```bash
# Check Brig
curl http://localhost:8082/i/status

# Check Galley
curl http://localhost:8085/i/status
```

### CLI Binary Not Found

If you see:
```
[ERROR] Cannot find entreprise-provisioning binary
```

Build the CLI tool:

```bash
cd tools/entreprise-provisioning
cabal build
```

### Feature Not Available

If channels feature cannot be enabled, it might not be available in your Wire server version. Check that you're running a version that supports channels.

### JSON Parsing Errors

If the output contains parsing errors, ensure you have `jq` installed:

```bash
# Ubuntu/Debian
sudo apt-get install jq

# macOS
brew install jq

# Arch Linux
sudo pacman -S jq
```

## Manual Testing

You can also test the CLI manually after creating a team and user groups:

1. Create a team admin using the helper script:

```bash
../../hack/bin/create_test_team_admins.sh -n 1
```

This outputs: `User-Id,Email,Password`

2. Login to get a token (you'll need to implement a login script or use the Wire client)

3. Enable channels via API:

```bash
curl -X PATCH "http://localhost:8085/i/teams/$TEAM_ID/features/channels" \
  -H "Content-Type: application/json" \
  -d '{"status": "enabled"}'
```

4. Create user groups via API:

```bash
curl -X POST "http://localhost:8082/v12/user-groups" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"name": "Engineering", "members": []}'
```

5. Create an input JSON file:

```json
{
  "group-uuid-1": ["general", "backend", "frontend"],
  "group-uuid-2": ["general", "campaigns"]
}
```

6. Run the CLI:

```bash
cabal run entreprise-provisioning -- user-groups channels \
  -t "$TEAM_ID" \
  --galley-url "http://localhost:8085" \
  --brig-url "http://localhost:8082" \
  --auth-token "$TOKEN" \
  -f input.json \
  -v
```

## Integration with CI/CD

To integrate this test into your CI/CD pipeline:

```bash
# Start infrastructure
cd deploy/dockerephemeral && ./run.sh &
sleep 30  # Wait for services to be ready

# Start Wire services (implementation depends on your setup)
make services &
sleep 10

# Run the test
cd tools/entreprise-provisioning
./test-e2e.sh

# Cleanup
pkill -f wire-server
docker-compose -f deploy/dockerephemeral/docker-compose.yaml down
```

## See Also

- [README.md](README.md) - Main CLI tool documentation
- [integration/test/Test/UserGroup.hs](../../integration/test/Test/UserGroup.hs) - Integration tests for user groups
- [services/integration.yaml](../../services/integration.yaml) - Service port configuration
