# entreprise-provisioning

A CLI tool for bulk provisioning user groups with channels in Wire.

## Features

- Create multiple channels for a team
- Associate channels with user groups in bulk
- Comprehensive error reporting with detailed JSON output
- Environment variable support for API URL and authentication token
- Verbose logging for debugging
- Built-in environment variable help with `env info` command

## Building

```bash
cd tools/entreprise-provisioning
cabal build
```

## Usage

```bash
entreprise-provisioning user-groups channels \
  -t TEAM_ID \
  --galley-url GALLEY_URL \
  --brig-url BRIG_URL \
  --auth-token TOKEN \
  -f INPUT_FILE \
  [-v]
```

### Arguments

- `-t, --team-id TEAM_ID`: Team ID (UUID format)
- `--galley-url GALLEY_URL`: Galley service URL (e.g., `https://prod-nginz-https.wire.com`)
- `--brig-url BRIG_URL`: Brig service URL (e.g., `https://prod-nginz-https.wire.com`)
- `-f, --file FILENAME`: Path to input JSON file
- `--auth-token TOKEN`: Authentication token
- `-v, --verbose`: Enable verbose output to stderr

### Environment Variables

Service URLs and authentication token can be provided via environment variables. When set, these become the default values for their respective CLI arguments, making them optional:

```bash
export WIRE_GALLEY_URL="https://prod-nginz-https.wire.com"
export WIRE_BRIG_URL="https://prod-nginz-https.wire.com"
export WIRE_AUTH_TOKEN="your-bearer-token"
```

To view environment variable documentation:

```bash
entreprise-provisioning env info
```

**Configuration Priority:**
1. Command line arguments (highest priority)
2. Environment variables
3. No default (required if not set)

## Input Format

The input file should be a JSON object mapping user group IDs to arrays of channel names:

```json
{
  "user-group-id-0": ["channel name 0", "channel name 1", "channel name 2"],
  "user-group-id-1": ["channel name 0", "channel name 1"],
  "user-group-id-2": ["channel name 0", "channel name 1", "channel name 2"],
  "user-group-id-3": ["channel name 0"]
}
```

## Output Format

The tool outputs a JSON object to stdout with the following structure:

```json
{
  "user-group-id-1": {
    "channel": [
      {
        "name": "channel name 0",
        "id": "conversation-id-uuid"
      },
      {
        "name": "channel name 1",
        "id": "conversation-id-uuid"
      }
    ],
    "association": {
      "success": true
    }
  },
  "user-group-id-2": {
    "channel": [
      {
        "name": "channel name 0",
        "id": "conversation-id-uuid"
      },
      {
        "name": "channel name 1",
        "failure": {
          "status": 401,
          "response": {"label": "access-denied"}
        }
      }
    ],
    "association": {
      "success": false,
      "detail": {
        "status": 403,
        "response": {"label": "operation-denied"}
      }
    }
  }
}
```

### Success Cases

- **Channel creation success**: Returns channel `name` and `id`
- **Association success**: Returns `"success": true`

### Error Cases

- **Channel creation failure**: Returns channel `name` and `failure` object with `status` code and `response` body
- **Association failure**: Returns `"success": false` with `detail` object containing `status` code and `response` body

## Examples

### View Environment Variable Help

```bash
entreprise-provisioning env info
```

Output:
```
Environment Variables:

  WIRE_GALLEY_URL    Galley service URL
                     Used as default for --galley-url if set

  WIRE_BRIG_URL      Brig service URL
                     Used as default for --brig-url if set

  WIRE_AUTH_TOKEN    Authentication token
                     Used as default for --auth-token if set

Example:
  export WIRE_GALLEY_URL=https://prod-nginz-https.wire.com
  export WIRE_BRIG_URL=https://prod-nginz-https.wire.com
  export WIRE_AUTH_TOKEN=your-token-here
```

### Basic Usage (All CLI Arguments)

```bash
entreprise-provisioning user-groups channels \
  -t "3fa85f64-5717-4562-b3fc-2c963f66afa6" \
  --galley-url "https://prod-nginz-https.wire.com" \
  --brig-url "https://prod-nginz-https.wire.com" \
  --auth-token "your-token-here" \
  -f input.json
```

### With Environment Variables

```bash
export WIRE_GALLEY_URL="https://prod-nginz-https.wire.com"
export WIRE_BRIG_URL="https://prod-nginz-https.wire.com"
export WIRE_AUTH_TOKEN="your-token-here"

# Now all service URLs and auth token are optional
entreprise-provisioning user-groups channels \
  -t "3fa85f64-5717-4562-b3fc-2c963f66afa6" \
  -f input.json
```

### With Verbose Logging

```bash
entreprise-provisioning user-groups channels \
  -t "3fa85f64-5717-4562-b3fc-2c963f66afa6" \
  --galley-url "https://prod-nginz-https.wire.com" \
  --brig-url "https://prod-nginz-https.wire.com" \
  --auth-token "your-token-here" \
  -f input.json \
  -v 2> debug.log
```

### Output to File

```bash
entreprise-provisioning user-groups channels \
  -t "3fa85f64-5717-4562-b3fc-2c963f66afa6" \
  --galley-url "https://prod-nginz-https.wire.com" \
  --brig-url "https://prod-nginz-https.wire.com" \
  --auth-token "your-token-here" \
  -f input.json \
  > results.json
```

## Requirements

- The authenticated user must be a team admin
- The team must have channels feature enabled
- User group IDs must exist and belong to the specified team
- API version v12 or higher is required

## Error Handling

The tool continues processing even if individual operations fail:

- If a channel creation fails, it's recorded in the output and processing continues with the next channel
- If some channels succeed and others fail, the tool will attempt to associate the successful ones
- If association fails, the error is recorded in the output

## Troubleshooting

### Authentication Errors

- **Error**: `No authentication token provided`
  - **Solution**: Provide token via `--auth-token` argument or `WIRE_AUTH_TOKEN` environment variable

- **Error**: `401 Unauthorized`
  - **Solution**: Verify your authentication token is valid and not expired

### Configuration Errors

- **Error**: Missing `--galley-url` argument
  - **Solution**: Provide via `--galley-url` argument or set `WIRE_GALLEY_URL` environment variable

- **Error**: Missing `--brig-url` argument
  - **Solution**: Provide via `--brig-url` argument or set `WIRE_BRIG_URL` environment variable

- **Error**: Missing `--auth-token` argument
  - **Solution**: Provide via `--auth-token` argument or set `WIRE_AUTH_TOKEN` environment variable

### Permission Errors

- **Error**: `403 Forbidden` on channel creation
  - **Solution**: Ensure the user is a team admin and has permission to create channels

- **Error**: `403 Forbidden` on association
  - **Solution**: Verify the user has permission to manage user groups

### API Errors

- **Error**: `404 Not Found` on user group
  - **Solution**: Verify the user group ID exists and belongs to the team

- **Error**: `channels-not-enabled`
  - **Solution**: Enable the channels feature for the team

## Testing

### Automated End-to-End Test

An automated end-to-end test script is provided to verify the complete workflow:

```bash
./test-e2e.sh
```

This script:
1. Creates a team and admin user
2. Enables the channels feature
3. Creates test user groups
4. Runs the CLI tool
5. Verifies the results

For detailed testing documentation, see [TEST.md](TEST.md).
