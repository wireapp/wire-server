#!/usr/bin/env bash
#
# End-to-end test script for entreprise-provisioning CLI tool
#
# This script:
# 1. Creates a team admin user
# 2. Logs in to get an auth token
# 3. Enables channels feature for the team
# 4. Creates user groups
# 5. Runs the CLI tool to create channels and associate them with user groups
# 6. Verifies the results
#
# Prerequisites:
# - Wire server services running (dockerephemeral + local services)
# - Services accessible at localhost on their default ports
# - entreprise-provisioning CLI tool built and in PATH or current directory

set -e

# Configuration
BRIG_INTERNAL="http://localhost:8082"
GALLEY_INTERNAL="http://localhost:8085"
API_URL="${API_URL:-http://localhost:8080}"  # Wire API URL (via nginx)
VERBOSE=${VERBOSE:-1}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() {
  if [ "$VERBOSE" -eq 1 ]; then
    echo -e "${GREEN}[INFO]${NC} $*" >&2
  fi
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $*" >&2
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*" >&2
}

# Generate random string
random_string() {
  local length=${1:-8}
  env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c "$length"
}

# Create a team admin user via Brig internal API
create_team_admin() {
  local email
  local password
  local name
  email="$(random_string)@example.com"
  password="$(random_string 12)"
  name="Test Admin"

  log "Creating team admin user: $email"

  local response
  response=$(curl -s -w "\n%{http_code}" -X POST "$BRIG_INTERNAL/i/users" \
    -H "Content-Type: application/json" \
    -d "{
      \"email\": \"$email\",
      \"password\": \"$password\",
      \"name\": \"$name\",
      \"team\": {
        \"name\": \"Test Team\",
        \"icon\": \"default\"
      }
    }")

  local http_code
  http_code=$(echo "$response" | tail -n1)
  local body
  body=$(echo "$response" | head -n-1)

  if [ "$http_code" != "201" ]; then
    log_error "Failed to create team admin (HTTP $http_code)"
    echo "$body" >&2
    return 1
  fi

  local user_id
  user_id=$(echo "$body" | jq -r '.id')
  local team_id
  team_id=$(echo "$body" | jq -r '.team')

  if [ -z "$user_id" ] || [ "$user_id" = "null" ]; then
    log_error "Failed to extract user ID from response"
    return 1
  fi

  log "Created user: $user_id on team: $team_id"

  # Export for use by caller
  echo "$email|$password|$user_id|$team_id"
}

# Login and get access token
login() {
  local email="$1"
  local password="$2"

  log "Logging in as $email"

  local response
  response=$(curl -s -w "\n%{http_code}" -X POST "$BRIG_INTERNAL/login?persist=true" \
    -H "Content-Type: application/json" \
    -d "{
      \"email\": \"$email\",
      \"password\": \"$password\"
    }")

  local http_code
  http_code=$(echo "$response" | tail -n1)
  local body
  body=$(echo "$response" | head -n-1)

  if [ "$http_code" != "200" ]; then
    log_error "Login failed (HTTP $http_code)"
    echo "$body" >&2
    return 1
  fi

  local access_token
  access_token=$(echo "$body" | jq -r '.access_token')

  if [ -z "$access_token" ] || [ "$access_token" = "null" ]; then
    log_error "Failed to extract access token from response"
    return 1
  fi

  log "Logged in successfully"
  echo "$access_token"
}

# Enable channels feature for a team
enable_channels() {
  local team_id="$1"

  log "Unlocking channels feature for team $team_id"

  # First unlock the feature
  local unlock_response
  unlock_response=$(curl -s -w "\n%{http_code}" -X PUT "$GALLEY_INTERNAL/i/teams/$team_id/features/channels/unlocked")

  local unlock_code
  unlock_code=$(echo "$unlock_response" | tail -n1)
  if [ "$unlock_code" != "200" ]; then
    log_warn "Failed to unlock channels feature (HTTP $unlock_code), continuing anyway..."
  fi

  log "Enabling channels feature for team $team_id"

  # Now enable the feature
  local response
  response=$(curl -s -w "\n%{http_code}" -X PATCH "$GALLEY_INTERNAL/i/teams/$team_id/features/channels" \
    -H "Content-Type: application/json" \
    -d '{"status": "enabled"}')

  local http_code
  http_code=$(echo "$response" | tail -n1)
  local body
  body=$(echo "$response" | head -n-1)

  if [ "$http_code" != "200" ]; then
    log_error "Failed to enable channels feature (HTTP $http_code)"
    echo "$body" >&2
    return 1
  fi

  log "Channels feature enabled"

  # Verify the feature was enabled
  log "Verifying channels feature status..."
  local verify_response
  verify_response=$(curl -s -X GET "$GALLEY_INTERNAL/i/teams/$team_id/features/channels")
  log "Feature status: $verify_response"
}

# Create a user group
create_user_group() {
  local user_id="$1"
  local auth_token="$2"
  local name="$3"
  local members="$4"  # JSON array of user IDs

  log "Creating user group: $name"

  local response
  response=$(curl -s -w "\n%{http_code}" -X POST "$BRIG_INTERNAL/v12/user-groups" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $auth_token" \
    -H "Z-User: $user_id" \
    -d "{
      \"name\": \"$name\",
      \"members\": $members
    }")

  local http_code
  http_code=$(echo "$response" | tail -n1)
  local body
  body=$(echo "$response" | head -n-1)

  if [ "$http_code" != "200" ] && [ "$http_code" != "201" ]; then
    log_error "Failed to create user group (HTTP $http_code)"
    echo "$body" >&2
    return 1
  fi

  local group_id
  group_id=$(echo "$body" | jq -r '.id')

  if [ -z "$group_id" ] || [ "$group_id" = "null" ]; then
    log_error "Failed to extract group ID from response"
    return 1
  fi

  log "Created user group: $group_id"
  echo "$group_id"
}

# Main test flow
main() {
  log "Starting end-to-end test for entreprise-provisioning"
  log ""

  # Step 1: Create team admin
  log "=== Step 1: Creating team admin ==="
  local user_data
  if ! user_data=$(create_team_admin); then
    log_error "Failed to create team admin"
    exit 1
  fi

  local email
  email=$(echo "$user_data" | cut -d'|' -f1)
  local password
  password=$(echo "$user_data" | cut -d'|' -f2)
  local user_id
  user_id=$(echo "$user_data" | cut -d'|' -f3)
  local team_id
  team_id=$(echo "$user_data" | cut -d'|' -f4)

  log ""

  # Step 2: Login
  log "=== Step 2: Logging in ==="
  local auth_token
  if ! auth_token=$(login "$email" "$password"); then
    log_error "Failed to login"
    exit 1
  fi

  log ""

  # Step 3: Enable channels
  log "=== Step 3: Enabling channels feature ==="
  if ! enable_channels "$team_id"; then
    log_error "Failed to enable channels"
    exit 1
  fi

  log ""

  # Step 4: Create user groups
  log "=== Step 4: Creating user groups ==="
  local group1_id
  if ! group1_id=$(create_user_group "$user_id" "$auth_token" "Engineering" "[]"); then
    log_error "Failed to create user group 1"
    exit 1
  fi

  local group2_id
  if ! group2_id=$(create_user_group "$user_id" "$auth_token" "Marketing" "[]"); then
    log_error "Failed to create user group 2"
    exit 1
  fi

  local group3_id
  if ! group3_id=$(create_user_group "$user_id" "$auth_token" "Sales" "[]"); then
    log_error "Failed to create user group 3"
    exit 1
  fi

  log ""

  # Step 5: Create input JSON
  log "=== Step 5: Creating input JSON ==="
  local input_file
  input_file=$(mktemp)
  cat > "$input_file" <<EOF
{
  "$group1_id": [
    "general",
    "backend",
    "frontend"
  ],
  "$group2_id": [
    "general",
    "campaigns"
  ],
  "$group3_id": [
    "general",
    "deals",
    "customers"
  ]
}
EOF

  log "Input file: $input_file"
  if [ "$VERBOSE" -eq 1 ]; then
    log "Contents:"
    cat "$input_file" >&2
  fi

  log ""

  # Step 6: Run CLI tool
  log "=== Step 6: Running entreprise-provisioning CLI ==="
  local output_file
  output_file=$(mktemp)

  log "Running CLI tool via cabal"

  local cli_opts=()
  if [ "$VERBOSE" -eq 1 ]; then
    cli_opts+=("-v")
  fi

  cabal run entreprise-provisioning -- user-groups channels \
    -t "$team_id" \
    -u "$user_id" \
    --api-url "$API_URL" \
    --auth-token "$auth_token" \
    -f "$input_file" \
    "${cli_opts[@]}" \
    > "$output_file" 2>&1

  # Extract only the JSON from the output (skip cabal build messages)
  local json_file
  json_file=$(mktemp)
  tail -n 1 "$output_file" > "$json_file"
  mv "$json_file" "$output_file"

  local cli_exit_code=$?

  if [ $cli_exit_code -ne 0 ]; then
    log_error "CLI tool failed with exit code $cli_exit_code"
    log_error "Output:"
    cat "$output_file" >&2
    rm -f "$input_file" "$output_file"
    exit 1
  fi

  log "CLI tool completed successfully"
  log ""

  # Step 7: Verify results
  log "=== Step 7: Verifying results ==="
  log "Output file: $output_file"

  if ! jq empty "$output_file" 2>/dev/null; then
    log_error "Output is not valid JSON"
    cat "$output_file" >&2
    rm -f "$input_file" "$output_file"
    exit 1
  fi

  log "Output is valid JSON"

  # Check that we have results for all 3 groups
  local result_groups
  result_groups=$(jq -r 'keys[]' "$output_file")
  local result_count
  result_count=$(echo "$result_groups" | wc -l)

  if [ "$result_count" -ne 3 ]; then
    log_error "Expected 3 groups in results, got $result_count"
    rm -f "$input_file" "$output_file"
    exit 1
  fi

  log "All 3 groups present in results"

  # Check for successful channels and associations
  local total_channels
  total_channels=$(jq '[.[] | .channel[]] | length' "$output_file")
  local successful_channels
  successful_channels=$(jq '[.[] | .channel[] | select(.id)] | length' "$output_file")
  local failed_channels
  failed_channels=$(jq '[.[] | .channel[] | select(.failure)] | length' "$output_file")
  local successful_associations
  successful_associations=$(jq '[.[] | .association | select(.success == true)] | length' "$output_file")

  log "Total channels: $total_channels"
  log "Successful channels: $successful_channels"
  log "Failed channels: $failed_channels"
  log "Successful associations: $successful_associations"

  if [ "$successful_channels" -eq 0 ]; then
    log_error "No channels were created successfully"
    log "Full output:"
    jq '.' "$output_file" >&2
    rm -f "$input_file" "$output_file"
    exit 1
  fi

  if [ "$successful_associations" -eq 0 ]; then
    log_error "No associations were successful"
    log "Full output:"
    jq '.' "$output_file" >&2
    rm -f "$input_file" "$output_file"
    exit 1
  fi

  log ""
  log "=== Test Summary ==="
  log "Team ID: $team_id"
  log "User ID: $user_id"
  log "User Groups Created: 3"
  log "- Engineering ($group1_id): 3 channels"
  log "- Marketing ($group2_id): 2 channels"
  log "- Sales ($group3_id): 3 channels"
  log "Total Channels Created: $successful_channels/$total_channels"
  log "Successful Associations: $successful_associations/3"

  if [ "$VERBOSE" -eq 1 ]; then
    log ""
    log "=== Full Results ==="
    jq '.' "$output_file" >&2
  fi

  log ""
  log -e "${GREEN}✓ All tests passed!${NC}"

  # Cleanup
  rm -f "$input_file" "$output_file"
}

# Run main
main "$@"
