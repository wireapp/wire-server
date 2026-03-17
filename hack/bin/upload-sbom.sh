#!/usr/bin/env bash

# Upload a SBOM file and ensure the project structure exists.
#
# The Dependency Track API is described here (openapi/swagger):
# https://editor.swagger.io/?url=https://deptrack.wire.link/api/openapi.json


set -euo pipefail

# Constants
API_BASE="https://deptrack.wire.link/api/v1"

# Parse arguments
SBOM_FILE="${1:-}"
SOURCE_NAME="${2:-}"
PARENT_PROJECT_NAME="${3:-}"
PARENT_PROJECT_VERSION="${4:-}"
CHART_NAME="${5:-}"

if [[ -z "$SBOM_FILE" ]] || [[ -z "$SOURCE_NAME" ]] || [[ -z "$PARENT_PROJECT_NAME" ]] || [[ -z "$PARENT_PROJECT_VERSION" ]]; then
  echo "Usage: $0 <sbom-file> <source-name> <parent-project-name> <parent-project-version> [chart-name]"
  echo "  sbom-file: Path to SBOM JSON file to upload"
  echo "  source-name: Source type (e.g., 'Helm charts', 'helmfile', 'docker-compose')"
  echo "  parent-project-name: Parent project name (required)"
  echo "  parent-project-version: Parent project version (required)"
  echo "  chart-name: Chart name for Helm charts (optional, will prefix project name)"
  echo ""
  echo "Environment variables:"
  echo "  DEPENDENCY_TRACK_API_KEY: API key for Dependency Track (required)"
  exit 1
fi

if [[ ! -f "$SBOM_FILE" ]]; then
  echo "ERROR: SBOM file not found: $SBOM_FILE" >&2
  exit 1
fi

if [[ -z "${DEPENDENCY_TRACK_API_KEY:-}" ]]; then
  echo "ERROR: DEPENDENCY_TRACK_API_KEY environment variable not set" >&2
  exit 1
fi

# ============================================================================
# Functions
# ============================================================================

# Lookup a project by name and version
# Returns: project JSON if found, empty string otherwise
lookup_project() {
  local name="$1"
  local version="$2"

  local response
  response=$(curl -s -w '\n%{http_code}' -X GET \
    "${API_BASE}/project/lookup?name=$(printf %s "$name" | jq -sRr @uri)&version=$(printf %s "$version" | jq -sRr @uri)" \
    -H "X-Api-Key: $DEPENDENCY_TRACK_API_KEY")

  local status="${response##*$'\n'}"
  local body="${response%$'\n'*}"

  if [[ "$status" == "200" ]]; then
    local uuid
    uuid=$(echo "$body" | jq -r '.uuid // empty')
    if [[ -n "$uuid" ]]; then
      echo "$body"
      return 0
    fi
  fi

  return 1
}

# Create a new project
# Args: name, version, parent_uuid (optional)
# Returns: project UUID
create_project() {
  local name="$1"
  local version="$2"
  # No $parent_uuid means: Top-level project
  local parent_uuid="${3:-}"

  local payload
  if [[ -n "$parent_uuid" ]]; then
    payload=$(jq -n \
      --arg name "$name" \
      --arg version "$version" \
      --arg parentUuid "$parent_uuid" \
      '{
        name: $name,
        version: $version,
        classifier: "APPLICATION",
        collectionLogic: "AGGREGATE_DIRECT_CHILDREN",
        parent: {uuid: $parentUuid},
        active: true
      }')
  else
    payload=$(jq -n \
      --arg name "$name" \
      --arg version "$version" \
      '{
        name: $name,
        version: $version,
        classifier: "APPLICATION",
        collectionLogic: "AGGREGATE_DIRECT_CHILDREN",
        active: true
      }')
  fi

  local response
  response=$(curl -s -w '\n%{http_code}' -X PUT "${API_BASE}/project" \
    -H "X-Api-Key: $DEPENDENCY_TRACK_API_KEY" \
    -H "Content-Type: application/json" \
    -d "$payload")

  local status="${response##*$'\n'}"
  local body="${response%$'\n'*}"

  if [[ "$status" == "201" ]]; then
    local uuid
    uuid=$(echo "$body" | jq -r '.uuid // empty')
    if [[ -n "$uuid" ]]; then
      echo "$uuid"
      return 0
    fi
  fi

  echo "ERROR: Failed to create project (HTTP $status)" >&2
  echo "$body" >&2
  return 1
}

# Check if project exists, create if not
# Args: name, version, parent_uuid (optional), description
# Returns: project UUID (to stdout)
check_or_create_project() {
  local name="$1"
  local version="$2"
  local parent_uuid="${3:-}"
  local description="$4"

  echo "Checking $description..." >&2

  local project
  if project=$(lookup_project "$name" "$version"); then
    local uuid
    uuid=$(echo "$project" | jq -r '.uuid')
    echo "✓ $description exists: $uuid" >&2
    echo "$uuid"
    return 0
  fi

  echo "$description not found, creating..." >&2
  local uuid
  if uuid=$(create_project "$name" "$version" "$parent_uuid"); then
    echo "✓ $description created: $uuid" >&2
    echo "$uuid"
    return 0
  fi

  return 1
}

# Upload BOM to Dependency Track using multipart/form-data
# Args: project_name, project_version, parent_name, parent_version, bom_file
upload_bom() {
  local project_name="$1"
  local project_version="$2"
  local parent_name="$3"
  local parent_version="$4"
  local bom_file="$5"

  echo "Uploading BOM..."
  echo "  projectName: $project_name"
  echo "  projectVersion: $project_version"
  echo "  parentName: $parent_name"
  echo "  parentVersion: $parent_version"
  echo ""

  local response_file
  response_file=$(mktemp)
  trap 'rm -f "$response_file"' RETURN

  local http_code
  http_code=$(curl -s -w '%{http_code}' -o "$response_file" -X POST "${API_BASE}/bom" \
    -H "X-Api-Key: $DEPENDENCY_TRACK_API_KEY" \
    -F "projectName=$project_name" \
    -F "projectVersion=$project_version" \
    -F "parentName=$parent_name" \
    -F "parentVersion=$parent_version" \
    -F "autoCreate=true" \
    -F "bom=@$bom_file")

  echo "HTTP Status: $http_code"

  if [[ "$http_code" != "200" ]]; then
    echo "✗ BOM upload failed (HTTP $http_code)" >&2
    cat "$response_file" >&2
    return 1
  fi

  echo "✓ BOM upload successful"
  local token
  token=$(jq -r '.token // empty' "$response_file")
  if [[ -n "$token" ]]; then
    echo "  Processing token: $token"
  fi
  echo ""

  return 0
}

# Update project parent relationship and external refs
# Args: child_uuid, parent_uuid, external_refs_json
update_parent_and_external_refs() {
  local child_uuid="$1"
  local parent_uuid="$2"
  local external_refs="$3"

  echo "Setting parent relationship..."

  # Get full project details
  local full_project
  full_project=$(curl -s -X GET \
    "${API_BASE}/project/${child_uuid}" \
    -H "X-Api-Key: $DEPENDENCY_TRACK_API_KEY")

  # Build the update payload
  local update_payload
  update_payload=$(echo "$full_project" | jq \
    --arg parentUuid "$parent_uuid" \
    --argjson externalReferences "$external_refs" \
    '. + {parent: {uuid: $parentUuid}}
      | if $externalReferences != null then . + {externalReferences: $externalReferences} else . end')

  # Update with parent UUID
  local response
  response=$(echo "$update_payload" \
    | curl -s -w '\n%{http_code}' -X POST "${API_BASE}/project" \
      -H "X-Api-Key: $DEPENDENCY_TRACK_API_KEY" \
      -H "Content-Type: application/json" \
      -d @-)

  local status="${response##*$'\n'}"
  local body="${response%$'\n'*}"

  if [[ "$status" == "200" ]]; then
    echo "✓ Parent relationship set"
    return 0
  fi

  echo "✗ Failed to set parent relationship (HTTP $status)" >&2
  echo "$body" >&2
  return 1
}

# Lookup and verify child project exists
# Args: project_name, project_version
# Returns: child_uuid (to stdout)
lookup_child_project() {
  local project_name="$1"
  local project_version="$2"

  echo "Looking up child project '$project_name'..." >&2

  local child_project
  child_project=$(lookup_project "$project_name" "$project_version")
  if [[ -z "$child_project" ]]; then
    echo "✗ Child project not found after BOM upload" >&2
    exit 1
  fi

  local child_uuid
  child_uuid=$(echo "$child_project" | jq -r '.uuid')
  echo "✓ Child project exists: $child_uuid" >&2
  echo "" >&2

  echo "$child_uuid"
  return 0
}

# Verify child appears in parent's children list
# Args: parent_uuid, child_project_name
verify_child_in_parent() {
  local parent_uuid="$1"
  local child_name="$2"

  echo "Verifying child appears in source project's children..."

  local children_response
  children_response=$(curl -s -X GET \
    "${API_BASE}/project/${parent_uuid}/children" \
    -H "X-Api-Key: $DEPENDENCY_TRACK_API_KEY")

  local child_uuid
  child_uuid=$(echo "$children_response" | jq -r --arg projectName "$child_name" \
    '.[] | select(.name == $projectName) | .uuid // empty')

  if [[ -n "$child_uuid" ]]; then
    echo "✓ Child project '$child_name' found in source project's children"
    return 0
  fi

  echo "✗ Child project not found in source project's children list" >&2
  echo "  Children: $(echo "$children_response" | jq -c 'map(.name)')" >&2
  return 1
}

# ============================================================================
# Main Script
# ============================================================================

# Extract metadata from SBOM
purl=$(jq -r '.metadata.component.purl // empty' "$SBOM_FILE")
component_name=$(jq -r '.metadata.component.name // empty' "$SBOM_FILE")
component_version=$(jq -r '.metadata.component.version // empty' "$SBOM_FILE")

if [[ -z "$purl" ]]; then
  echo "ERROR: No purl found in SBOM metadata.component.purl" >&2
  exit 1
fi

# Use component version or default to "unknown"
PROJECT_VERSION="${component_version:-unknown}"

# Construct project name: for Helm charts, prefix with chart name
if [[ -n "$CHART_NAME" ]]; then
  PROJECT_NAME="$CHART_NAME: $purl"
else
  PROJECT_NAME="$purl"
fi

# Extract externalReferences
external_refs=$(jq -c '.metadata.component.externalReferences // null' "$SBOM_FILE")

echo "Uploading SBOM: $SBOM_FILE"
echo "  Component: $component_name"
echo "  Source: $SOURCE_NAME"
echo "  Project Name: $PROJECT_NAME"
[[ -n "$CHART_NAME" ]] && echo "  Chart: $CHART_NAME"
echo "  Purl: $purl"
echo "  Project Version: $PROJECT_VERSION"
echo "  Parent: $PARENT_PROJECT_NAME @ $PARENT_PROJECT_VERSION"
[[ "$external_refs" != "null" ]] && echo "  External References: $(echo "$external_refs" | jq 'length')"
echo ""

# Step 1: Check/create parent project
parent_uuid=$(check_or_create_project "$PARENT_PROJECT_NAME" "$PARENT_PROJECT_VERSION" "" "parent project")
echo ""

# Step 2: Check/create source project (intermediate level)
source_uuid=$(check_or_create_project "$SOURCE_NAME" "$PARENT_PROJECT_VERSION" "$parent_uuid" "source project")
echo ""

# Step 3: Upload BOM using multipart/form-data (supports large files)
upload_bom "$PROJECT_NAME" "$PROJECT_VERSION" "$SOURCE_NAME" "$PARENT_PROJECT_VERSION" "$SBOM_FILE"

# Step 4: Lookup and verify child project
child_uuid=$(lookup_child_project "$PROJECT_NAME" "$PROJECT_VERSION")

# Step 5: Fixup parent relationship and set external refs
# These are unfortunately not ensured during SBOM uploading
update_parent_and_external_refs "$child_uuid" "$source_uuid" "$external_refs"

# Step 6: Verify child appears in source project's children
verify_child_in_parent "$source_uuid" "$PROJECT_NAME"
echo ""

echo "✓ Upload completed successfully"
