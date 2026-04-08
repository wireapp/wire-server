#!/usr/bin/env bash
set -euo pipefail

# Simple OpenAPI client generator using openapi-generator
# Usage: ./generate-clients.sh <swagger-url>

SWAGGER_URL="${1:-https://staging-nginz-https.zinfra.io/v16/api/swagger.json}"
OUTPUT_DIR="$(pwd)/generated"

echo "==> Generating clients from: $SWAGGER_URL"
echo "==> Output directory: $OUTPUT_DIR"

# Clean up previous runs
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Download the spec
echo "==> Downloading OpenAPI spec..."
curl -s "$SWAGGER_URL" > "$OUTPUT_DIR/swagger.json"

# Check if docker is available
if ! command -v docker &> /dev/null; then
    echo "Error: docker is not installed. Please install docker to use openapi-generator."
    echo "Alternative: install openapi-generator-cli via npm: npm install -g @openapitools/openapi-generator-cli"
    exit 1
fi

# Generate TypeScript client
echo ""
echo "==> Generating TypeScript client..."
docker run --rm \
    -v "$OUTPUT_DIR:/local" \
    openapitools/openapi-generator-cli:latest generate \
    -i /local/swagger.json \
    -g typescript-axios \
    -o /local/typescript \
    --additional-properties=supportsES6=true,npmName=wire-api-client,npmVersion=1.0.0

# Generate Kotlin client
echo ""
echo "==> Generating Kotlin client..."
docker run --rm \
    -v "$OUTPUT_DIR:/local" \
    openapitools/openapi-generator-cli:latest generate \
    -i /local/swagger.json \
    -g kotlin \
    -o /local/kotlin \
    --additional-properties=packageName=com.wire.api.client,serializationLibrary=gson

echo ""
echo "==> Done! Generated clients:"
echo "  TypeScript: $OUTPUT_DIR/typescript"
echo "  Kotlin:     $OUTPUT_DIR/kotlin"
