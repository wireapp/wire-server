#!/usr/bin/env bash

# Configuration - edit these values as needed
WIRE_BACKEND="https://staging-nginz-https.zinfra.io"
EMAIL="user@example.com"
PASSWORD="your-password"
SCIM_TOKEN_PAYLOAD='{
  "description": "My SCIM Token",
  "name": "Optional token name",
  "password": "confirmation-password",
  "verification_code": "123456"
}'

# Step 1: Login to get access token
echo "Logging in..."
LOGIN_RESPONSE=$(curl -s -w "\n%{http_code}" -X POST \
  --header 'Content-Type: application/json' \
  -d "{\"email\":\"$EMAIL\",\"password\":\"$PASSWORD\"}" \
  "$WIRE_BACKEND/login")

# Extract response body and status code
LOGIN_BODY=$(echo "$LOGIN_RESPONSE" | head -n -1)
LOGIN_CODE=$(echo "$LOGIN_RESPONSE" | tail -n 1)

echo "Login HTTP Status: $LOGIN_CODE"

# Extract access token from login response
ACCESS_TOKEN=$(echo "$LOGIN_BODY" | jq -r '.access_token')

if [ -z "$ACCESS_TOKEN" ] || [ "$ACCESS_TOKEN" = "null" ]; then
  echo "Failed to obtain access token"
  echo "$LOGIN_BODY" | jq '.'
  exit 1
fi

echo "Access token obtained successfully"

# Step 2: Create SCIM token
echo ""
echo "Creating SCIM token..."
SCIM_RESPONSE=$(curl -s -w "\n%{http_code}" -X POST \
  --header "Authorization: Bearer $ACCESS_TOKEN" \
  --header 'Content-Type: application/json' \
  -d "$SCIM_TOKEN_PAYLOAD" \
  "$WIRE_BACKEND/scim/auth-tokens")

# Extract response body and status code
SCIM_BODY=$(echo "$SCIM_RESPONSE" | head -n -1)
SCIM_CODE=$(echo "$SCIM_RESPONSE" | tail -n 1)

# Validate SCIM token creation succeeded
if [ "$SCIM_CODE" != "200" ] && [ "$SCIM_CODE" != "201" ]; then
  echo "Failed to create SCIM token (HTTP $SCIM_CODE)"
  echo "$SCIM_BODY" | jq '.'
  exit 1
fi

# Step 3: Display results
echo "HTTP Status: $SCIM_CODE"
echo ""
echo "$SCIM_BODY" | jq '.'
