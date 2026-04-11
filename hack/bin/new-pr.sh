#!/usr/bin/env bash

set -euo pipefail
set -x

# Validate arguments
if [ $# -ne 3 ]; then
  echo "Usage: $0 <wpb-number> <changelog-category> <description>"
  echo "  wpb-number: just the number (e.g., 4231)"
  echo "  changelog-category: must be 1, 2, 3, 4, 5, or 6; selects sub-directory in ./changelog.d/"
  echo "  description: the changelog text (e.g., 'Fix problem with bug.')"
  exit 1
fi

WPB_NUMBER="$1"
CATEGORY="$2"
DESCRIPTION="$3"

# Validate WPB number format (should be just digits)
if ! [[ "$WPB_NUMBER" =~ ^[0-9]+$ ]]; then
  echo "Error: WPB number must be numeric"
  exit 1
fi

# Validate category
if ! [[ "$CATEGORY" =~ ^[1-6]$ ]]; then
  echo "Error: Category must be 1, 2, 3, 4, 5, or 6"
  exit 1
fi

# Compute branch name from description
# Remove trailing period, convert to lowercase, replace spaces with dashes, sanitize special chars
BRANCH_SUFFIX=$(echo "$DESCRIPTION" | sed 's/\.$//' | tr '[:upper:]' '[:lower:]' | sed 's/ /-/g' | sed 's/[^a-z0-9_-]/_/g')
BRANCH_NAME="WPB-${WPB_NUMBER}-${BRANCH_SUFFIX}"

# Step 1: Switch to develop, pull, checkout new branch
echo "Switching to develop and pulling latest changes..."
git checkout develop
git pull origin develop
git checkout -b "$BRANCH_NAME"

# Step 2: Create changelog entry

# Find the changelog directory matching the pattern
CHANGELOG_DIR=$(find ./changelog.d -type d -name "${CATEGORY}-*" | head -n 1)

if [ -z "$CHANGELOG_DIR" ]; then
  echo "Error: Could not find changelog directory matching pattern ./changelog.d/${CATEGORY}-*"
  exit 1
fi

# Create changelog file with the description as-is
CHANGELOG_FILE="${CHANGELOG_DIR}/${BRANCH_NAME}"
echo "Creating changelog entry: $CHANGELOG_FILE"
echo "$DESCRIPTION" > "$CHANGELOG_FILE"

# Step 3: Commit the changelog
echo "Committing changelog..."
git add "$CHANGELOG_FILE"
git commit -m "Changelog."

# Push the branch
echo "Pushing branch to origin..."
git push -u origin "$BRANCH_NAME"

# Step 4: Create draft PR
PR_TITLE="[WPB-${WPB_NUMBER}] ${DESCRIPTION}"
JIRA_URL="https://wearezeta.atlassian.net/browse/WPB-${WPB_NUMBER}"

# Read PR template
if [ -f .github/pull_request_template.md ]; then
  PR_TEMPLATE=$(cat .github/pull_request_template.md)
  PR_BODY="${JIRA_URL}

${PR_TEMPLATE}"
else
  PR_BODY="$JIRA_URL"
fi

echo "Creating draft PR..."
gh pr create --draft --title "$PR_TITLE" --body "$PR_BODY" --head "$BRANCH_NAME"

echo ""
echo "Done!"
echo "  Branch: $BRANCH_NAME"
echo "  Changelog: $CHANGELOG_FILE"
echo "  Content: $DESCRIPTION"
