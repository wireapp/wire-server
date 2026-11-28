#!/usr/bin/env bash
set -euo pipefail

# Script to extract Jira issue from PR title and body with priority order:
# 1. Title
# 2. ticket:<url> pattern
# 3. Anywhere in description

# Usage:
#   ./extract-jira-issue.sh <pr_number>
#   or
#   PR_TITLE="..." PR_BODY="..." ./extract-jira-issue.sh

extract_jira_issue() {
  local PR_TITLE="$1"
  local PR_BODY="$2"

  echo "PR Title: $PR_TITLE"
  echo "PR Body:"
  echo "$PR_BODY"
  echo "---"

  local JIRA_ISSUE=""
  local FOUND_AT=""

  # Priority 1: Check title for WPB-* pattern
  echo "Checking PR title for Jira issues..."
  local TITLE_ISSUES
  TITLE_ISSUES=$(echo "$PR_TITLE" | grep -oE '\[?WPB-[0-9]+\]?' | tr -d '[]' | sort -u || true)
  local TITLE_COUNT
  if [ -z "$TITLE_ISSUES" ]; then
    TITLE_COUNT=0
  else
    TITLE_COUNT=$(echo -n "$TITLE_ISSUES" | grep -c '^')
  fi

  if [ "$TITLE_COUNT" -gt 1 ]; then
    echo "ERROR: Multiple Jira issues found in title (ambiguous):"
    echo "$TITLE_ISSUES"
    exit 1
  elif [ "$TITLE_COUNT" -eq 1 ]; then
    JIRA_ISSUE="$TITLE_ISSUES"
    FOUND_AT="title"
    echo "Found Jira issue in title: $JIRA_ISSUE"
  else
    # Priority 2: Check for ticket:<url> or bare Jira URL pattern with WPB-* (case-insensitive)
    echo "No issue in title, checking for ticket URL patterns..."
    local TICKET_ISSUES
    # Match both: ticket:<url> or https://.../browse/WPB-* or .../WPB-*
    TICKET_ISSUES=$(echo "$PR_BODY" | grep -ioE '(ticket:|https?://[^ ]*/browse/)WPB-[0-9]+' | grep -ioE 'WPB-[0-9]+' | sort -u || true)
    local TICKET_COUNT
    TICKET_COUNT=$(echo -n "$TICKET_ISSUES" | grep -c '^' || echo 0)
    # Handle empty string case
    if [ -z "$TICKET_ISSUES" ]; then
      TICKET_COUNT=0
    fi

    if [ "$TICKET_COUNT" -gt 1 ]; then
      echo "ERROR: Multiple Jira issues found in ticket URLs (ambiguous):"
      echo "$TICKET_ISSUES"
      exit 1
    elif [ "$TICKET_COUNT" -eq 1 ]; then
      JIRA_ISSUE="$TICKET_ISSUES"
      FOUND_AT="ticket URL"
      echo "Found Jira issue in ticket URL: $JIRA_ISSUE"
    else
      # Priority 3: Check anywhere in description for WPB-*
      echo "No ticket URL found, checking entire description..."
      local DESC_ISSUES
      DESC_ISSUES=$(echo "$PR_BODY" | grep -oE '\[?WPB-[0-9]+\]?' | tr -d '[]' | sort -u || true)
      local DESC_COUNT
      if [ -z "$DESC_ISSUES" ]; then
        DESC_COUNT=0
      else
        DESC_COUNT=$(echo -n "$DESC_ISSUES" | grep -c '^')
      fi

      if [ "$DESC_COUNT" -gt 1 ]; then
        echo "ERROR: Multiple Jira issues found in description (ambiguous):"
        echo "$DESC_ISSUES"
        exit 1
      elif [ "$DESC_COUNT" -eq 1 ]; then
        JIRA_ISSUE="$DESC_ISSUES"
        FOUND_AT="description"
        echo "Found Jira issue in description: $JIRA_ISSUE"
      fi
    fi
  fi

  if [ -z "$JIRA_ISSUE" ]; then
    echo "No Jira issue found"
    return 1
  else
    echo "========================================="
    echo "Final Jira issue: $JIRA_ISSUE (found in $FOUND_AT)"
    echo "========================================="
    # Output in format that can be easily parsed
    echo "JIRA_ISSUE=$JIRA_ISSUE"
    echo "FOUND_AT=$FOUND_AT"
    return 0
  fi
}

# Main script logic
if [ $# -eq 1 ]; then
  # PR number provided, fetch from GitHub
  PR_NUMBER="$1"
  if ! command -v gh &> /dev/null; then
    echo "ERROR: gh CLI is not installed. Please install it or provide PR_TITLE and PR_BODY environment variables."
    exit 1
  fi

  echo "Fetching PR #$PR_NUMBER..."
  PR_DATA=$(gh pr view "$PR_NUMBER" --json title,body)
  PR_TITLE=$(echo "$PR_DATA" | jq -r '.title')
  PR_BODY=$(echo "$PR_DATA" | jq -r '.body // ""')

  extract_jira_issue "$PR_TITLE" "$PR_BODY"
elif [ -n "${PR_TITLE:-}" ] && [ -n "${PR_BODY:-}" ]; then
  # Environment variables provided
  extract_jira_issue "$PR_TITLE" "$PR_BODY"
else
  echo "Usage:"
  echo "  $0 <pr_number>                    # Fetch PR from GitHub"
  echo "  PR_TITLE=\"...\" PR_BODY=\"...\" $0    # Use environment variables"
  echo ""
  echo "Examples:"
  echo "  $0 5134"
  echo "  PR_TITLE=\"WPB-12345 Fix bug\" PR_BODY=\"Some description\" $0"
  exit 1
fi
