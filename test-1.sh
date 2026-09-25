#!/usr/bin/env bash
#
# Shell translation of `testReadExternalAppToGroupConversation` in
# integration/test/Test/Apps.hs (WPB-28977).
#
# An app is created in team 1 and added to team 2 as a collaborator, then
# to a group conversation owned by a member of team 2.  Then:
#
#   - removing the collaborator must remove the app from the conversation;
#   - re-adding the collaborator must NOT silently put the app back;
#   - explicitly adding the app to the conversation again must work.
#
# By default the script bootstraps both teams itself against a local
# wire-server (brig's internal port).  Pass --t1-admin/--t2-admin/--t2-member
# to run against teams that already exist (e.g. on a staging backend), in
# which case only the public API is used.

# shellcheck disable=all

set -euo pipefail

# ---------------------------------------------------------------- defaults --

BASE_URL="http://localhost:8080"    # nginz
BRIG_URL="http://localhost:8082"    # brig internal (bootstrap only)
API_VERSION=""                      # autodetected from /api-version
DOMAIN=""                           # autodetected from /api-version
EMAIL_DOMAIN="example.com"
PASSWORD="hunter2."
HTTP_TIMEOUT=10                     # per request, seconds
RETRY_TIMEOUT=10                    # for eventually-consistent assertions
SETTLE=2                            # grace period before "must not happen" checks
INSECURE=false
VERBOSE=false

T1_ADMIN_EMAIL="NBTtvgGR@example.com"
T1_ADMIN_PASSWORD="2ohZy4cF"
T1_ID="4f513d01-cfde-4f74-ae2b-5e1c0dbc6422"
T2_ADMIN_EMAIL="v2FykzAR@example.com"
T2_ADMIN_PASSWORD="50tCoVul"
T2_ID="86b0704b-7ecd-42b7-8a03-2b4ac3de647f"
T2_MEMBER_EMAIL="w001@example.com"
T2_MEMBER_PASSWORD="P61HpSQU"

USAGE="Run the 'external app in a group conversation' scenario against a
running backend, asserting each step.

USAGE: $0 [OPTIONS...]

Connection:
    -u, --base-url URL      nginz base URL. default: $BASE_URL
    -b, --brig-url URL      brig internal base URL, used for bootstrapping
                            teams. default: $BRIG_URL
    -a, --api-version VER   API version, e.g. 'v18'. default: highest
                            version reported by GET \$BASE_URL/api-version
    -d, --domain DOMAIN     backend domain. default: reported by
                            GET \$BASE_URL/api-version
    -k, --insecure          accept self-signed TLS certificates

Bootstrap (ignored if credentials are given):
    -e, --email-domain DOM  domain for generated email addresses.
                            default: $EMAIL_DOMAIN
    -p, --password PW       password for generated users. default: $PASSWORD

Existing teams (public API only; all three must be given together):
    --t1-admin EMAIL:PASSWORD:TEAMID   owner of the team owning the app
    --t2-admin EMAIL:PASSWORD:TEAMID   owner of the team adding the app
                                       as a collaborator
    --t2-member EMAIL:PASSWORD         member of team 2, owns the conversation

Misc:
    -t, --retry-timeout N   seconds to wait for eventually-consistent
                            assertions. default: $RETRY_TIMEOUT
    -s, --settle N          seconds to wait before checking that something
                            did NOT happen. default: $SETTLE
    -v, --verbose           log every request and response body
    -h, --help              this message
"

die() { echo "error: $*" 1>&2; exit 1; }

split_creds() { # <value> <expected field count> <flag name>
  local IFS=:
  read -r -a _CREDS <<<"$1"
  [[ ${#_CREDS[@]} -eq $2 ]] || die "$3 needs $2 colon-separated fields, got '$1'"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -u|--base-url)      BASE_URL="${2:?}"; shift 2 ;;
    -b|--brig-url)      BRIG_URL="${2:?}"; shift 2 ;;
    -a|--api-version)   API_VERSION="${2:?}"; shift 2 ;;
    -d|--domain)        DOMAIN="${2:?}"; shift 2 ;;
    -e|--email-domain)  EMAIL_DOMAIN="${2:?}"; shift 2 ;;
    -p|--password)      PASSWORD="${2:?}"; shift 2 ;;
    -t|--retry-timeout) RETRY_TIMEOUT="${2:?}"; shift 2 ;;
    -s|--settle)        SETTLE="${2:?}"; shift 2 ;;
    -k|--insecure)      INSECURE=true; shift ;;
    -v|--verbose)       VERBOSE=true; shift ;;
    -h|--help)          echo "$USAGE"; exit 0 ;;
    --t1-admin)
      split_creds "${2:?}" 3 "$1"
      T1_ADMIN_EMAIL="${_CREDS[0]}"; T1_ADMIN_PASSWORD="${_CREDS[1]}"; T1_ID="${_CREDS[2]}"
      shift 2 ;;
    --t2-admin)
      split_creds "${2:?}" 3 "$1"
      T2_ADMIN_EMAIL="${_CREDS[0]}"; T2_ADMIN_PASSWORD="${_CREDS[1]}"; T2_ID="${_CREDS[2]}"
      shift 2 ;;
    --t2-member)
      split_creds "${2:?}" 2 "$1"
      T2_MEMBER_EMAIL="${_CREDS[0]}"; T2_MEMBER_PASSWORD="${_CREDS[1]}"
      shift 2 ;;
    *) echo "$USAGE" 1>&2; die "unknown argument '$1'" ;;
  esac
done

for tool in curl jq; do
  command -v "$tool" >/dev/null || die "'$tool' is required but not on \$PATH"
done

BOOTSTRAP=true
if [[ -n "$T1_ADMIN_EMAIL$T2_ADMIN_EMAIL$T2_MEMBER_EMAIL" ]]; then
  BOOTSTRAP=false
  [[ -n "$T1_ADMIN_EMAIL" && -n "$T2_ADMIN_EMAIL" && -n "$T2_MEMBER_EMAIL" ]] ||
    die "--t1-admin, --t2-admin and --t2-member must be given together"
fi

# ------------------------------------------------------------------ output --

if [[ -t 1 ]]; then
  C_RED=$'\e[31m'; C_GREEN=$'\e[32m'; C_BOLD=$'\e[1m'; C_OFF=$'\e[0m'
else
  C_RED=""; C_GREEN=""; C_BOLD=""; C_OFF=""
fi

FAILURES=0
CHECKS=0

step() { echo; echo "${C_BOLD}=== $* ===${C_OFF}"; }
info() { echo "    $*"; }
pass() { CHECKS=$((CHECKS + 1)); echo "    ${C_GREEN}ok${C_OFF}   $*"; }
fail() {
  CHECKS=$((CHECKS + 1)); FAILURES=$((FAILURES + 1))
  echo "    ${C_RED}FAIL${C_OFF} $*"
}

# ----------------------------------------------------------------- http/api --

CURL_OPTS=(-sS --max-time "$HTTP_TIMEOUT")
$INSECURE && CURL_OPTS+=(-k)

STATUS=""
BODY=""

# api <method> <url> <json body or ""> [header...]; sets $STATUS and $BODY.
api() {
  local method="$1" url="$2" body="$3"
  shift 3
  local -a opts=("${CURL_OPTS[@]}" -X "$method" -H 'Accept: application/json')
  local h
  for h in "$@"; do opts+=(-H "$h"); done
  if [[ -n "$body" ]]; then
    opts+=(-H 'Content-Type: application/json' --data-binary "$body")
  fi

  $VERBOSE && { echo "    > $method $url"; [[ -n "$body" ]] && echo "    > $body"; } || true

  local out
  out=$(curl "${opts[@]}" -w $'\n%{http_code}' "$url")
  STATUS="${out##*$'\n'}"
  BODY="${out%$'\n'*}"

  $VERBOSE && echo "    < $STATUS $BODY" || true
  return 0
}

# expect <expected status or '2xx'> <description>
expect() {
  local want="$1" what="$2"
  if [[ "$want" == "2xx" && "$STATUS" == 2* ]] || [[ "$STATUS" == "$want" ]]; then
    pass "$what (HTTP $STATUS)"
  else
    fail "$what: expected HTTP $want, got $STATUS: $BODY"
  fi
}

# require <expected status or '2xx'> <description> -- abort if it does not hold
require() {
  local before="$FAILURES"
  expect "$@"
  [[ "$FAILURES" == "$before" ]] || die "cannot continue: $2"
}

rand_str() {
  local n="${1:-8}" chars=abcdefghijklmnopqrstuvwxyz0123456789 s="" i
  for ((i = 0; i < n; i++)); do s+="${chars:RANDOM % ${#chars}:1}"; done
  echo "$s"
}

rand_email() { echo "$(rand_str 12)@${EMAIL_DOMAIN}"; }

# retry <timeout secs> <cmd...> -- until it succeeds
retry() {
  local deadline=$((SECONDS + $1)); shift
  while true; do
    if "$@"; then return 0; fi
    ((SECONDS >= deadline)) && return 1
    sleep 0.5
  done
}

# ------------------------------------------------------------- backend info --

api GET "$BASE_URL/api-version" ""
require 200 "reach nginz at $BASE_URL"

[[ -n "$API_VERSION" ]] || API_VERSION="v$(jq -r '.supported | max' <<<"$BODY")"
[[ -n "$DOMAIN" ]] || DOMAIN="$(jq -r '.domain' <<<"$BODY")"
V="$BASE_URL/$API_VERSION"

info "api version: $API_VERSION, domain: $DOMAIN"

# ---------------------------------------------------------------- bootstrap --

# create_team_owner -> "<uid> <tid> <email>"
create_team_owner() {
  local email; email="$(rand_email)"
  local body
  body=$(jq -n --arg e "$email" --arg p "$PASSWORD" \
    '{email: $e, password: $p, name: $e, icon: "default",
      team: {name: "test-1.sh team", icon: "default"}}')
  api POST "$BRIG_URL/i/users" "$body"
  [[ "$STATUS" == 2* ]] || die "could not create team owner (HTTP $STATUS): $BODY"
  jq -r '"\(.id) \(.team) "' <<<"$BODY" | tr -d '\n'
  echo "$email"
}

# invite_team_member <tid> <owner uid> -> "<uid> <email>"
invite_team_member() {
  local tid="$1" owner="$2" email; email="$(rand_email)"

  api POST "$BRIG_URL/teams/$tid/invitations" \
    "$(jq -n --arg e "$email" '{email: $e}')" "Z-User: $owner"
  [[ "$STATUS" == 2* ]] || die "could not invite $email (HTTP $STATUS): $BODY"
  local iid; iid="$(jq -r '.id' <<<"$BODY")"

  api GET "$BRIG_URL/i/teams/invitation-code?team=$tid&invitation_id=$iid" ""
  [[ "$STATUS" == 2* ]] || die "no invitation code for $email (HTTP $STATUS): $BODY"
  local code; code="$(jq -r '.code' <<<"$BODY")"

  local body
  body=$(jq -n --arg e "$email" --arg p "$PASSWORD" --arg c "$code" \
    '{email: $e, password: $p, name: $e, team_code: $c}')
  api POST "$BRIG_URL/i/users" "$body"
  [[ "$STATUS" == 2* ]] || die "could not register $email (HTTP $STATUS): $BODY"
  echo "$(jq -r '.id' <<<"$BODY") $email"
}

if $BOOTSTRAP; then
  step "0. Bootstrapping two teams via $BRIG_URL"
  read -r OWNER1_ID T1_ID T1_ADMIN_EMAIL < <(create_team_owner)
  read -r OWNER2_ID T2_ID T2_ADMIN_EMAIL < <(create_team_owner)
  read -r MEMBER2_ID T2_MEMBER_EMAIL < <(invite_team_member "$T2_ID" "$OWNER2_ID")
  T1_ADMIN_PASSWORD="$PASSWORD"
  T2_ADMIN_PASSWORD="$PASSWORD"
  T2_MEMBER_PASSWORD="$PASSWORD"
  info "team 1: $T1_ID (owner $OWNER1_ID / $T1_ADMIN_EMAIL)"
  info "team 2: $T2_ID (owner $OWNER2_ID / $T2_ADMIN_EMAIL)"
  info "        member $MEMBER2_ID / $T2_MEMBER_EMAIL"
fi

# -------------------------------------------------------------------- login --

login() {
  local email="$1" pass="$2"
  api POST "$V/login?persist=false" "$(jq -n --arg e "$email" --arg p "$pass" \
    '{email: $e, password: $p}')"
  [[ "$STATUS" == 200 ]] || die "login failed for $email (HTTP $STATUS): $BODY"
  jq -r '.access_token' <<<"$BODY"
}

step "1. Logging in"
T1_ADMIN_TOKEN="$(login "$T1_ADMIN_EMAIL" "$T1_ADMIN_PASSWORD")"
T2_ADMIN_TOKEN="$(login "$T2_ADMIN_EMAIL" "$T2_ADMIN_PASSWORD")"
T2_MEMBER_TOKEN="$(login "$T2_MEMBER_EMAIL" "$T2_MEMBER_PASSWORD")"
info "all three logins successful"

AS_T1_ADMIN="Authorization: Bearer $T1_ADMIN_TOKEN"
AS_T2_ADMIN="Authorization: Bearer $T2_ADMIN_TOKEN"
AS_T2_MEMBER="Authorization: Bearer $T2_MEMBER_TOKEN"

# ---------------------------------------------------------- the actual test --

step "2. Creating an app in team 1"
APP_NAME="external-app-$(rand_str 6)"
api POST "$V/teams/$T1_ID/apps" \
  "$(jq -n --arg n "$APP_NAME" --arg p "$PASSWORD" \
    '{name: $n, category: "other", description: "default description", password: $p}')" \
  "$AS_T1_ADMIN"
require 200 "create app '$APP_NAME'"
APP_ID="$(jq -r '.user.id' <<<"$BODY")"
[[ -n "$APP_ID" && "$APP_ID" != null ]] || die "no app id in response: $BODY"
info "app id: $APP_ID"

add_collaborator() {
  api POST "$V/teams/$T2_ID/collaborators" \
    "$(jq -n --arg u "$APP_ID" \
      '{user: $u, permissions: ["create_team_conversation", "implicit_connection"]}')" \
    "$AS_T2_ADMIN"
}

step "3. Adding the app to team 2 as a collaborator"
add_collaborator
require 2xx "add collaborator"

step "4. Creating a proteus group conversation in team 2"
api POST "$V/conversations" \
  "$(jq -n --arg t "$T2_ID" \
    '{qualified_users: [], conversation_role: "wire_admin", protocol: "proteus",
      cells: false, team: {teamid: $t, managed: false}}')" \
  "$AS_T2_MEMBER"
require 201 "create conversation"
CONV_ID="$(jq -r '.qualified_id.id' <<<"$BODY")"
CONV_DOMAIN="$(jq -r '.qualified_id.domain' <<<"$BODY")"
[[ -n "$CONV_ID" && "$CONV_ID" != null ]] || die "no conversation id in response: $BODY"
info "conversation: $CONV_DOMAIN/$CONV_ID"

add_app_to_conv() {
  api POST "$V/conversations/$CONV_DOMAIN/$CONV_ID/members" \
    "$(jq -n --arg d "$DOMAIN" --arg u "$APP_ID" \
      '{qualified_users: [{domain: $d, id: $u}]}')" \
    "$AS_T2_MEMBER"
}

conv_member_ids() {
  api GET "$V/conversations/$CONV_DOMAIN/$CONV_ID" "" "$AS_T2_MEMBER"
  [[ "$STATUS" == 200 ]] || { echo "get conversation failed (HTTP $STATUS): $BODY" 1>&2; return 1; }
  jq -r '.members.others[]?.qualified_id.id' <<<"$BODY"
}

conv_has_app() {
  local ids; ids="$(conv_member_ids)" || return 2
  [[ $'\n'"$ids"$'\n' == *$'\n'"$APP_ID"$'\n'* ]]
}

conv_lacks_app() { ! conv_has_app; }

assert_conv_has_app() { # <description>
  if retry "$RETRY_TIMEOUT" conv_has_app; then
    pass "$1"
  else
    fail "$1: app $APP_ID is not among [$(conv_member_ids | tr '\n' ' ')]"
  fi
}

assert_conv_lacks_app() { # <description> <eventually?>
  if [[ "${2:-}" == eventually ]]; then
    retry "$RETRY_TIMEOUT" conv_lacks_app || true
  fi
  if conv_lacks_app; then
    pass "$1"
  else
    fail "$1: app $APP_ID is still among [$(conv_member_ids | tr '\n' ' ')]"
  fi
}

step "5. Adding the app to the conversation"
add_app_to_conv
expect 2xx "add app to conversation"
assert_conv_has_app "app is a member of the conversation"

step "6. Removing the collaborator from team 2"
api DELETE "$V/teams/$T2_ID/collaborators/$APP_ID" "" "$AS_T2_ADMIN"
expect 2xx "remove collaborator"
assert_conv_lacks_app "app was removed from the conversation" eventually

step "7. Adding the collaborator back to team 2"
add_collaborator
expect 2xx "re-add collaborator"
info "waiting ${SETTLE}s to see whether the app reappears..."
sleep "$SETTLE"
assert_conv_lacks_app "app did NOT silently rejoin the conversation"

step "8. Adding the app to the conversation again (the regression under test)"
add_app_to_conv
expect 2xx "re-add app to conversation"
assert_conv_has_app "app is a member of the conversation again"

# ------------------------------------------------------------------ summary --

echo
if ((FAILURES == 0)); then
  echo "${C_GREEN}${C_BOLD}PASS${C_OFF} — $CHECKS checks, 0 failures"
else
  echo "${C_RED}${C_BOLD}FAIL${C_OFF} — $CHECKS checks, $FAILURES failures"
fi
exit $((FAILURES > 0))
