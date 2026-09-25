#!/usr/bin/env bash
#
# Shell translation of `testRemoveReadExternalAppOne2OneConversation` in
# integration/test/Test/Apps.hs (WPB-28977).
#
# An app is created in team 1 and added to team 2 as a collaborator.  A
# member of team 2 then opens a 1:1 conversation with it.  Then:
#
#   - removing the collaborator must make the 1:1 conversation vanish
#     (404 no-conversation, on the public and on galley's internal API)
#     and make the MLS 1:1 unreachable (403 not-connected);
#   - adding the collaborator back must make the MLS 1:1 reachable again.
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
GALLEY_URL=""                       # galley internal; default depends on mode
API_VERSION=""                      # autodetected from /api-version
DOMAIN=""                           # autodetected from /api-version
EMAIL_DOMAIN="example.com"
PASSWORD="hunter2."
HTTP_TIMEOUT=10                     # per request, seconds
RETRY_TIMEOUT=10                    # for eventually-consistent assertions
INSECURE=false
VERBOSE=false
SKIP_INTERNAL=false


T1_ADMIN_EMAIL="NBTtvgGR@example.com"
T1_ADMIN_PASSWORD="2ohZy4cF"
T1_ID="4f513d01-cfde-4f74-ae2b-5e1c0dbc6422"
T2_ADMIN_EMAIL="v2FykzAR@example.com"
T2_ADMIN_PASSWORD="50tCoVul"
T2_ID="86b0704b-7ecd-42b7-8a03-2b4ac3de647f"
T2_MEMBER_EMAIL="w001@example.com"
T2_MEMBER_PASSWORD="P61HpSQU"

USAGE="Run the 'external app in a 1:1 conversation' scenario against a
running backend, asserting each step.

USAGE: $0 [OPTIONS...]

Connection:
    -u, --base-url URL      nginz base URL. default: $BASE_URL
    -b, --brig-url URL      brig internal base URL, used for bootstrapping
                            teams. default: $BRIG_URL
    -g, --galley-url URL    galley internal base URL, used for the
                            'GET /i/conversations/:cid' checks. default:
                            http://localhost:8085 when bootstrapping,
                            otherwise those checks are skipped
        --skip-internal     skip the galley internal API checks
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
    --t2-member EMAIL:PASSWORD         member of team 2, opens the 1:1

Misc:
    -t, --retry-timeout N   seconds to wait for eventually-consistent
                            assertions. default: $RETRY_TIMEOUT
    -v, --verbose           log every request and response body
    -h, --help              this message
"

die() { echo "error: $*" 1>&2; exit 1; }

split_creds() { # <value> <expected field count> <flag name>
  local IFS=:
  read -r -a _CREDS <<<"$1"
  [[ ${#_CREDS[@]} -eq $2 ]] || die "$3 needs $2 colon-separated fields, got '$1'"
}

GALLEY_URL_GIVEN=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    -u|--base-url)      BASE_URL="${2:?}"; shift 2 ;;
    -b|--brig-url)      BRIG_URL="${2:?}"; shift 2 ;;
    -g|--galley-url)    GALLEY_URL="${2:?}"; GALLEY_URL_GIVEN=true; shift 2 ;;
    --skip-internal)    SKIP_INTERNAL=true; shift ;;
    -a|--api-version)   API_VERSION="${2:?}"; shift 2 ;;
    -d|--domain)        DOMAIN="${2:?}"; shift 2 ;;
    -e|--email-domain)  EMAIL_DOMAIN="${2:?}"; shift 2 ;;
    -p|--password)      PASSWORD="${2:?}"; shift 2 ;;
    -t|--retry-timeout) RETRY_TIMEOUT="${2:?}"; shift 2 ;;
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

# The galley internal API is not exposed through nginz, so it is only
# available when we are talking to a local backend anyway.
if ! $GALLEY_URL_GIVEN; then
  if $BOOTSTRAP; then GALLEY_URL="http://localhost:8085"; else SKIP_INTERNAL=true; fi
fi

# ------------------------------------------------------------------ output --

if [[ -t 1 ]]; then
  C_RED=$'\e[31m'; C_GREEN=$'\e[32m'; C_YELLOW=$'\e[33m'; C_BOLD=$'\e[1m'; C_OFF=$'\e[0m'
else
  C_RED=""; C_GREEN=""; C_YELLOW=""; C_BOLD=""; C_OFF=""
fi

FAILURES=0
CHECKS=0

step() { echo; echo "${C_BOLD}=== $* ===${C_OFF}"; }
info() { echo "    $*"; }
skip() { echo "    ${C_YELLOW}skip${C_OFF} $*"; }
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

# matches <expected status or '2xx'> <expected label, "" for any>
matches() {
  if [[ "$1" == "2xx" ]]; then
    [[ "$STATUS" == 2* ]] || return 1
  else
    [[ "$STATUS" == "$1" ]] || return 1
  fi
  [[ -z "$2" ]] && return 0
  [[ "$(jq -r 'if type == "object" then .label // "" else "" end' <<<"$BODY" 2>/dev/null)" == "$2" ]]
}

describe() { # <status> <label>
  if [[ -n "$2" ]]; then echo "HTTP $1 with label '$2'"; else echo "HTTP $1"; fi
}

# check <expected status> <expected label or ""> <description> <cmd...>
check() {
  local want="$1" label="$2" desc="$3"; shift 3
  "$@"
  if matches "$want" "$label"; then
    pass "$desc ($(describe "$STATUS" "$label"))"
  else
    fail "$desc: expected $(describe "$want" "$label"), got $STATUS: $BODY"
  fi
}

# like check, but retries until $RETRY_TIMEOUT is up
check_eventually() {
  local want="$1" label="$2" desc="$3"; shift 3
  local deadline=$((SECONDS + RETRY_TIMEOUT))
  while true; do
    "$@"
    if matches "$want" "$label"; then break; fi
    if ((SECONDS >= deadline)); then break; fi
    sleep 0.5
  done
  if matches "$want" "$label"; then
    pass "$desc ($(describe "$STATUS" "$label"))"
  else
    fail "$desc: expected $(describe "$want" "$label"), got $STATUS: $BODY"
  fi
}

# require <expected status> <expected label or ""> <description> <cmd...>
require() {
  local before="$FAILURES"
  check "$@"
  [[ "$FAILURES" == "$before" ]] || die "cannot continue: $3"
}

rand_str() {
  local n="${1:-8}" chars=abcdefghijklmnopqrstuvwxyz0123456789 s="" i
  for ((i = 0; i < n; i++)); do s+="${chars:RANDOM % ${#chars}:1}"; done
  echo "$s"
}

rand_email() { echo "$(rand_str 12)@${EMAIL_DOMAIN}"; }

# ------------------------------------------------------------- backend info --

get_api_version() { api GET "$BASE_URL/api-version" ""; }
require 200 "" "reach nginz at $BASE_URL" get_api_version

[[ -n "$API_VERSION" ]] || API_VERSION="v$(jq -r '.supported | max' <<<"$BODY")"
[[ -n "$DOMAIN" ]] || DOMAIN="$(jq -r '.domain' <<<"$BODY")"
V="$BASE_URL/$API_VERSION"

info "api version: $API_VERSION, domain: $DOMAIN"
$SKIP_INTERNAL && info "galley internal checks: skipped" || info "galley internal: $GALLEY_URL"

# ---------------------------------------------------------------- bootstrap --

# create_team_owner -> "<uid> <tid> <email>"
create_team_owner() {
  local email; email="$(rand_email)"
  local body
  body=$(jq -n --arg e "$email" --arg p "$PASSWORD" \
    '{email: $e, password: $p, name: $e, icon: "default",
      team: {name: "test-2.sh team", icon: "default"}}')
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

echo $AS_T1_ADMIN
echo $AS_T2_ADMIN
echo $AS_T2_MEMBER


# ---------------------------------------------------------- the actual test --

create_app() {
  api POST "$V/teams/$T1_ID/apps" \
    "$(jq -n --arg n "$APP_NAME" --arg p "$PASSWORD" \
      '{name: $n, category: "other", description: "default description", password: $p}')" \
    "$AS_T1_ADMIN"
}

step "2. Creating an app in team 1"
APP_NAME="external-app-o2o-$(rand_str 6)"
require 200 "" "create app '$APP_NAME'" create_app
APP_ID="$(jq -r '.user.id' <<<"$BODY")"
[[ -n "$APP_ID" && "$APP_ID" != null ]] || die "no app id in response: $BODY"
info "app id: $APP_ID"

add_collaborator() {
  api POST "$V/teams/$T2_ID/collaborators" \
    "$(jq -n --arg u "$APP_ID" \
      '{user: $u, permissions: ["create_team_conversation", "implicit_connection"]}')" \
    "$AS_T2_ADMIN"
}

remove_collaborator() {
  api DELETE "$V/teams/$T2_ID/collaborators/$APP_ID" "" "$AS_T2_ADMIN"
}

step "3. Adding the app to team 2 as a collaborator"
require 2xx "" "add collaborator" add_collaborator

post_one2one() {
  api POST "$V/one2one-conversations" \
    "$(jq -n --arg d "$DOMAIN" --arg u "$APP_ID" --arg t "$T2_ID" \
      '{name: "chit-chat", qualified_users: [{domain: $d, id: $u}],
        team: {teamid: $t, managed: false}}')" \
    "$AS_T2_MEMBER"
}

step "4. Team 2 member opens a 1:1 conversation with the app"
require 201 "" "create 1:1 conversation" post_one2one
CONV_ID="$(jq -r '.qualified_id.id' <<<"$BODY")"
CONV_DOMAIN="$(jq -r '.qualified_id.domain' <<<"$BODY")"
[[ -n "$CONV_ID" && "$CONV_ID" != null ]] || die "no conversation id in response: $BODY"
info "conversation: $CONV_DOMAIN/$CONV_ID"

get_conv() {
  api GET "$V/conversations/$CONV_DOMAIN/$CONV_ID" "" "$AS_T2_MEMBER"
}

get_conv_internal() {
  api GET "$GALLEY_URL/i/conversations/$CONV_ID" ""
}

get_mls_one2one() {
  api GET "$V/one2one-conversations/$DOMAIN/$APP_ID" "" "$AS_T2_MEMBER"
}

check 2xx "" "conversation is readable" get_conv
if $SKIP_INTERNAL; then
  skip "conversation exists according to galley's internal API"
else
  check 2xx "" "conversation exists according to galley's internal API" get_conv_internal
fi
check 2xx "" "MLS 1:1 conversation is reachable" get_mls_one2one

step "5. Removing the collaborator from team 2"
check 2xx "" "remove collaborator" remove_collaborator
check_eventually 404 "no-conversation" "conversation is gone" get_conv
if $SKIP_INTERNAL; then
  skip "conversation is gone according to galley's internal API"
else
  check_eventually 404 "no-conversation" \
    "conversation is gone according to galley's internal API" get_conv_internal
fi
check_eventually 403 "not-connected" "MLS 1:1 conversation is unreachable" get_mls_one2one

step "6. Adding the collaborator back to team 2"
check 2xx "" "re-add collaborator" add_collaborator
check_eventually 2xx "" "MLS 1:1 conversation is reachable again" get_mls_one2one

# ------------------------------------------------------------------ summary --

echo
if ((FAILURES == 0)); then
  echo "${C_GREEN}${C_BOLD}PASS${C_OFF} — $CHECKS checks, 0 failures"
else
  echo "${C_RED}${C_BOLD}FAIL${C_OFF} — $CHECKS checks, $FAILURES failures"
fi
exit $((FAILURES > 0))
