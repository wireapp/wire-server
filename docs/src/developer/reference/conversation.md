# Creating and populating conversations

Reference: {#RefCreateAndPopulateConvs}

_Author: Matthias Fischmann_

---

This will walk you through creating and populating a conversation
using [curl](https://curl.haxx.se/) commands and the credentials of an
ordinary user (member role).

If you have a system for identity management like a SAML IdP, you may
be able to use that as a source of user and group information, and
write a script or a program based on this document to keep your wire
conversations in sync with your groups.

Sidenote: in the future we may implement groups in our [SCIM
API](http://www.simplecloud.info/) to handle conversations.  For the
time being, we hope you consider the approach explained here a decent
work-around.


## Prerequisites

We will talk to the backend using the API that the clients use, so we
need a pseudo-user that we can authenticate as.  We assume this user
has been created by other means.  For the sake of testing, you could
just use the team admin (but you don't need admin privileges for this
user).

So here is some shell environment we will need:

```bash
export WIRE_BACKEND=https://prod-nginz-https.wire.com
export WIRE_USER=...
export WIRE_PASSWD=...
export WIRE_TEAMID=...
```

Now you can login and get a wire token to authenticate all further
requests:

```bash
export BEARER=$(curl -X POST \
  --header 'Content-Type: application/json' \
  --header 'Accept: application/json' \
  -d '{"email":"'"$WIRE_USER"'","password":"'"$WIRE_PASSWD"'"}' \
  $WIRE_BACKEND/login'?persist=false' | jq -r .access_token)
```

This token will be good for 15 minutes; after that, just repeat.

If you don't want to install [jq](https://stedolan.github.io/jq/), you
can just call the `curl` command and copy the access token into the
shell variable manually.

Here is a quick test that you're logged in:

```bash
curl -X GET --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/self
```


## Contact requests

If `$WIRE_USER` is in a team, all other team members are implicitly
connected to it.  So for the users in your team, you don't have to do
anything here.

TODO: contact requests to users not on the team.


## Conversations

To create a converation with no users (except the pseudo-user creating it):

```bash
export WIRE_CONV_NAME="The B-Team"
export WIRE_CONV='{
  "users": [],
  "name": "'${WIRE_CONV_NAME}'",
  "team": {
    "teamid": "'${WIRE_TEAMID}'"
  },
  "receipt_mode": 0,
  "message_timer": 0
}'

export CONV_ID=$(curl -X POST --header "Authorization: Bearer $BEARER" \
  -H "Content-Type: application/json" \
  -d "$WIRE_CONV" \
  $WIRE_BACKEND/conversations | jq -r .id)
```

The `users` field can contain UUIDs that need to point to existing
wire users (in your team or not), and `$WIRE_USER` needs to have an
accepted connection with them.  You can extract these ids from the
corresponding SCIM user records.  If in doubt, leave empty.

You can also add and remove users once the converation has been
created:

```bash
curl -X POST --header "Authorization: Bearer $BEARER" \
  -H "Content-Type: application/json" \
  -d '{ "users": ["b4b6a96c-70c8-11e9-99e6-f3ea044b132c", "b7293854-70c8-11e9-b620-97ff1eba6324"] }' \
  $WIRE_BACKEND/conversations/$CONV_ID/members

curl -X DELETE --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/conversations/$CONV_ID/members/b9f1c786-70c8-11e9-91a6-fbeb48cdcdd1
```

You can also look at one or all conversations:

```bash
curl -X GET --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/conversations/ids

curl -X GET --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/conversations/

curl -X GET --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/conversations/$CONV_ID/
```

Finally, conversations can be renamed or deleted:

```bash
curl -X PUT --header "Authorization: Bearer $BEARER" \
  -H "Content-Type: application/json" \
  -d '{ "name": "The C-Team" }' \
  $WIRE_BACKEND/conversations/$CONV_ID

curl -X DELETE --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/teams/$WIRE_TEAMID/conversations/$CONV_ID
```


## Advanced topics

TODO: pseudo-user leaving the conv, and being added by admin for changes later.
