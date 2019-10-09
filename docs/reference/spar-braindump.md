# the spar service for user provisioning (scim) and authentication (saml) - a brain dump

this is a mix of information on inmplementation details, architecture,
and operation.  it should probably be sorted into different places in
the future, but if you can't find any more well-structured
documentation answering your questions, look here!


## operations

### enabeling / disabeling the sso feature for a team

if you have sso disabled by default, you need to turn on the feature
for every team that wants to use it.  you can do this in the stern
service (aka backoffice).  look for `get/put
/teams/{tid}/features/sso`


### registering an idp with a team via curl

you need to have:

```sh
# user id of an admin of the team (or the creator from the team info
# in the backoffice, if you only have the team id).
export ADMIN_ID=...

# path of the xml metadata file (if you only have the url, curl it)
export METADATA_FILE=...
```

then you need to do:

```sh
./khan.me scp upload -e prod -r spar -s .../wire-server/deploy/services-demo/register_idp_internal.sh -d ./register.sh
./khan.me scp upload -e prod -r spar -s ${METADATA_FILE} -d ./metadata.xml
./khan.me ssh -e prod -r spar
export TEAM_OWNER_ID=...
./register.sh metadata.xml $TEAM_OWNER_ID
```

the output contains the a json object representing the idp.  safe the `id` field of that object.

the sso login code needed by the users to authenticate via saml is

```
wire-<that-id>
```


### trouble shooting

...?
