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

#### gathering information

- find metadata for team in table `spar.idp_raw_metadata` via cqlsh
  (since https://github.com/wireapp/wire-server/pull/872)

- ask user for screenshots of the error message, or even better, for
  the text.  the error message contains lots of strings that you can
  grep for in the spar sources.


#### making spar work with a new IdP

often, new IdPs work out of the box, because there appears to be some
consensus about what minimum feature set everybody should support.

if there are problems: collect the metadata xml and an authentication
response xml (either from the browser http logs via a more technically
savvy customer; FUTUREWORK: it would be nice to log all saml response
xml files that spar receives in prod and cannot process).

https://github.com/wireapp/saml2-web-sso supports writing [unit vendor
compatibility
tests](https://github.com/wireapp/saml2-web-sso/blob/ff9b9f445475809d1fa31ef7f2932caa0ed31613/test/Test/SAML2/WebSSO/APISpec.hs#L266-L329)
against that response value.  once that test passes, it should all
work fine.


### common misconceptions


#### an email address can be one of two things

the email used for saml auth is only a name, and never used for
sending out emails, and does not show as the email address of the user
in the team settings.

RATIONALE: emails that are passed in from an external identity
provider must be trusted, so the user cannot have them as an actual
email address that wire is sending emails to.

POSSIBLE FEATURE: we could authenticate the emails sent in from the
identity provider in the same way we are doing that for
password-authenticated non-team users: email receives a link
containing a crypto token, user clicks on link if the email is
authentic, email gets authenticated.


#### scim, provisioning, metadata

for changing the user information (name, handle, email, ...), saml
isn't enough.  the identity management software (AD in this case, or
some add-on) needs to support scim.  we *could* support doing that via
saml, but the part of the standards that are needed for that are even
in worse shape than the ones for the authentication bits, and it would
not lead to a good user experience.  so instead we require users to
adopt the more robust and contemporary scim standard.
