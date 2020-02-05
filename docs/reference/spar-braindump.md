# the spar service for user provisioning (scim) and authentication (saml) - a brain dump

this is a mix of information on inmplementation details, architecture,
and operation.  it should probably be sorted into different places in
the future, but if you can't find any more well-structured
documentation answering your questions, look here!


## related documentation

- [list of howtos for supported SAML IdP vendors](https://docs.wire.com/how-to/single-sign-on/index.html)
- [fragment](https://docs.wire.com/understand/single-sign-on/design.html)  (TODO: clean up the section "common misconceptions" below and move it here.)
- [official docs for team admin from customer support](https://support.wire.com/hc/en-us/categories/360000248538?section=administration%3Fsection%3Dadministration)  (skip to "Authentication")
- [talk scim using curl](https://github.com/wireapp/wire-server/blob/develop/docs/reference/provisioning/scim-via-curl.md)
- if you want to work on our saml/scim implementation and do not have access to [https://github.com/wireapp/design-specs/tree/master/Single%20Sign%20On], please get in touch with us.


## operations

### enabling / disabling the sso feature for a team

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

the output contains the a json object representing the idp.  construct
the login code from the `id` field of that object by adding `wire-` in
front, eg.:

```
wire-e97fbe2e-eeb1-11e9-acf3-9ba77d8a04bf
```

give this login code to the users that you want to connect to wire
using this idp.  see
[here](https://support.wire.com/hc/en-us/articles/360000954617-Login-with-SSO)
on how to use the login code.

### setting a default SSO code

To avoid having to give users the login code, a backend can also provide a default code on the endpoint `/sso/settings`.
This needs to be set explicitly, since this is not always wanted and there might even be multiple idps (each with their own login code):

```
curl -XPUT ${API_URL}/i/sso/settings -H 'Content-Type: application/json' -d '{"default_sso_code":"e97fbe2e-eeb1-11e9-acf3-9ba77d8a04bf"}'
```

Note the lack of the `wire-` prefix.

This entry gets removed automatically when the idp is deleted, or manually by setting `{"default_sso_code":null}` as shown above.

Clients can then ask for the default SSO code on `/sso/settings` and use it to initiate single sign-on.


### troubleshooting

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


#### we don't support binding password/phone-auth'ed users to saml yet

to keep track of whether we have, see https://github.com/zinfra/backend-issues/issues/731



## application logic

### deleting users that exist on spar

For scim- or saml-created users, there are three locations for user data:

- `brig.user` (and a few things associated with that on brig and galley)
- `spar.user`
- `spar.scim_user`

The single source of truth is `brig.user`.  Dangling entries in the
other places are allowed, and must be checked by the application code
for danglingness.  ([test case for
scim](https://github.com/wireapp/wire-server/blob/010ca7e460d13160b465de24dd3982a397f94c16/services/spar/test-integration/Test/Spar/Scim/UserSpec.hs#L239-L308);
[test case for
saml](https://github.com/wireapp/wire-server/blob/293518655d7bae60fbcb0c4aaa06034785bfb6fc/services/spar/test-integration/Test/Spar/APISpec.hs#L742-L795))

For the semantics of interesting corner cases, consult [the test
suite](https://github.com/wireapp/wire-server/blob/develop/services/spar/test-integration/Test/Spar/APISpec.hs).
If you can't find what you're looking for there, please add at least a
pending test case explaining what's missing.

Side note: Users in brig carry an enum type
[`ManagedBy`](https://github.com/wireapp/wire-server/blob/010ca7e460d13160b465de24dd3982a397f94c16/libs/brig-types/src/Brig/Types/Common.hs#L393-L413);
see also {#DevScimOneWaySync}.  This is a half-implemented feature for
managing conflicts between changes via scim vs. changes from wire
clients; and does currently not affect deletability of users.


#### delete via deleting idp

[Currently](https://github.com/wireapp/wire-server/blob/010ca7e460d13160b465de24dd3982a397f94c16/services/spar/src/Spar/API.hs#L172-L187),
deleting an IdP does not delete any user data.  In particular:

- cookies of users that have authenticated via an IdP will remain valid if the IdP gets deleted.
- if a user authenticates via an IdP that has been deleted to obtain a new cookie, the login code will not work, and the user will never be able to login again.
- the user will still show in the team settings, and can be manually deleted from there.
- if a new idp is registered, and a user authenticates via that idp, the old user is unreachable.  (spar will look up the wire `UserId` under the saml user id that consists partly of the id of the new IdP, come up empty, and [create a fresh user on brig](https://github.com/wireapp/wire-server/blob/010ca7e460d13160b465de24dd3982a397f94c16/services/spar/src/Spar/App.hs#L306).)


#### user deletes herself

TODO


#### delete in team settings

TODO (probably little difference between this and "user deletes herself"?)


#### delete via scim

TODO
