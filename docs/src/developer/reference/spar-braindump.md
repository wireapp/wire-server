# Spar braindump

Reference: {#SparBrainDump}
/home/stefan/repos/wire-server/docs/src/how-to/install/includes/helm_dns-ingress-troubleshooting.inc.rst:147: WARNING: duplicate label trying things out, other instance in /home/stefan/repos/wire-server/docs/src/how-to/install/helm.md
/home/stefan/repos/wire-server/docs/src/how-to/install/includes/helm_dns-ingress-troubleshooting.inc.rst:170: WARNING: duplicate label troubleshooting, other instance in /home/stefan/repos/wire-server/docs/src/how-to/install/helm.md
/home/stefan/repos/wire-server/docs/src/developer/reference/config-options.md:33: WARNING: 'myst' reference target not found: ../../how-to/install/mls.md
/home/stefan/repos/wire-server/docs/src/developer/reference/spar-braindump.md:116: WARNING: 'myst' reference target not found: ../../how-to/single-sign-on/understand/main.rst
/home/stefan/repos/wire-server/docs/src/how-to/install/ansible-VMs.md:97: WARNING: undefined label: 'checks'
/home/stefan/repos/wire-server/docs/src/understand/federation/api.md:162: WARNING: 'myst' reference target not found: ../../how-to/install/mls
_Author: Matthias Fischmann_

---

this is a mix of information on inmplementation details, architecture,
and operation.  it should probably be sorted into different places in
the future, but if you can't find any more well-structured
documentation answering your questions, look here!


## related documentation

- [list of howtos for supported SAML IdP vendors](https://docs.wire.com/how-to/single-sign-on/index.html)
- [fragment](https://docs.wire.com/understand/single-sign-on/design.html)  (TODO: clean up the section "common misconceptions" below and move it here.)
- [official docs for team admin from customer support](https://support.wire.com/hc/en-us/categories/360000248538?section=administration%3Fsection%3Dadministration)  (skip to "Authentication")
- [talk scim using curl](https://github.com/wireapp/wire-server/blob/develop/docs/reference/provisioning/scim-via-curl.md)
- if you want to work on our saml/scim implementation and do not have access to [https://github.com/zinfra/backend-issues/issues?q=is%3Aissue+is%3Aopen+label%3Aspar] and [https://github.com/wireapp/design-specs/tree/master/Single%20Sign%20On], please get in touch with us.


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

copy these two files to one of your spar instances:

- `.../wire-server/hack/bin/register_idp_internal.sh`
- `${METADATA_FILE}`

...  and ssh into it.  then:

```sh
./register_idp_internal.sh metadata.xml ${TEAM_OWNER_ID}
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


### updating an idp via curl

Your IdP metadata may change over time (eg., because a certificate
expires, or because you change vendor and all the metadata looks
completely different), these changes need to be updated in wire.  We
offer two options to do that.

Like when creating your first IdP, for both options you need define a
few things:

```
# user id of an admin of the team (or the creator from the team info
# in the backoffice, if you only have the team id).
export ADMIN_ID=...

# path of the xml metadata file (if you only have the url, curl it)
export METADATA_FILE=...

# The ID of the IdP you want to update (login code without the `wire-`
# prefix, which is a UUIDv4):
export IDP_ID=...
```

Copy the new metadata file to one of your spar instances.


Ssh into it.  If you can't, {ref}`the sso docs <sso-main-documentation>` explain how you can create a
bearer token if you have the admin's login credentials.  If you follow
that approach, you need to replace all mentions of `-H'Z-User ...'`
with `-H'Authorization: Bearer ...'` in the following, and you won't need
`$ADMIN_ID`, but something like `$BEARER`.

There are two ways to update an IDP, described below, each with their own tradeoffs that affect users.

#### Option 1: Update the existing IdP in-place

Effects:

- You keep the login code, no visible changes for your users.
- The old IdP becomes immediately unaccessible.  It will disappear
  from team-settings, and users will have no way of using it for
  authentication.
- If the has no account on the new IdP, she won't be able to login.
- If a user has an account on the new IdP, *but not with exactly the
  same user name* (SAML NameID), she will not be logged in to their
  old account.  Instead, depending on your setup, a second account is
  created for them, or they are blocked (both not what you want).

```shell
curl -v \
     -XPUT http://localhost:8080/identity-providers/${IDP_ID} \
     -H"Z-User: ${ADMIN_ID}" \
     -H'Content-type: application/xml' \
     -d@"${METADATA_FILE}"
```

#### Option 2: Create a second IdP, and mark it as replacing the old one.

Effects:

- The new IdP will have a new login code.  Users need to be invited to
  use this new login code.
- If they use the old login code, they can keep using the old IdP as
  long as it is still running as before.  (This option is good for
  smooth transitions from one IdP to the other.)
- If they use the new login code, they will be automatically moved
  from the old IdP to the new one.  After that, they won't be able to
  use the old one any more.
- If a user logs into the team for the first time using the old login
  code, no user will be created.  The old IdP is marked as "replaced",
  and wire only authenticates existing users with it.
- This doesn't currently work if you are using SCIM for provisioning,
  because SCIM requires you to have exactly one IdP configured in your
  wire team.  (Internal details:
  https://github.com/zinfra/backend-issues/issues/1365,
  https://github.com/zinfra/backend-issues/issues/1377).
- If you go to team settings, you will see the old IdP and the new
  one, and there is currently no way to distinguish between replaced
  and active IdPs.  (Internal details:
  https://github.com/wireapp/wire-team-settings/issues/3465).

```
curl -v \
     -XPOST http://localhost:8080/identity-providers'?replaces='${IDP_ID} \
     -H"Z-User: ${ADMIN_ID}" \
     -H'Content-type: application/xml' \
     -d@"${METADATA_FILE}"
```


### deleting an idp via curl

Read the beginning of the last section up to "Option 1".  You need
`ADMIN_ID` (or `BEARER`) and `IDP_ID`, but not `METADATA_FILE`.

```
curl -v
     -XDELETE http://localhost:8080/identity-providers/${IDP_ID} \
     -H"Z-User: ${ADMIN_ID}" \
     -H'Content-type: application/json
```

If there are still users in your team with SAML credentials associated
with this IdP, you will get an error.  You can either move these users
elsewhere, delete them manually, or purge them implicitly during
deletion of the IdP:

```
curl -v
     -XDELETE http://localhost:8080/identity-providers/${IDP_ID}?purge=true \
     -H"Z-User: ${ADMIN_ID}" \
     -H'Content-type: application/json
```

Haskell code: https://github.com/wireapp/wire-server/blob/d231550f67c117b7d100c7c8c6c01b5ad13b5a7e/services/spar/src/Spar/API.hs#L217-L271


### setting a default SSO code

To avoid having to give users the login code, a backend can also provide a default code on the endpoint `/sso/settings`.
This needs to be set explicitly, since this is not always wanted and there might even be multiple idps (each with their own login code):

```
curl -XPUT ${API_URL}/i/sso/settings -H 'Content-Type: application/json' -d '{"default_sso_code":"e97fbe2e-eeb1-11e9-acf3-9ba77d8a04bf"}'
```

Note the lack of the `wire-` prefix.

This entry gets removed automatically when the corresponding idp is deleted. You can manually delete the default by making a `PUT` request as shown above with payload `{"default_sso_code":null}`.

Clients can then ask for the default SSO code on `/sso/settings` and use it to initiate single sign-on.


### common misconceptions


#### an email address can be one of two things

When users are SAML-authenticated with an email address under NameID,
that email address is used by wire as an opaque identifier, not to
send actual emails.  In order to *also* assign the user that email
address, you can enable the feature flag `validateSAMLemails`.  This
will trigger the regular email validation flow that is also triggered
when the user changes their email themselves.


#### scim, provisioning, metadata

for changing the user information (name, handle, email, ...), saml
isn't enough.  the identity management software (AD in this case, or
some add-on) needs to support scim.  we *could* support doing that via
saml, but the part of the standards that are needed for that are even
in worse shape than the ones for the authentication bits, and it would
not lead to a good user experience.  so instead we require users to
adopt the more robust and contemporary scim standard.


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
[`ManagedBy`](https://github.com/wireapp/wire-server/blob/010ca7e460d13160b465de24dd3982a397f94c16/libs/brig-types/src/Brig/Types/Common.hs#L393-L413).  This is a half-implemented feature for
managing conflicts between changes via scim vs. changes from wire
clients; and does currently not affect deletability of users.


#### delete via deleting idp

[Currently](https://github.com/wireapp/wire-server/blob/d231550f67c117b7d100c7c8c6c01b5ad13b5a7e/services/spar/src/Spar/API.hs#L217-L271), we only have the rest API for this.  Team settings will follow with a button.


#### user deletes herself

TODO


#### delete in team settings

TODO (probably little difference between this and "user deletes herself"?)


#### delete via scim

TODO
