# Single sign-on and user provisioning

> ##### Contents
> 
> * [Single sign-on and user provisioning](#single-sign-on-and-user-provisioning)
>   * [Introduction](#introduction)
>   * [Further reading](#further-reading)
>   * [Definitions](#definitions)
>   * [User login for the first time with SSO](#user-login-for-the-first-time-with-sso)
>   * [SAML/SSO](#saml-sso)
>     * [Introduction](#id2)
>     * [Terminology and concepts](#terminology-and-concepts)
>     * [Setting up SSO externally](#setting-up-sso-externally)
>     * [Setting up SSO internally](#setting-up-sso-internally)
>   * [User provisioning (SCIM/LDAP)](#user-provisioning-scim-ldap)
>     * [SCIM management in Wire (in Team Management)](#scim-management-in-wire-in-team-management)
>       * [SCIM security and authentication](#scim-security-and-authentication)
>       * [Generating a SCIM token](#generating-a-scim-token)
>     * [Using SCIM via Curl](#using-scim-via-curl)
>       * [Creating a SCIM token](#creating-a-scim-token)
>       * [Associating SCIM tokens with SAML IdPs for authentication](#associating-scim-tokens-with-saml-idps-for-authentication)
>       * [Using a SCIM token to Create Read Update and Delete (CRUD) users](#using-a-scim-token-to-create-read-update-and-delete-crud-users)

## Introduction

This page is intended as a manual for administrator users in need of setting up [SSO](#term-SSO) and provisionning users using [SCIM](#term-SCIM) on their installation of Wire.

Historically and by default, Wire’s user authentication method is via phone or password. This has security implications and does not scale.

Solution: [SSO](#term-SSO) with [SAML](#term-SAML)! [(Security Assertion Markup Language)](https://en.wikipedia.org/wiki/Security_Assertion_Markup_Language)

[SSO](#term-SSO) systems allow users to identify on multiple systems (including Wire once configured as such) using a single ID and password.

You can find some of the advantages of [SSO](#term-SSO) over more traditional schemes [here](https://en.wikipedia.org/wiki/Single_sign-on).

Also historically, wire has allowed team admins and owners to manage their users in the team management app.

This does not scale as it requires a lot of manual labor for each user.

The solution we offer to solve this issue is implementing [SCIM](#term-SCIM) [(System for Cross-domain Identity Management)](https://en.wikipedia.org/wiki/System_for_Cross-domain_Identity_Management)

[SCIM](#term-SCIM) is an interface that allows both software (for example Active Directory) and custom scripts to manage Identities (users) in bulk.

This page explains how to set up [SCIM](#term-SCIM) and then use it.

#### NOTE
Note that it is recommended to use both [SSO](#term-SSO) and [SCIM](#term-SCIM) (as opposed to just [SSO](#term-SSO) alone).
The reason is if you only use [SSO](#term-SSO), but do not configure/implement [SCIM](#term-SCIM), you will experience reduced functionality.
In particular, without [SCIM](#term-SCIM) all Wire users will be named according their e-mail address and won’t have any rich profiles.
See below in the [SCIM](#term-SCIM) section for a more detailled explanation.

## Further reading

If you can’t find the answers to your questions here, we have a few
more documents.  Some of them are very technical, some may not be up
to date any more, and we are planning to move many of them into this
page.  But for now they may be worth checking out.

- [Trouble shooting & FAQ](../trouble-shooting.md#trouble-shooting-faq)
- [https://support.wire.com/hc/en-us/sections/360000580658-Authentication](https://support.wire.com/hc/en-us/sections/360000580658-Authentication)
- [https://github.com/wireapp/wire-server/blob/1753b790e5cfb2d35e857648c88bcad3ac329f01/docs/reference/spar-braindump.md](https://github.com/wireapp/wire-server/blob/1753b790e5cfb2d35e857648c88bcad3ac329f01/docs/reference/spar-braindump.md)
- [https://github.com/wireapp/wire-server/tree/1753b790e5cfb2d35e857648c88bcad3ac329f01/docs/reference/provisioning/](https://github.com/wireapp/wire-server/tree/1753b790e5cfb2d35e857648c88bcad3ac329f01/docs/reference/provisioning/)

## Definitions

The following concepts need to be understood to use the present manual:

<a id="term-SCIM"></a>

SCIM
: System for Cross-domain Identity Management ([SCIM](#term-SCIM)) is a standard for automating the exchange of user identity information between identity domains, or IT systems.
  <br/>
  One example might be that as a company onboards new employees and separates from existing employees, they are added and removed from the company’s electronic employee directory. [SCIM](#term-SCIM) could be used to automatically add/delete (or, provision/de-provision) accounts for those users in external systems such as G Suite, Office 365, or Salesforce.com. Then, a new user account would exist in the external systems for each new employee, and the user accounts for former employees might no longer exist in those systems.
  <br/>
  See: [System for Cross-domain Identity Management at Wikipedia](https://en.wikipedia.org/wiki/System_for_Cross-domain_Identity_Management)
  <br/>
  In the context of Wire, SCIM is the interface offered by the Wire service (in particular the spar service) that allows for single or mass automated addition/removal of user accounts.

<a id="term-SSO"></a>

SSO
: Single sign-on ([SSO](#term-SSO)) is an authentication scheme that allows a user to log in with a single ID and password to any of several organizationally related, yet independent, software systems.
  <br/>
  True single sign-on allows the user to log in once and access different, independent services without re-entering authentication factors.
  <br/>
  See: [Single-Sign-On at Wikipedia](https://en.wikipedia.org/wiki/Single_sign-on)

<a id="term-SAML"></a>

SAML
: Security Assertion Markup Language ([SAML](#term-SAML), pronounced SAM-el, /’sæməl/) is an open standard for exchanging authentication and authorization data between parties, in particular, between an identity provider and a service provider. [SAML](#term-SAML) is an XML-based markup language for security assertions (statements that service providers use to make access-control decisions). [SAML](#term-SAML) is also:
  <br/>
  * A set of XML-based protocol messages
  * A set of protocol message bindings
  * A set of profiles (utilizing all of the above)
  <br/>
  An important use case that [SAML](#term-SAML) addresses is web-browser [single sign-on (SSO)](https://en.wikipedia.org/wiki/Single_sign-on) . Single sign-on is relatively easy to accomplish within a security domain (using cookies, for example) but extending [SSO](#term-SSO) across security domains is more difficult and resulted in the proliferation of non-interoperable proprietary technologies. The [SAML Web Browser SSO](https://en.wikipedia.org/wiki/Single_sign-on) profile was specified and standardized to promote interoperability.
  <br/>
  See: [SAML at Wikipedia](https://en.wikipedia.org/wiki/Security_Assertion_Markup_Language)
  <br/>
  In the context of Wire, SAML is the standard/protocol used by the Wire services (in particular the spar service) to provide the Single Sign On feature.

<a id="term-IdP"></a>

IdP
: In the context of Wire, an identity provider (abbreviated [IdP](#term-IdP)) is a service that provides SAML single sign-on ([SSO](#term-SSO)) credentials that give users access to Wire.

<a id="term-Curl"></a>

Curl
: [Curl](#term-Curl) (pronounced “[Curl](#term-Curl)”) is a command line tool used to download files over the HTTP (web) protocol. For example, curl http://wire.com will download the `wire.com` web page.
  <br/>
  In this manual, it is used to contact API (Application Programming Interface) endpoints manually, where those endpoints would normally be accessed by code or other software.
  <br/>
  This can be used either for illustrative purposes (to “show” how the endpoints can be used) or to allow the manual execution of some simple tasks.
  <br/>
  For example (not a real endpoint) curl http://api.wire.com/delete_user/thomas would (schematically) execute the [Curl](#term-Curl) command, which would contact the wire.com API and delete the user named “thomas”.
  <br/>
  Running this command in a terminal would cause the [Curl](#term-Curl) command to access this URL, and the API at that URL would execute the requested action.
  <br/>
  See: [curl at Wikipedia](https://en.wikipedia.org/wiki/Curl)

<a id="term-Spar"></a>

Spar
: The Wire backend software stack is composed of different services, [running as pods](../../overview.md#focus-on-pods) in a kubernetes cluster.
  <br/>
  One of those pods is the “spar” service. That service/pod is dedicated to the providing [SSO](#term-SSO) (using [SAML](#term-SAML)) and [SCIM](#term-SCIM) services. This page is the manual for this service.
  <br/>
  In the context of [SCIM](#term-SCIM), Wire’s spar service is the [Service Provider](https://en.wikipedia.org/wiki/Service_provider_(SAML)) that Identity Management Software
  (for example Azure, Okta, Ping Identity, SailPoint, Technology Nexus, etc.) uses for user account provisioning and deprovisioning.

## User login for the first time with SSO

[SSO](#term-SSO) allows users to register and log into Wire with their company credentials that they use on other software in their workplace.
No need to remember another password.

When a team is set up on Wire, the administrators can provide users a login code or link that they can use to go straight to their company’s login page.

Here is what this looks from a user’s perspective:

1. Download Wire.
2. Select and copy the code that your company gave you / the administrator generated
3. Open Wire.  Wire may detect the code on your clipboard and open a pop-up window with a text field.
   Wire will automatically put the code into the text field.
   If so, click Log in and go to step 8.
4. If no pop-up: click Login on the first screen.
5. Click Enterprise Login.
6. A pop-up will appear. In the text field, paste or type the code your company gave you.
7. Click Log in.
8. Wire will load your company’s login page: log in with your company credentials.

<a id="saml-sso"></a>

## SAML/SSO

### Introduction

SSO (Single Sign-On) is technology allowing users to sign into multiple services with a single identity provider/credential.

SSO is about `authentication`, not `provisioning` (create, update, remove user accounts).  To learn more about the latter, continue  [below](#user-provisioning-scim-ldap).

For example, if a company already has SSO setup for some of their services, and they start using Wire, they can use Wire’s SSO support to add Wire to the set of services their users will be able to sign into with their existing SSO credentials.

Here is a blog post we like about how SAML works: [https://duo.com/blog/the-beer-drinkers-guide-to-saml](https://duo.com/blog/the-beer-drinkers-guide-to-saml)

And here is a diagram that explains it in slightly more technical terms:

Here is a critique of XML/DSig security (which SAML relies on): [https://www.cs.auckland.ac.nz/~pgut001/pubs/xmlsec.txt](https://www.cs.auckland.ac.nz/~pgut001/pubs/xmlsec.txt)

### Terminology and concepts

- Transport:
  The browser carrries out all the redirections from the SP to the IdP and vice versa.
- Service Provider (SP): The entity (here Wire software) that provides its protected resource when an end user tries to access this resource. To accomplish the SAML based SSO authentication, the Service Provider
  must have the Identity Provider’s metadata.
- Identity Provider (IdP): Defines the entity that provides the user identities, including the ability to authenticate a user to get access to a protected resource / application from a Service Provider. To accomplish
  the SAML based SSO authentication, the IdP must have the Service Provider’s metadata.
- SAML Request: This is the authentication request generated by the Service Provider to request an authentication from the Identity Provider for verifying the user’s identity.
- SAML Response: The SAML Response contains the cryptographically signed assertion of the authenticated user and is generated by the Identity Provider.

(Definitons adapted from [collab.net](http://help.collab.net/index.jsp?topic=/teamforge178/action/saml.html))

<a id="setting-up-sso-externally"></a>

### Setting up SSO externally

To set up [SSO](#term-SSO) for a given Wire installation, the Team owner/administrator must enable it.

The first step is to configure the Identity Provider: you’ll need to register Wire as a service provider in your Identity Provider.

We’ve put together guides for registering with different providers:

- Instructions for [Okta](../okta/main.md#sso-int-with-okta)
- Instructions for [Centrify](../centrify/main.md)
- Instructions for [Azure](../azure/main.md)
- Some screenshots for [ADFS](../adfs/main.md)
- [Generic instructions (try this if none of the above are applicable)](../generic-setup.md)

As you do this, make sure you take note of your [IdP](#term-IdP) metadata, which you will need for the next step.

Once you are finished with registering Wire to your [IdP](#term-IdP), move on to the next step, setting up [SSO](#term-SSO) internally.

### Setting up SSO internally

Now that you’ve registered Wire with your identity provider ([IdP](#term-IdP)), you can enable [SSO](#term-SSO) for your team on Wire.

On Desktop:

- Click Settings and click “Manage Team”; or go directly to teams.wire.com, or if you have an on-premise install, go to teams.<your-domain>.com
- Login with your account credentials.
- Click “Customization”. Here you will see the section for [SSO](#term-SSO).
- Click the blue down arrow.
- Click “Add [SAML](#term-SAML) Connection”.
- Provide the [IdP](#term-IdP) metadata. To find out more about retrieving this for your provider, see the guides in the “Setting up [SSO](#term-SSO) externally” step just above.
- Click “Save”.
- Wire will now validate the document to set up the [SAML](#term-SAML) connection.
- If the data is valid, you will return to the Settings page.
- The page shows the information you need to log in with [SSO](#term-SSO). Copy the login code or URL and send it to your team members or partners. For more information see: Logging in with [SSO](#term-SSO).

What to expect after [SSO](#term-SSO) is enabled:

Anyone with a login through your [SAML](#term-SAML) identity provider ([IdP](#term-IdP)) and with access to the Wire app will be able to register and log in to your team using the [SSO](#term-SSO) Login URL and/or Code.

Take care to share the code only with members of your team.

If you haven’t set up [SCIM](#term-SCIM) ([we recommend you do]()), your team members can create accounts on Wire using [SSO](#term-SSO) simply by logging in, and will appear on the People tab of the team management page.

If team members already have Wire accounts, use [SCIM](#term-SCIM) to associate them with the [SAML](#term-SAML) credentials.  If you make a mistake here, you may end up with several accounts for the same person.

<a id="user-provisioning-scim-ldap"></a>

## User provisioning (SCIM/LDAP)

SCIM/LDAP is about `provisioning` (create, update, remove user accounts), not `authentication`.  To learn more about the latter, continue [above](#saml-sso).

Wire supports the [SCIM](http://www.simplecloud.info/) ([RFC 7643](https://tools.ietf.org/html/rfc7643)) protocol to create, update and delete users.

If your user data is stored in an LDAP data source like Active Directory or OpenLDAP, you can use our docker-base [ldap-scim-bridge](https://github.com/wireapp/ldap-scim-bridge/#use-via-docker) to connect it to wire.

Note that connecting a SCIM client to Wire also disables the functionality to create new users in the SSO login process. This functionality is disabled when a token is created (see below) and re-enabled when all tokens have been deleted.

To set up the connection of your SCIM client (e.g. Azure Active Directory) you need to provide

1. The URL under which Wire’s SCIM API is hosted: `https://prod-nginz-https.wire.com/scim/v2`.
   If you are hosting your own instance of Wire then the URL is `https://<hostname>/scim/v2`, where `<hostname>` is where you are serving Wire’s public endpoints. Some SCIM clients append `/v2` to the URL your provide. If this happens (check the URL mentioned in error messages of your SCIM client) then please provide the URL without the `/v2` suffix, i.e. `https://prod-nginz-https.wire.com/scim` or `https://<hostname>/scim`.
2. A secret token which authorizes the use of the SCIM API. Use the  [wire_scim_token.py](https://raw.githubusercontent.com/wireapp/wire-server/654b62e3be74d9dddae479178990ebbd4bc77b1e/docs/reference/provisioning/wire_scim_token.py)
   script to generate a token. To run the script you need access to an user account with “admin” privileges that can login via email and password. Note that the token is independent from  the admin account that created it, i.e. the token remains valid if the admin account gets deleted or changed.

You need to configure your SCIM client to use the following mandatory SCIM attributes:

1. Set the `userName` attribute to the desired user handle (the handle is shown
   with an @ prefix in apps). It must be unique accross the entire Wire Cloud
   (or unique on your own instance), and consist of the characters `a-z0-9_.-`
   (no capital letters).
2. Set the `displayName` attribute to the user’s desired display name, e.g. “Jane Doe”.
   It must consist of 1-128 unicode characters. It does not need to be unique.
3. The `externalId` attribute:
   1. If you are using Wire’s SAML SSO feature then set `externalId` attribute to the same identifier used for `NameID` in your SAML configuration (suppored `NameID` types are `email` and `unspecified`).
   2. If you are using email/password authentication then set the `externalId`
      attribute to the user’s email address. The user will receive an invitation email during provisioning. Also note that the account will be set to `"active": false` until the user has accepted the invitation and activated the account.

You can optionally make use of Wire’s `urn:wire:scim:schemas:profile:1.0` extension field to store arbitrary user profile data that is shown in the users profile, e.g. department, role. See [docs](https://github.com/wireapp/wire-server/blob/develop/docs/reference/user/rich-info.md#scim-support-refrichinfoscim) for details.

### SCIM management in Wire (in Team Management)

#### SCIM security and authentication

Wire uses a very basic variant of oauth, where a *bearer token* is presented to the server in header with all [SCIM](#term-SCIM) requests.

You can create such bearer tokens in team management and copy them from there into your the dashboard of your SCIM data source.

#### Generating a SCIM token

In order to be able to send SCIM requests to Wire, we first need to generate a SCIM token. This section explains how to do this.

Once the token is generated, it should be noted/remembered, and it will be used in all subsequent SCIM uses/requests to authenticate the request as valid/authenticated.

These are the steps to generate a new [SCIM](#term-SCIM) token, which you will need to provide to your identity provider ([IdP](#term-IdP)), along with the target API URL, to enable [SCIM](#term-SCIM) provisionning.

- Step 1: Go to [https://teams.wire.com/settings](https://teams.wire.com/settings) (Here replace “wire.com” with your own domain if you have an on-premise installation of Wire).

![image](token-step-01.png)
- Step 2: In the left menu, go to “Customization”.

![image](token-step-02.png)
- Step 3: Go to “Automated User Management ([SCIM](#term-SCIM))” and click the “down” to expand

![image](token-step-03.png)
- Step 4: Click “Generate token”, if your password is requested, enter it.

![image](token-step-04.png)
- Step 5: Once the token is generated, copy it into your clipboard and store it somewhere safe (eg., in the dashboard of your SCIM data source).

![image](token-step-05.png)
- Step 6: You’re done!  You can now view token information, delete the token, or create more tokens should you need them.

![image](token-step-06.png)

Tokens are now listed in this [SCIM](#term-SCIM)-related area of the screen, you can generate up to 8 such tokens.

### Using SCIM via Curl

You can use the [Curl](#term-Curl) command line HTTP tool to access the wire backend (in particular the `spar` service) through the [SCIM](#term-SCIM) API.

This can be helpful to write your own tooling to interface with wire.

#### Creating a SCIM token

Before we can send commands to the [SCIM](#term-SCIM) API/Spar service, we need to be authenticated. This is done through the creation of a [SCIM](#term-SCIM) token.

First, we need a little shell environment. Run the following in your terminal/shell:

```bash
 export WIRE_BACKEND=https://prod-nginz-https.wire.com
 export WIRE_ADMIN=...
 export WIRE_PASSWD=...
```

Wire’s SCIM API currently supports a variant of HTTP basic auth.

In order to create a token in your team, you need to authenticate using your team admin credentials.

The way this works behind the scenes in your browser or cell phone, and in plain sight if you want to use curl, is you need to get a Wire token.

First install the `jq` command ([https://stedolan.github.io/jq/](https://stedolan.github.io/jq/)):

```bash
sudo apt install jq
```

#### NOTE
If you don’t want to install `jq`, you can just call the `curl` command and copy the access token into the shell variable manually.

Then run:

```bash
export BEARER=$(curl -X POST \
--header 'Content-Type: application/json' \
--header 'Accept: application/json' \
-d '{"email":"'"$WIRE_ADMIN"'","password":"'"$WIRE_PASSWD"'"}' \
$WIRE_BACKEND/login'?persist=false' | jq -r .access_token)
```

This token will be good for 15 minutes; after that, just repeat the command above to get a new token.

#### NOTE
SCIM requests are authenticated with a SCIM token, see below. SCIM tokens and Wire tokens are different things.

A Wire token is necessary to get a SCIM token. SCIM tokens do not expire, but need to be deleted explicitly.

You can test that you are logged in with the following command:

```bash
curl -X GET --header "Authorization: Bearer $BEARER" $WIRE_BACKEND/self
```

Now you are ready to create a SCIM token:

```bash
export SCIM_TOKEN_FULL=$(curl -X POST \
--header "Authorization: Bearer $BEARER" \
--header 'Content-Type: application/json;charset=utf-8' \
-d '{ "description": "test '"`date`"'", "password": "'"$WIRE_PASSWD"'" }' \
$WIRE_BACKEND/scim/auth-tokens)
export SCIM_TOKEN=$(echo $SCIM_TOKEN_FULL | jq -r .token)
export SCIM_TOKEN_ID=$(echo $SCIM_TOKEN_FULL | jq -r .info.id)
```

The SCIM token is now contained in the `SCIM_TOKEN` environment variable.

EXPERT HINT: If you are a site operator and have the ok from the team admin, but not their password, you can try something like this (WARNING!  TEST THIS ON NON-PROD ACCOUNTS FIRST!):

```default
export id="$(uuid -v4)"
export token_="$(dd if=/dev/random bs=32 count=1 | base64)"
export created_at='2023-05-30 11:15:00+0000'
export team=...
export descr=...
echo 'INSERT INTO spar.team_provisioning_by_token (token_, team, id, created_at, idp, descr) VALUES ('\'$token_\', $team, $id, \'$created_at\', NULL, \'$descr\'');'
echo 'INSERT INTO spar.team_provisioning_by_team (token_, team, id, created_at, idp, descr) VALUES ('\'$token_\', $team, $id, \'$created_at\', NULL, \'$descr\'');'
echo 'SELECT * FROM spar.team_provisioning_by_team WHERE '"team = $team AND id = $id;"
echo 'SELECT * FROM spar.team_provisioning_by_token WHERE '"token_ = $token_;"
echo 'DELETE FROM spar.team_provisioning_by_team WHERE '"team = $team AND id = $id;"
echo 'DELETE FROM spar.team_provisioning_by_token WHERE '"token_ = $token_;"
```

You can look it up again with:

```bash
curl -X GET --header "Authorization: Bearer $BEARER" \
$WIRE_BACKEND/scim/auth-tokens
```

And you can delete it with:

```bash
curl -X DELETE --header "Authorization: Bearer $BEARER" \
$WIRE_BACKEND/scim/auth-tokens?id=$SCIM_TOKEN_ID
```

#### Associating SCIM tokens with SAML IdPs for authentication

You can create both multiple SAML IdPs and multiple SCIM peers.  If
both numbers are non-zero, there is the question of how the
provisioned users should be authenticated: should they be invited via
password, and other users authenticated *and* provisioned by SAML?  Or
should they be provisioned so they can login with their SAML
credentials?

The request body in the [scim token creation
request](https://staging-nginz-https.zinfra.io/v7/api/swagger-ui/#/default/auth-tokens-create)
has an optional parameter `idp` that can contain an IdP’s UUID.  This
allows you to associate a SCIM token with a SAML IdP at creation of
the scim token.

If you already have a SCIM token and want to associate it with a SAML
IdP, delete the SCIM token and create a new one.  The user accounts
provisioned with that token will remain unaffected.

If you do not provide a SAML IdP when creating it, the behavior
differs based on the version you use:

**V6 and below:** If there is a unique IdP registered with your team,
associate implicitly.  Otherwise, do not associate.

**V7 and above:** Never associate unless explicitly told to do so.

#### Using a SCIM token to Create Read Update and Delete (CRUD) users

Now that you have your SCIM token, you can use it to talk to the SCIM API to manipulate (create, read, update, delete) users, either individually or in bulk.

**JSON encoding of SCIM Users**

In order to manipulate users using commands, you need to specify user data.

A minimal definition of a user is written in JSON format and looks like this:

```json
{
   "schemas"     : ["urn:ietf:params:scim:schemas:core:2.0:User"],
   "externalId"  : "nick@example.com",
   "userName"    : "nick",
   "displayName" : "The Nick"
}
```

You can store it in a variable using this sort of command:

```bash
export SCIM_USER='{
   "schemas"     : ["urn:ietf:params:scim:schemas:core:2.0:User"],
   "externalId"  : "nick@example.com",
   "userName"    : "nick",
   "displayName" : "The Nick"
}'
```

The `externalId` is used to construct a SAML identity.  Two cases are
currently supported:

1. `externalId` contains a valid email address.
   The SAML `NameID` has the form `<NameID Format="urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress">me@example.com</NameID>`.
2. `externalId` contains anything that is *not* an email address.
   The SAML `NameID` has the form `<NameID Format="urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified">...</NameID>`.

#### NOTE
It is important to configure your SAML provider to use `nameid-format:emailAddress` or `nameid-format:unspecified`.  Other nameid formats are not supported at this moment.

See [FAQ](../../../understand/single-sign-on/trouble-shooting.md#how-should-i-map-user-data-to-scim-attributes-when-provisioning-users-via-scim)

We also support custom fields that are used in rich profiles in this form (see: [https://github.com/wireapp/wire-server/blob/develop/docs/reference/user/rich-info.md](https://github.com/wireapp/wire-server/blob/develop/docs/reference/user/rich-info.md)):

```bash
 export SCIM_USER='{
   "schemas"     : ["urn:ietf:params:scim:schemas:core:2.0:User", "urn:wire:scim:schemas:profile:1.0"],
   "externalId"  : "rnick@example.com",
   "userName"    : "rnick",
   "displayName" : "The Rich Nick",
   "urn:wire:scim:schemas:profile:1.0": {
      "richInfo": [
         {
         "type": "Department",
         "value": "Sales & Marketing"
         },
         {
         "type": "Favorite color",
         "value": "Blue"
         }
      ]
   }
 }'
```

**How to create a user**

You can create a user using the following command:

```bash
 export STORED_USER=$(curl -X POST \
  --header "Authorization: Bearer $SCIM_TOKEN" \
  --header 'Content-Type: application/json;charset=utf-8' \
  -d "$SCIM_USER" \
  $WIRE_BACKEND/scim/v2/Users)
 export STORED_USER_ID=$(echo $STORED_USER | jq -r .id)
```

Note that `$SCIM_USER` is in the JSON format and is declared before running this commend as described in the section above.

**Get a specific user**

```bash
 curl -X GET \
   --header "Authorization: Bearer $SCIM_TOKEN" \
   --header 'Content-Type: application/json;charset=utf-8' \
   $WIRE_BACKEND/scim/v2/Users/$STORED_USER_ID
```

**Search a specific user**

SCIM user search is quite flexible.  Wire currently only supports lookup by wire handle or email address.

Email address (and/or SAML NameID, if /a):

```bash
 curl -X GET \
   --header "Authorization: Bearer $SCIM_TOKEN" \
   --header 'Content-Type: application/json;charset=utf-8' \
   $WIRE_BACKEND/scim/v2/Users/'?filter=externalId%20eq%20%22me%40example.com%22'
```

Wire handle: same request, just replace the query part with

```bash
'?filter=userName%20eq%20%22me%22'
```

**Update a specific user**

For each put request, you need to provide the full json object.  All omitted fields will be set to `null`.  (If you do not have an up-to-date user present, just `GET` one right before the `PUT`.)

```bash
 export SCIM_USER='{
   "schemas"     : ["urn:ietf:params:scim:schemas:core:2.0:User"],
   "externalId"  : "rnick@example.com",
   "userName"    : "newnick",
   "displayName" : "The New Nick"
 }'
```

```bash
 curl -X PUT \
  --header "Authorization: Bearer $SCIM_TOKEN" \
  --header 'Content-Type: application/json;charset=utf-8' \
  -d "$SCIM_USER" \
  $WIRE_BACKEND/scim/v2/Users/$STORED_USER_ID
```

**Deactivate user**

It is possible to temporarily deactivate an user (and reactivate him later) by setting his `active` property to `true/false` without affecting his device history.  (`active=false` changes the wire user status to `suspended`.)

**Delete user**

```bash
 curl -X DELETE \
   --header "Authorization: Bearer $SCIM_TOKEN" \
   $WIRE_BACKEND/scim/v2/Users/$STORED_USER_ID
```
