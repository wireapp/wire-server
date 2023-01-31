(trouble-shooting-faq)=

```{contents}
:depth: 2
```

# Trouble shooting & FAQ

## Reporting a problem with user provisioning or SSO authentication

In order for us to analyse and understand your problem, we need at least the following information up-front:

- Have you followed the following instructions?
    - {ref}`FAQ <trouble-shooting-faq>` (This document)
    - [Howtos](https://docs.wire.com/how-to/single-sign-on/index.html) for supported vendors
    - [General documentation on the setup flow](https://support.wire.com/hc/en-us/articles/360001285718-Set-up-SSO-externally)
- Which vendor (and product if /a) are you using (octa, azure, centrica, other (which one)?)
- Team ID (looks like eg. `2e9a9c9c-6f83-11eb-a118-3342c6f16f4e`, can be found in team settings)
- User ID of the account that has the problem (alternatively: handle, email address)
- What do you expect to happen?
    - eg.: "I enter login code, authenticate successfully against IdP, get redirected, and see the wire landing page."
- What does happen instead?
    - Screenshots
    - Copy the text into your report where applicable in addition to screenshots (for automatic processing).
    - eg.: "instead of being logged into wire, I see the following error page: ..."
- Screenshots of the Configuration (both SAML and SCIM, as applicable), including, but not limited to:
    - If you are using SAML: SAML IdP metadata file
    - If you are using SCIM for provisioning: Which attributes in the User schema are mapped?  How?
- If you have successfully authenticated on your IdP and are
  redirected into wire, and then see a white page with an error
  message that contains a lot of machine-readable info: copy the full
  message to the clipboard and insert it into your report.  We do not
  log this information for privacy reasons, but we can use it to
  investigate your problem.  (Hint, if you want to investigate
  yourself: it's base64 encoded!

### notes for wire support / development

Not officially supported IdP vendors may work out of the box, as we
are requiring a minimum amount of SAML features.

If there are problems: collect the metadata xml and an authentication
response xml (either from the browser http logs via a more technically
savvy customer; or from the "white page with an error" mentioned
above.

https://github.com/wireapp/saml2-web-sso supports writing [unit vendor
compatibility
tests](https://github.com/wireapp/saml2-web-sso/blob/ff9b9f445475809d1fa31ef7f2932caa0ed31613/test/Test/SAML2/WebSSO/APISpec.hs#L266-L329)
against that response value.  once that test passes, it should all
work fine.


## Can I use SCIM without SAML?

TODO: move https://docs.wire.com/developer/reference/spar-braindump.html#scim-without-saml here!


## Can I use SAML without SCIM?

Yes, but this is not recommended.  User (de-)provisioning requires more manual work without SCIM, and some of the account information cannot be provisioned at all via SAML.


## Can I use the same SSO login code for multiple teams?

TODO: this works, but we need to change the docs everywhere to not use
the old api!

No, but there is a good reason for it and a work-around.

Reason: we *could* implement this, but that would require that we
disable implicit user creation for those teams.  Implicit user
creation means that a person who has never logged onto wire before can
use her credentials for the IdP to get access to wire, and create a
new user based on those credentials.  In order for this to work, the
IdP must uniquely determine the team.

Work-around: on your IdP dashboard, you can set up a separate app for
every wire team you own.  Each IdP will get a different metadata file,
and can be registered with its target team only.  This way, users from
different teams have different SSO logins, but the IdP operators can
still use the same user base for all teams.  This has the extra
advantage that a user can be part of two teams with the same
credentials, which would be impossible even with the hypothetical fix.


## Can an existing user without IdP (or with a different IdP) be bound to a new IdP?

TODO: yes, but you need scim!  it's documented in spar-braindump, move that content here.

## Can the SSO feature be disabled for a team?

No, this is [not implemented](https://github.com/wireapp/wire-server/blob/7a97cb5a944ae593c729341b6f28dfa1dabc28e5/services/galley/src/Galley/API/Error.hs#L215).  But the team admin can remove all IdPs, which will effectively disable all SAML logins.

## Can you remove a SAML connection?

TODO: is this up to date?

It is not possible to delete a SAML connection in the Team Settings app, however it can be overwritten with a new connection.
It is possible do delete a SAML connection directly via the API endpoint `DELETE /identity-providers/{id}`. However deleting a SAML connection also requires deleting all users that can log in with this SAML connection. To prevent accidental deletion of users this functionality is not available directly from Team Settings.

## Do I need to change any firewall settings in order to use SAML?

No.

There is nothing to be done here.  There is no internet traffic
between your SAML IdP and the wire service.  All communication happens
via the browser or app.


## using the same IdP (same entityID, or Issuer) with different teams

TODO: go over this section again!

Some SAML IdP vendors do not allow to set up fresh entityIDs (issuers)
for fresh apps; instead, all apps controlled by the IdP are receiving
SAML credentials from the same issuer.

In the past, wire has used the a tuple of IdP issuer and 'NameID'
(Haskell type 'UserRef') to uniquely identity users (tables
`spar.user_v2` and `spar.issuer_idp`).

In order to allow one IdP to serve more than one team, this has been
changed: we now allow to identity an IdP by a combination of
entityID/issuer and wire `TeamId`.  The necessary tweaks to the
protocol are listed here.

For everybody using IdPs that do not have this limitation, we have
taken great care to not change the behavior.


### what you need to know when operating a team or an instance

No instance-level configuration is required.

If your IdP supports different entityID / issuer for different apps,
you don't need to change anything.  We hope to deprecate the old
flavor of the SAML protocol eventually, but we will keep you posted in
the release notes, and give you time to react.

If your IdP does not support different entityID / issuer for different
apps, keep reading.  At the time of writing this section, there is no
support for multi-team IdP issuers in team-settings, so you have two
options: (1) use the rest API directly; or (2) contact our customer
support and send them the link to this section.

If you feel up to calling the rest API, try the following:

- Use the above end-point `GET /sso/metadata/:tid` with your `TeamId`
  for pulling the SP metadata.
- When calling `POST /identity-provider`, make sure to add
  `?api_version=v2`.  (`?api_version=v1` or no omission of the query
  param both invoke the old behavior.)

NB: Neither version of the API allows you to provision a user with the
same Issuer and same NamdID.  RATIONALE: this allows us to implement
'getSAMLUser' without adding 'TeamId' to 'UserRef', which in turn
would break the (admittedly leaky) abstarctions of saml2-web-sso.


### API changes in more detail

- New query param `api_version=<v1|v2>` for `POST
  /identity-providers`.  The version is stored in `spar.idp` together
  with the rest of the IdP setup, and is used by `GET
  /sso/initiate-login` (see below).
- `GET /sso/initiate-login` sends audience based on api_version stored
  in `spar.idp`: for v1, the audience is `/sso/finalize-login`; for
  v2, it's `/sso/finalize-login/:tid`.
- New end-point `POST /sso/finalize-login/:tid` that behaves
  indistinguishable from `POST /sso/finalize-login`, except when more
  than one IdP with the same issuer, but different teams are
  registered.  In that case, this end-point can process the
  credentials by discriminating on the `TeamId`.
- `POST /sso/finalize-login/:tid` remains unchanged.
- New end-point `GET /sso/metadata/:tid` returns the same SP metadata as
  `GET /sso/metadata`, with the exception that it lists
  `"/sso/finalize-login/:tid"` as the path of the
  `AssertionConsumerService` (rather than `"/sso/finalize-login"` as
  before).
- `GET /sso/metadata` remains unchanged, and still returns the old SP
  metadata, without the `TeamId` in the paths.


### database schema changes

[V15](https://github.com/wireapp/wire-server/blob/b97439756cfe0721164934db1f80658b60de1e5e/services/spar/schema/src/V15.hs#L29-L43)



## Why does the team owner have to keep using password?

The user who creates the team cannot be authenticated via SSO.  There
is fundamentally no easy way around that: we need somebody to give us
the IdP credentials, and we need to trust that somebody.  For now,
that's the team owner with their password.

(It is also unwise to bind that owner to SAML once it's installed.  If
there is ever any issue with SAML authentication that can only be
resolved by updating the IdP metadata in team settings, the owner must
still have a way to authenticate in order to do that.)

There is a good workaround, though: you can create a team with user A
and use A for registering the IdP.  All the users for everyday use and
maintenance of the team, including admins and owners, can then be created
via SSO (either IdP or SCIM).  The original user A is only ever used
for IdP registration and upgrade of IdP-authenticated owners / admins.

In practice, user A and some owner authenticated via IdP would then be
controlled by the same person, probably.

## What should the SAML response look like?

Here is an example that works.  Much of this beyond the subject's
NameID is required by the SAML standard.  If you can find a more
minimal example that still works, we'd be love to take a look.

```xml
<saml:Assertion xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion" ID="..." IssueInstant="..." Version="2.0">
  <saml:Issuer>...</saml:Issuer>
  <ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#">
    <ds:SignedInfo>
      <ds:CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/>
      <ds:SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/>
      <ds:Reference URI="#...">
        <ds:Transforms>
          <ds:Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>
          <ds:Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#">
            <ec:InclusiveNamespaces xmlns:ec="http://www.w3.org/2001/10/xml-exc-c14n#" PrefixList="ds saml"/>
          </ds:Transform>
        </ds:Transforms>
        <ds:DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>
        <ds:DigestValue>...</ds:DigestValue>
      </ds:Reference>
    </ds:SignedInfo>
    <ds:SignatureValue>...</ds:SignatureValue>
    <ds:KeyInfo>
      <ds:X509Data>
        <ds:X509Certificate>...</ds:X509Certificate>
      </ds:X509Data>
    </ds:KeyInfo>
  </ds:Signature>
  <saml:Subject>
    <saml:NameID Format="urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress">...</saml:NameID>
    <saml:SubjectConfirmation Method="urn:oasis:names:tc:SAML:2.0:cm:bearer">
      <saml:SubjectConfirmationData Address="..." InResponseTo="..." NotOnOrAfter="..."
                                    Recipient="https://prod-nginz-https.wire.com/sso/finalize-login"/>
    </saml:SubjectConfirmation>
  </saml:Subject>
  <saml:Conditions NotBefore="..." NotOnOrAfter="...">
    <saml:AudienceRestriction>
      <saml:Audience>https://prod-nginz-https.wire.com/sso/finalize-login</saml:Audience>
    </saml:AudienceRestriction>
  </saml:Conditions>
  <saml:AuthnStatement AuthnInstant="..." SessionNotOnOrAfter="...">
    <saml:SubjectLocality Address="..."/>
    <saml:AuthnContext>
      <saml:AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</saml:AuthnContextClassRef>
    </saml:AuthnContext>
  </saml:AuthnStatement>
</saml:Assertion>
```

## Why does the auth response not contain a reference to an auth request?  (Also: can i use IdP-initiated login?)

**tl;dr:** Wire only supports SP-initiated login, where the user selects
the auth method from inside the app's login screen.  It does not
support IdP-initiated login, where the user enters the app from a list
of applications in the IdP UI.

### The full story

SAML authentication can be initiated by the IdP (eg., Okta or Azure),
or by the SP (Wire).

A user doing IdP-initiated authentication starts from some dashboard
in her IdP portal, and selects a button or link to the SP she wants to
interact with.  The IdP will then refer the user to the SP with the
SAML credentials in the redirect request.  The user needs to do
nothing but wait for the App to start.

In SP-initiated authentication, the user starts off on the login
screen of the app or web site of the SP.  She selects the IdP she
wants to authenticate with, and gets redirected there with an
authentication request.

That last part is important: the authentication request contains
cryptographic credentials that make some attacks (like
machine-in-the-middle attacks for stealing sessions and making users
impersonate rogue accounts) hard that were otherwise quite feasible.

Wire therefore only supports SP-initiated login.

## How are SAML2 assertion details used in wire?

Wire only uses the SAML `NameID` from the assertion, plus the
information whether authentication and authorization was successful.
Any other information is ignored.  In particular, Wire does not
support SAML2 `AttributeStatements`.  For best user experience, use
SCIM for provisioning users (see next section).

If SCIM is not in the picture, the SAML `NameID` is used to give the
wire user display name a default value.  (The user will be allowed to
change that value later; changing it does NOT affect the
authentication handshake between wire and the IdP.)

## How should I map user data to SCIM attributes when provisioning users via SCIM?

If you are provisioning users via SCIM, the following mapping is used
in your wire team:

1. SCIM's `userName` is mapped to wire's handle.  It must be unique
   accross the entire wire cloud or instance, and consist of the
   characters `a-z0-9_.-` (no capital letters).

2. SCIM's `displayName` is mapped to wire's user display name.  It
   must consist of 1-128 unicode characters, and does not need to be
   unique.

3. SCIM's `preferredLanguage` is mapped to wire's user locale settings
   when a locale is not defined for that user. It must consist of an
   ISO 639-1 language code.

4. SCIM's `externalId`:

   1. If SAML SSO is used, it is mapped on the SAML `NameID`.  If it
      parses as an email, it will have format `email`, and you can
      choose to validate it during provisioning (by enabeling the
      feature flag for your team).  Otherwise, the format will be
      `unspecified`.
   2. If email/password authentication is used, SCIM's `externalId` is
      mapped on wire's email address, and provisioning works like in
      team settings with invitation emails.

This means that if you use email/password authentication, you **must**
map an email address to `externalId` on your side.  With `userName`
and `displayName`, you are more flexible.

All three fields are mandatory.

Also note that the account will be set to `"active": false` until the
user has accepted the invitation and activated the account.  Please
contact customer support if this causes any issues.

## Can I distribute a URL to my users that contains the login code?

Users may find it awkward to copy and paste the login code into the
form.  If they are using the webapp, an alternative is to give them
the following URL (fill in the login code that you can find in your
team settings):

```bash
https://wire-webapp-dev.zinfra.io/auth#sso/3c4f050a-f073-11eb-b4c9-931bceeed13e
```

## (Theoretical) name clashes in SAML NameIDs

You can technically configure your SAML IdP to create name clashes in
wire, ie., to map two (technically) different NameIDs to the same wire
user.

### How to know you're safe

This is highly unlikely, since the
distinguishing parts of `NameID` that we ignore are generally either
unused or redundant.  If you are confident that any two users you have
assigned to the wire app can be distinguished solely by the
lower-cased `NameID` content, you're safe.

### Impact

If you are using SCIM for user provisioning, this may lead
to errors during provisioning of new users ("user already exists").
If you use SAML auto-provisioning, this may lead to unintential
account sharing instead of an error.

### How to reproduce

If you have users whose combination of
`IssuerId` and `NameID` can only be distinguished by casing (upper
vs. lower) or by the `NameID` qualifiers (`NameID` xml attributes
`NameQualifier`, `IdPNameQualifier`, ...), those users will name
clash.

### Solution

Do not rely on case sensitivity of `IssuerID` or `NameID`, or on
`NameID` qualifiers for distinguishing user identifiers.
