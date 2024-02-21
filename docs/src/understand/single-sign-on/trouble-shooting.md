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
- Which vendor (or product) are you using (octa, azure, centrica, other (which one)?)
- Team ID (looks like eg. `2e9a9c9c-6f83-11eb-a118-3342c6f16f4e`, can be found in the team management app)
- User ID of the account that has the problem (alternatively: handle, email address)
- What do you expect to happen?
    - e.g.: "I enter login code, authenticate successfully against IdP, get redirected, and see the wire landing page."
- What does happen instead?
    - Screenshots
    - Copy the text into your report where applicable in addition to screenshots (for automatic processing).
    - e.g.: "instead of being logged into wire, I see the following error page: ..."
- Screenshots of the Configuration (both SAML and SCIM, as applicable), including, but not limited to:
    - If you are using SAML: SAML IdP metadata file
    - If you are using SCIM for provisioning: Which attributes in the User schema are mapped?  How?
- If you have successfully authenticated on your IdP and are
  redirected into wire, and then see a white page with an error
  message that contains a lot of machine-readable info: copy the full
  message to the clipboard and insert it into your report.  We do not
  log this information for privacy reasons, but we can use it to
  investigate your problem.  (Hint, if you want to investigate
  yourself: it's base64 encoded!)

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
against that response value.  Once that test passes, it should all
work fine.


## Can I use SCIM without SAML?

Yes.  Scim is a technology for onboarding alternative to the team management app, and can produce both user accounts authenticated via SAML or via email and password.  (Phone may or may not work, but is not officially supported.)

How does it work?  Make sure your team has no SAML IdPs registered.  Set up your SCIM peer to provision users with valid email addresses as `externalIds`.  Newly provisioned users will be created in status `PendingInvitation`, and an invitation email will be sent.  From here on out, the flow is exactly the same as if you had added the user to your team in the team management app.

Upcoming features:
- support for the `emails` field in the scim user record (so you can choose non-email `externalId` values).
- flexible mapping between any number of SAML IdPs and any number of SCIM tokens in team management.

## Can I use SAML without SCIM?

Yes, but this is not recommended.  User (de-)provisioning requires more manual work without SCIM, and some of the account information cannot be provisioned at all via SAML.


## Can I use the same SSO login code for multiple teams?

Most SAML IdP products allow you to register arbitrary many apps for
arbitrary many teams, by using a different entity Id for each app/team.  This is
currently supported out of the box.

If you don't have this option, i.e. you need to serve two teams with an
IdP that has only one entity ID, please keep reading and/or contact
customer support.

### The long answer

Some SAML IdP vendors do not allow to set up fresh entity IDs (issuers)
for fresh apps; instead, all apps controlled by the IdP are receiving
SAML credentials from the same issuer.

In the past, wire has used a tuple of IdP issuer and 'NameID'
(Haskell type 'UserRef') to uniquely identity users (tables
`spar.user_v2` and `spar.issuer_idp`).

In order to allow one IdP to serve more than one team, this has been
changed: we now allow to identify an IdP by a combination of
entityID/issuer and wire `TeamId`.  The necessary tweaks to the
protocol are listed here.

**This extension is currently (as of 2023-02-03) not supported by the
team management app.  If you need this, please contact customer
support.**

#### what you need to know when operating a team or an instance

No instance-level configuration is required.

If your IdP supports different entityID / issuer for different apps,
you don't need to change anything.

If your IdP does not support different entityID / issuer for different
apps, keep reading.  At the time of writing this section, there is no
support for multi-team IdP issuers in the team management app, so you have two
options: (1) use the rest API directly; or (2) contact our customer
support and send them the link to this section.

If you feel up to calling the rest API, try the following:

- Use the above end-point `GET /sso/metadata/:tid` with your `TeamId`
  for pulling the SP metadata.
- When calling `POST /identity-provider`, make sure to add
  `?api_version=v2`.  (`?api_version=v1` or no omission of the query
  param both invoke the old behavior.)

NB: Neither version of the API allows you to provision a user with the
same Issuer and same NameID.  The pair of Issuer and NameID must
always be globally unique.

#### API changes in even more detail

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
- `POST /sso/finalize-login/` remains unchanged.
- New end-point `GET /sso/metadata/:tid` returns the same SP metadata as
  `GET /sso/metadata`, with the exception that it lists
  `"/sso/finalize-login/:tid"` as the path of the
  `AssertionConsumerService` (rather than `"/sso/finalize-login"` as
  before).
- `GET /sso/metadata` remains unchanged, and still returns the old SP
  metadata, without the `TeamId` in the paths.

#### database schema changes

[V15](https://github.com/wireapp/wire-server/blob/b97439756cfe0721164934db1f80658b60de1e5e/services/spar/schema/src/V15.hs#L29-L43)


## Can an existing user without IdP (or with a different IdP) be bound to a new IdP?

Yes, you can, by updating the user via SCIM.  (If you use SAML without
SCIM, there is a way in theory, but there are no plans to implement
it.)


## Can the SSO feature be disabled for a team?

No, this is [not implemented](https://github.com/wireapp/wire-server/blob/7a97cb5a944ae593c729341b6f28dfa1dabc28e5/services/galley/src/Galley/API/Error.hs#L215).  But the team admin can remove all IdPs, which will effectively disable all SAML logins.


## Do I need to change any firewall settings in order to use SAML?

No.

There is nothing to be done here.  There is no internet traffic
between your SAML IdP and the wire service.  All communication happens
via the browser or app.


## Why does the team owner have to keep using password?

The user who creates the team cannot be authenticated via SSO.  There
is fundamentally no easy way around that: we need somebody to give us
the IdP credentials, and we need to trust that somebody.  For now,
that's the team owner with their password.

(It is also unwise to bind that owner to SAML once it's installed.  If
there is ever any issue with SAML authentication that can only be
resolved by updating the IdP metadata in the team management app, the owner must
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
      the team management app with invitation emails.

5. SCIM's `roles` is mapped to team role.  Only lists of length 0 or 1
   are allowed.  Valid values are:

   - `[member]` (same as `[]`, `null`, or missing field)
   - `[admin]`
   - `[owner]`
   - `[partner]`

The mapping of `externalId` implies that if you use email/password
authentication, you **must**
map an email address to `externalId` on your side.  With `userName`
and `displayName`, you are more flexible.

All three fields are mandatory.

Also note that the account will be set to `"active": false` until the
user has accepted the invitation and activated the account.  Please
contact customer support if this causes any issues.

## Can I distribute a URL to my users that contains the login code?

Users may find it awkward to copy and paste the login code into the
form.  If they are using the webapp, an alternative is to give them
the following URL (fill in the login code that you can find in the
team management app):

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

## After logging in via IdP page, the redirection to the wire app is not happening

**Problem:** when logging in using SSO, the user gets redirected to
the IdP page. After entering the credentials, IdP successfully
authenticates but is stuck at the stage where redirection needs to
happen to Wire app.

The console log may mention CSP violations, or declare to refuse to
forward form data to the `finalize-login` url.

**Possible cause and fix:** Some browsers (chrome is one example) prevent redirects if there
is a risk of leaking sensitive form data to the redirect target. In
your setup (in particular when you're using email domain-based
redirect from the wire cloud to your on-prem instance), your browser
may decide to not trust the wire app with the results of the IdP login
procedure.  In order to circumvent this issue your IdP needs to be
configured to add "wire://*" to the "form-action" CSP value.


See also:
[1](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/form-action),
[2](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy),
[3](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP).
