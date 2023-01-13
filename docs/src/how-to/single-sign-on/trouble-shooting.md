(trouble-shooting-faq)=

# Trouble shooting & FAQ

## Reporting a problem with user provisioning or SSO authentication

In order for us to analyse and understand your problem, we need at least the following information up-front:

- Have you followed the following instructions?
  : - {ref}`FAQ <trouble-shooting-faq>` (This document)
    - [Howtos](https://docs.wire.com/how-to/single-sign-on/index.html) for supported vendors
    - [General documentation on the setup flow](https://support.wire.com/hc/en-us/articles/360001285718-Set-up-SSO-externally)
- Vendor information (octa, azure, centrica, other (which one)?)
- Team ID (looks like eg. `2e9a9c9c-6f83-11eb-a118-3342c6f16f4e`, can be found in team settings)
- What do you expect to happen?
  : - eg.: "I enter login code, authenticate successfully against IdP, get redirected, and see the wire landing page."
- What does happen instead?
  : - Screenshots
    - Copy the text into your report where applicable in addition to screenshots (for automatic processing).
    - eg.: "instead of being logged into wire, I see the following error page: ..."
- Screenshots of the Configuration (both SAML and SCIM, as applicable), including, but not limited to:
  : - If you are using SAML: SAML IdP metadata file
    - If you are using SCIM for provisioning: Which attributes in the User schema are mapped?  How?

## Can I use the same SSO login code for multiple teams?

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

No.  This is a feature we never fully implemented.  Details / latest
updates: <https://github.com/wireapp/wire-server/issues/1151>

## Can the SSO feature be disabled for a team?

No, this is [not implemented](https://github.com/wireapp/wire-server/blob/7a97cb5a944ae593c729341b6f28dfa1dabc28e5/services/galley/src/Galley/API/Error.hs#L215).

## Can you remove a SAML connection?

It is not possible to delete a SAML connection in the Team Settings app, however it can be overwritten with a new connection.
It is possible do delete a SAML connection directly via the API endpoint `DELETE /identity-providers/{id}`. However deleting a SAML connection also requires deleting all users that can log in with this SAML connection. To prevent accidental deletion of users this functionality is not available directly from Team Settings.

## If you get an error when returning from your IdP

`Symptoms:`

You have successfully authenticated on your IdP and are
redirected into wire.  Wire shows a white page with an error message
that contains a lot of machine-readable info.

`What we need from you:`

- Your SSO metadata file
- The SSO login code (eg. `wire-3f61d2ce-525c-11ea-b8da-cf641a7b716a`;
  you can find it in the team settings where you registered your IdP)
- The full browser page with the error message (copy it into your
  clipboard and insert it into an email to us, or save the page as an
  html file and send that to us)

With all this information, please get in touch with our customer
support.

## Do I need any firewall settings?

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

tl;dr: Wire only supports SP-initiated login, where the user selects
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

## How to report problems

If you have a problem you cannot resolve by yourself, please get in touch.  Add as much of the following details to your report as possible:

- Are you on cloud or on-prem?  (If on-prem: which instance?)
- XML IdP metadata
- SSL Login code or IdP Issuer EntityID
- NameID of the account that has the problem
- SP metadata

Problem description, including, but not limited to:

- what happened?
- what did you want to happen?
- what does your idp config in the wire team management app look like?
- what does your wire config in your IdP management app look like?
- Please include screenshots *and* copied text (for cut&paste when we investigate) *and* further description and comments where feasible.

(If you can't produce some of this information of course please get in touch anyway!  It'll merely be harder for us to resolve your issue quickly, and we may need to make a few extra rounds of data gathering together with you.)
