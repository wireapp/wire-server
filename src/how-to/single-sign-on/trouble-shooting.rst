Trouble shooting & FAQ
======================


Can I use the same SSO login code for multiple teams?
-----------------------------------------------------

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


Can an existing user without IdP (or with a different IdP) be bound to a new IdP?
---------------------------------------------------------------------------------

No.  This is a feature we never fully implemented.  Details / latest
updates: https://github.com/zinfra/backend-issues/issues/731


If you get an error when returning from your IdP
------------------------------------------------

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


Do I need any firewall settings?
--------------------------------

No.

There is nothing to be done here.  There is no internet traffic
between your SAML IdP and the wire service.  All communication happens
via the browser or app.


Why does the team owner have to keep using password?
----------------------------------------------------

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


What should the SAML response look like?
----------------------------------------

Here is an example that works.  Much of this beyond the subject's
NameID is required by the SAML standard.  If you can find a more
minimal example that still works, we'd be love to take a look.

.. code:: xml

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
