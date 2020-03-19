Trouble shooting & FAQ
======================

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
