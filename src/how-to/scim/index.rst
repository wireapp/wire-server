How to set up user provisioning with SCIM
=========================================

Wire supports the `SCIM <http://www.simplecloud.info/>`__ (`RFC 7643 <https://tools.ietf.org/html/rfc7643>`__) protocol to create, update and delete users.

To set up the connection of your SCIM client (e.g. Azure Active Directory) you need to provide

1. The URL under which Wire's SCIM API is hosted: ``https://prod-nginz-https.wire.com/scim/v2``.
   If you are hosting your own instance of Wire then the URL is ``https://<hostname>/scim/v2``, where ``<hostname>`` is where you are serving Wire's public endpoints. Some SCIM clients append ``/v2`` to the URL your provide. If this happens (check the URL mentioned in error messages of your SCIM client) then please provide the URL without the ``/v2`` suffix, i.e. ``https://prod-nginz-https.wire.com/scim`` or ``https://<hostname>/scim``.

2. A secret token which authorizes the use of the SCIM API. Use the  `wire_scim_token.py <https://raw.githubusercontent.com/wireapp/wire-server/654b62e3be74d9dddae479178990ebbd4bc77b1e/docs/reference/provisioning/wire_scim_token.py>`__
   script to generate a token. To run the script you need access to an user account with "admin" privileges that can login via email and password. Note that the token is independent from  the admin account that created it, i.e. the token remains valid if the admin account gets deleted or changed.

You need to configure your SCIM client to use the following mandatory SCIM attributes:

1. Set the ``userName`` attribute to the desired user handle (the handle is shown
   with an @ prefix in apps). It must be unique accross the entire Wire Cloud
   (or unique on your own instance), and consist of the characters ``a-z0-9_.-``
   (no capital letters).

2. Set the ``displayName`` attribute to the user's desired display name, e.g. "Jane Doe".
   It must consist of 1-128 unicode characters. It does not need to be unique.

3. The ``externalId`` attribute:

   a. If you are using Wire's SAML SSO feature then set ``externalId`` attribute to the same identifier as ``NameID`` in your SAML configuration.

      If this identifier is an email address, then you can optionally enable email validatation during provisioning by activating the ``validateSAMLemails`` feature flag.

   b. If you are using email/password authentication then set the ``externalId``
      attribute to the user's email address. The user will receive an invitation email during provisioning. Also note that the account will be set to ``"active": false`` until the user has accepted the invitation and activated the account.

You can optionally make use of Wire's ``urn:wire:scim:schemas:profile:1.0`` extension field to store arbitrary user profile data. See `docs <https://github.com/wireapp/wire-server/blob/develop/docs/reference/user/rich-info.md#scim-support-refrichinfoscim>`__ for details.
