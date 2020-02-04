Single-sign-on background and design choices (fragment)
=======================================================

Overview
--------

.. image:: Wire_SAML_Flow.png

IdP-initiated login and SP-initiated login
------------------------------------------

tl;dr: Wire only supports SP-initiated login, where the user doesn't
start from a list of applications in the IdP, but from the login
screen in the app.

Why?
^^^^

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
