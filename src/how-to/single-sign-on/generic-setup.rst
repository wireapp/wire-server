How to set up SSO integration with your IdP
===========================================

What you need from us to set up your IdP
----------------------------------------

- The SP Metadata URL: https://prod-nginz-https.wire.com/sso/metadata
- The SSO Login URL: https://prod-nginz-https.wire.com/sso/finalize-login
- SP Entity ID (aka Request Issuer ID): https://prod-nginz-https.wire.com/sso/finalize-login

How you need to use this information during setting up your IdP
depends on the vendor.  Let us know if you run into any trouble!

What we need from you
---------------------

- The IdP Metadata URL or XML file
- Your Team ID (open https://teams.wire.com/settings/ on the "billing" tab)

Please pass this information on to our customer support, and we will
set things up for you in your wire team!

Authentication
--------------

You will get a login code from us that looks like
eg.

> `wire-959b5840-3e8a-11e9-adff-0fa5314b31c0`

Your users need to
open the app, select "company login", enter this code there, and
follow the instructions.  A wire user will be created for them if they
login for the first time.
