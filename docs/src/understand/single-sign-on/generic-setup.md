(sso-generic-setup)=

# How to set up SSO integration with your IdP

## Preprequisites

- An account with your SAML IdP, admin access to that account
- Wire team, admin access to that team
- If your team is hosted at wire.com:
  : - Ask customer support to enable the SSO feature flag for you.
- If you are running your own on-prem instance:
  : - for handling the feature flag, you can run your own [backoffice](https://github.com/wireapp/wire-server-deploy/tree/259cd2664a4e4d890be797217cc715499d72acfc/charts/backoffice) service.
    - More simply, you can configure the galley service so that sso is always enabled (just put "enabled-by-default" [here](https://github.com/wireapp/wire-server-deploy/blob/a4a35b65b2312995729b0fc2a04461508cb12de7/values/wire-server/prod-values.example.yaml#L134)).

## Setting up your IdP

- The SP Metadata URL: <https://prod-nginz-https.wire.com/sso/metadata>
- The SSO Login URL: <https://prod-nginz-https.wire.com/sso/finalize-login>
- SP Entity ID (aka Request Issuer ID): <https://prod-nginz-https.wire.com/sso/finalize-login>

How you need to use this information during setting up your IdP
depends on the vendor.  Let us know if you run into any trouble!

## Setting up your wire team

See <https://support.wire.com/hc/en-us/articles/360001285638-Set-up-SSO-internally>

## Authentication

The team settings will show you a login code from us that looks like
eg.

\> `wire-959b5840-3e8a-11e9-adff-0fa5314b31c0`

See
<https://support.wire.com/hc/en-us/articles/360000954617-Pro-How-to-log-in-with-SSO>-
on how to use this to login on wire.
