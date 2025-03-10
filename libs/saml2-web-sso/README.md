# Status

- incomplete (some parts of the standard are not supported yet)
- ready (we are using this in production)

# Introduction

This is a library plus toy-sp app for the role of the service provider
in [SAML2 Web
Single-Sign-On](https://en.wikipedia.org/wiki/Security_Assertion_Markup_Language).

The library gives you:

- xml parsing and rendering
- signature verification
- policy checks
- servant APIs and wai application
- configuration file handling

We are making heavy use of the more complete but also experimental
[hsaml2](https://hackage.haskell.org/package/hsaml2/).  We are hoping
to stick with a smaller, robust subset of the standard and phase out
that dependency in the future.

# Configuration

Assume you have an IdP set up and somebody who knows how to configure
it.  This section explains what you need to ask them in order to write
a config file for the `toy-sp` app in order to connect to it, and what
you need to tell them so they can register your `toy-sp` app with
their IdP.

## what the SP (us) needs to know

`/test/samples/server-config.yaml` contains a commented sample config
that you can copy.  It contains a list of IdP records with the
following record fields (all but the first can be extracted from the
metadata that the IdP provides via HTTP).

- `path`: The path segment in the `toy-sp` login url.  You can pick
  something short and easy to memorize, or, if you just want to
  redirect users to `toy-sp` from your intranet and they do not have
  to look at this link at all, a serial number or UUID.

- `metadata`: the URL under which the `<IDPSSODescriptor>` blob can be
  fetched.  This should contain everything that is repeatedin the
  fields below.  The reason for the redundancy is security: By copying
  the `KeyInfo` blob into your config file, you commit to it being
  ultimatly trustworthy.

- `issuer-id`: this is the IdP's SAML2 issuer ID.  It is used in
  the XML response data and needs to match this entry in order for the
  public key to be found.

- `request-url`: the URL to which the `<AuthnRequest>` is to be
  forwarded by the client during SSO login.

- `public-key`: the `<KeyInfo>` blob with the certificate that
  contains the public key for response signature verification.

## what the IdP (them) needs to know

For configuring the IdP the most important thing you need is the
end-point that is located under `GET /sso/meta` in toy-sp.  This is
where the SP provides its own metadata that can be fetched by the IdP.
If you have a trusted channel to the IdP, you can also fetch the
end-point locally and pass the XML data that you get to the IdP
directly.

You also need to help users to find the `GET /sso/authreq/<path>`
end-point.  It could either be linked from some globally accessible
page in a menu of different supported IdPs, or from an app menu in the
intranet of the company running the IdP.

# Try it out!

You need:

- an IdP (e.g. on [azure](https://azure.microsoft.com/)).
- a host that is reachable from the internet (e.g. via [ngrok](https://ngrok.com/)).
- an SSL proxy (e.g. with [nginx](https://docs.nginx.com/nginx/)).
- a clone of the repo you are looking at.
- a file `/server.yaml` copied and adopted from `/test/samples/server-config.yaml`.

Now run:

```
export SAML2_WEB_SSO_ROOT=`pwd`
stack test --fast
stack exec toy-sp
```

This should start the `toy-sp` app.  Now you can connect to
`https://<domain>:<port>/sso/authreq/<your-idp>`.

`<your-idp>` has to be the path value of one of the IdPs registered in
`server.yaml`.

# Contributing

Before submitting a PR, make sure to install [ormolu](https://github.com/tweag/ormolu)
by doing `stack install ormolu` (we pin the version in our `stack.yaml` file)
and run `make format`.
