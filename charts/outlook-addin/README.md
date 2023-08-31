# How to install Outlook AddIn for Wire-Server

WIP: Some of these configurations are subject to change down the line. This documentation will be updated accordingly as they happen.

This document assumes you already have an instance of wire-server running. If you don't, follow this [documentation](https://github.com/wireapp/wire-server-deploy/blob/master/offline/docs.md)

## Set up OAuth with wire-server

To use OAuth, first you will need to enable it by editing `values/wire-server/values.yaml` as follows:

```
brig:
  # ...
  config:
    # ...
    optSettings:
      # ...
      setOAuthEnabled: true
```

Then you will need to generate a key using "OKP" (Octet Key Pair) and the "Ed25519" curve with OpenSSL that will be used as JWK (JSON Web Key) in the wire-server helm chart. This key will be used to sign and verify [OAuth](https://docs.wire.com/developer/reference/oauth.html#setting-up-public-and-private-keys) access tokens.

```
openssl genpkey -algorithm Ed25519 -out private_key.pem
```

You can find a `generate_jwk.py` in this chart which you can use to generate the JWK in JSON format that can be used in your wire-server helm chart. Use it in `brig` and `nginz` namespaces in `values/wire-server/secrets.yaml` like shown below.

```
brig:
  secrets:
    oauthJwkKeyPair: |
      {
        "kty": "OKP",
        "crv": "Ed25519",
        "x": "...",
        "d": "...",
        "kid": "..."
      }
```

```
# values.yaml or secrets.yaml
nginz:
  secrets:
    oAuth:
      publicKeys: |
        {
          "kty": "OKP",
          "crv": "Ed25519",
          "x": "...",
          "kid": "..."
        }
```

Now redeploy wire-server chart:

```
d helm upgrade --install wire-server charts/wire-server --values values/wire-server/values.yaml --values/wire-server/secrets.yaml
```

## Outlook integration feature flag

By default, outlook addin as a feature is disabled for all teams. To change this make the following changes in your configuration in `galley` namespace:

```
galley:
  config:
    # ...
    settings:
      # ...
      featureFlags:
        # ...
        outlookCalIntegration:
          defaults:
            status: enabled
            lockStatus: unlocked
```

Redeploy wire-server for these changes to take effect.

NOTE: As of the time of writing `outlookCalIntegration` is not a typo! (at least not in this documentation)

If you have an existing team in your wire-server that did not have this feature flag enabled prior to this. You will need to enable that feature flag through [Backoffice API](https://github.com/wireapp/wire-server/tree/05778a2b14ac5aaffca937d6e2cdd9b7b5f3106d/charts/backoffice).

NOTE: As of the time of writing Backoffice API endpoint for enabling this feature flag is not working as intended so please follow this manual on how to do it with curl on the machine wire-server is running on.

### How to manually enable outlookCalIntegration feature flag for a team

You will need your `teamId` (you can find it in TeamSettings under Customization tab).
List all your pods in your Kubernetes cluster with:

```
d kubectl get pods -owide
```

Copy the name of one of your galley pods and run:

```
d kubectl exec -it galley_pod_name /bin/bash
```

In the new terminal type:

```
curl -v -XPATCH 'http://localhost:8080/i/teams/your_teamID/features/outlookCalIntegration' -H 'content-type: application/json;charset=utf-8' -d '{"status": "enabled", "lockStatus": "unlocked"}'
```

Do this for all the teams you want to enable the feature for.

## Create new client service for OAuth in Brig

List all your pods in your Kubernetes cluster with:

```
d kubectl get pods -owide
```

Copy the name of one of your brig pods and run:

```
d kubectl exec -it brig_pod_name /bin/bash
```

In the new terminal type:

```
curl -s -X POST localhost:8080/i/oauth/clients \
    -H "Content-Type: application/json" \
    -d '{
      "application_name":"Wire Microsoft Outlook Calendar Add-in",
      "redirect_url":"https://outlook.example.com/callback.html"
    }'
```

You will get back a response in JSON format that should look like:

```
{"client_id":"b2b3...","client_secret":"9ee60..."}
```

Write down your client_id as it will be needed later.

## Deploying Wire Outlook AddIn

Create a new `values.yaml` file in `values/outlook-addin` directory (create the directory too if missing).
Append the following configuration (change the example.com with your domain).

```
host: "outlook.example.com" # this entry has to be without https://!!!
wireApiBaseUrl: "https://nginz-https.example.com"
wireAuthorizationEndpoint: "https://webapp.example.com/auth"
clientId: ""
```

As of the time of writing nginz used by wire-server is not set up to whitelist outlook subdomain for CORS requests. So please edit `charts/wire-server/charts/nginz/values.yaml` and find under `nginx_conf`:

```
  allowlisted_origins:
    - webapp
    - teams
    - account
    - outlook # add outlook entry so your addin doesnt get CORS blocked
```

### Certificates

If you are using cert-manager just make the following configuration in values.yaml:

```
tls:
  issuerRef:
    name: letsencrypt-http01 # letsencrypt-http01 is a default config in wire-server, change if needed in your instance
```

Now deploy outlook addin chart with:

```
d helm upgrade --install outlook-addin charts/outlook-addin --values values/outlook-addin/values.yaml
```

If you are using your own provided certificates, deploy the addin with this command:

```
d helm upgrade --install outlook-addin charts/outlook-addin --values values/outlook-addin/values.yaml --set-file tls.crt=/path/to/tls.crt --set-file tls.key=/path/to/tls.key
```

## Install Wire AddIn in Microsoft Outlook

After deploying `outlook-addin` you will be able to find `manifest.xml` file on https://outlook.example.com/manifest.xml which you can use to install the addin in your outlook. You can find instructions and screenshots how to do it [here](https://github.com/tlebon/outlook-addin/blob/staging/README.md#how-to-install-the-add-in-in-ms-outlook).
NOTE: Links in the outlined documents are hardcoded for a testing/prod environment, any reference to zinfra.io or wire.com in it should be treated as example.com.
