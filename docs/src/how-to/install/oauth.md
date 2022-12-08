# OAuth

To use the OAuth functionality, you will need to set up a public and private JSON web key pair (JWK) in the wire-server helm chart. This key pair will be used to sign and verify OAuth access tokens.

To configure the JWK, go to `brig.secrets.oauthJwkKeyPair` in the wire-server helm chart and provide the JWK information, as shown in the example below:

```yaml
# values.yaml or secrets.yaml
brig:
  secrets:
    oauthJwkKeyPair: |
      {
        "p":"8U9gI_...",
        "kty":"RSA",
        "q":"43dqC...",
        "d":"ixZk7x...",
        "e":"AQAB",
        "use":"sig",
        "kid":"QVapB_J...",
        "qi":"sYHbPsy...",
        "dp":"LFmnVNPW...",
        "alg":"RS256",
        "dq":"UXTY7...",
        "n":"1mnyGVT..."
      }
```

Note that the JWK is a sensitive configuration value, so it is recommended to use helm's support for managing secrets instead of including it in a plaintext values.yaml file.

Please keep in mind that OAuth is currently under development and may not be available for use yet. Once it is ready, you will be able to use the OAuth functionality by setting up the JWK as described above.
