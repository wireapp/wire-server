# OAuth

If not configured, OAuth is disabled per default. OAuth can be enabled in the wire-server Helm as follows:

```yaml
brig:
  # ...
  config:
    # ...
    optSettings:
      # ...
      setOAuthEnabled: true
```

To use the OAuth functionality, you will need to set up a public and private JSON web key pair (JWK) in the wire-server Helm chart. This key pair will be used to sign and verify OAuth access tokens.

To configure the JWK, go to the wire-server Helm chart and provide the JWK information, as shown in the example below:

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

Note that the JWK is a sensitive configuration value, so it is recommended to use Helm's support for managing secrets instead of including it in a plaintext values.yaml file.

[2023-01-16] OAuth is currently under development and may not be available for use yet. Once it is ready, you will be able to use the OAuth functionality by setting up the JWK as described above.

### OAuth authorization code, access token, and refresh token expiration

The the OAuth authorization code expiration (default 5 minutes) and access token expiration (default 3 weeks) can be overridden in the Helm file as follows:

```yaml
brig:
  # ...
  config:
    # ...
    optSettings:
      # ...
      setOAuthAuthCodeExpirationTimeSecs: 300 # 5 minutes
      setOAuthAccessTokenExpirationTimeSecs: 300 # 5 minutes
      setOAuthRefreshTokenExpirationTimeSecs: 14515200 # 24 weeks
```

### Maximum number of active refresh tokens

The maximum number of active OAuth refresh tokens a user is allowed to have can be configured as follows:

```yaml
brig:
  # ...
  config:
    # ...
    optSettings:
      # ...
      setOAuthMaxActiveRefreshTokens: 20
```
