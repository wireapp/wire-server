# OAuth

We use OAuth 2.0 to authorize 3rd party applications to access wire-server resources on behalf of the a wire user who wants to use the 3rd party application.

Currently, we only support 3rd party applications that have been implemented and approved by Wire, e.g. Outlook Calendar Extension. OAuth is not open for public use.

## OAuth flow

Wire-server currently only supports the [authorization code flow](https://oauth.net/2/grant-types/authorization-code/).

```{eval-rst}
.. kroki::
   :type: mermaid
   :caption: OAuth 2.0 authorization code flow

   sequenceDiagram
       actor U as User
       participant C as Outlook Calendar Extension
       participant A as Authorization Server (wire-server)
       participant B as Resource Server (wire-server)

       U->>C: Click Login
       C->>A: Authorization code request /authorize
       A->>U: Redirect to login/authorization prompt

```

### Registering OAuth client

### Get authorization code

### Retrieve access and refresh token

### Refresh access token

### Revoke a refresh token

### Accessing a resource

### Retrieve a list of 3rd party apps with account access

### Revoke account access

## Configuration

### En-/Disable OAuth

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
        "kty": "OKP",
        "crv": "Ed25519",
        "x": "...",
        "d": "..."
      }
```

Note that the JWK is a sensitive configuration value, so it is recommended to use Helm's support for managing secrets instead of including it in a plaintext values.yaml file.

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

## Enable 3rd party applications for teams

### OutlookCalIntegrationConfig

## Implementation
