# OAuth

We use OAuth 2.0 to authorize 3rd party applications to access wire-server resources on behalf of a wire user (resource owner) who wants to use the 3rd party application.

Currently, we only support 3rd party applications that have been implemented and approved by Wire, e.g. Outlook Calendar Extension. OAuth is not open for public use.

## Roles

### 3rd party application

Also called (Oauth) client.

An application that is attempting to get access a resource on behalf of the user. It needs to get permission from the user in order to do so.

### Resource server

This is the API server the 3rd party app attempts to access on behalf of the user which is wire-server.
  
## OAuth flow

Wire-server currently only supports the [authorization code flow](https://www.rfc-editor.org/rfc/rfc6749#section-4.1) which is optimized for confidential clients such as Outlook Calendar Extension.

<!-- ```{eval-rst}
.. kroki::
   :type: mermaid
   :caption: OAuth 2.0 authorization code flow

   sequenceDiagram
       autonumber
       actor U as User
       participant C as Outlook Calendar Extension
       participant A as Authorization Server (wire-server)
       participant R as Resource Server (wire-server)

       U->>C: Click login
       C->>A: Authorization code request /authorize
       A->>U: Redirect to login/authorization prompt
       U->>A: Authenticate and consent
       A->>C: Authorization code
       C->>A: Authentication code + client crednetials
       A->>A: Validate authentication code + client crednetials
       A->>C: Access token
       C->>R: Request a resource with access token (e.g. POST /conversations)
       R->>R: Validate access token with public key from auth server
       R->>C: Response
``` -->

### Registering an OAuth client

A new OAuth client can be register via the internal API of `brig` by providing an application name and a redirect URL:

```curl
  curl -s -X POST localhost:8082/i/oauth/clients \
    -H "Content-Type: application/json" \
    -d '{
      "applicationName":"foobar",
      "redirectUrl":"https://example.com"
    }'
```

Client credentials will be generated and returned by wire-server:


```json
{
  "clientId": "b9e65569-aa61-462d-915d-94c8d6ef17a7",
  "clientSecret": "3f6fbd62835859b2bac411b2a2a2a54699ec56504ee32099748de3a762d41a2d"
}
```

These credentials have to be stored in a safe place and cannot be recovered if they are lost.

### Get authorization code

When the user wants to use the 3rd party app for the first time, they need to authorize it to access wire resources on their behalf.

To do so the user is redirected to a login screen for authentication. Once authenticated, the user can either grant or deny the client's access request and the corresponding scope (a list of permissions to give to the 3rd party app).

Example request:

```http
GET /authorize?
  scope=SCOPE&
  response_type=code&
  client_id=b9e65569-aa61-462d-915d-94c8d6ef17a7&
  redirect_uri=https://example.com&
  state=foobar
```

Once the user consents the user agent will be redirected back to the 3rd party app, using the redirect URI provided during client registration, with an authorization code which can be used to retrieve an access token and a refresh token and is good for one use.

Example response:

```http
HTTP/1.1 302 Found
Transfer-Encoding: chunked
Date: Thu, 23 Feb 2023 15:50:21 GMT
Server: Warp/3.3.23
Location: https://example.com?code=1395a1a44b72e0b81ec8fe6c791d2d3f22bc1c4df96857a88c3e2914bb687b7b&state=foobar
Vary: Accept-Encoding
```

### Retrieve access and refresh token

The 3rd party application 

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

## Implementation details
