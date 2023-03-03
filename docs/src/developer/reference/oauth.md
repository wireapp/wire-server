# OAuth

```{contents}
:depth: 3
```

## Introduction and overview

OAuth 2.0 is used to authorize 3rd party applications to access `wire-server` resources on behalf of a Wire user.

Currently, only 3rd party apps that have been implemented and approved by Wire are supported. OAuth is not open for public use.

Supported OAuth apps:

- Outlook Calendar Extension

`wire-server` implements a subset of the [The OAuth 2.0 Authorization Framework (RFC 6749)](https://www.rfc-editor.org/rfc/rfc6749).

Please refer to the documentation below for a reference of the subset that is implemented without the noise of having to go through the complete RFC.

### Roles

#### The user (resource owner)

The user is the resource owner who gives permission to the OAuth client to access parts of their resources.

#### 3rd party application (OAuth client)

A 3rd party app is attempting to get access a resource on behalf of the user. It needs to get permission from the user in order to do so.  The terminology is a bit fuzzy here: we use the terms app, application, client synonymous.  To disambiguate, we qualify with "oauth" (*oauth* client, ...).

#### Resource server

The resource server is the API server the 3rd party app attempts to access on behalf of the user. In our case the resource server is `wire-server`.

#### Authorization server

The authorization server does the authentication of the user and establishes whether the user approves or denies the client's access request. In this case the authorization server is the same server as the resource server which is `wire-server`.

### Supported OAuth flow

`wire-server` currently only supports the [authorization code flow](https://www.rfc-editor.org/rfc/rfc6749#section-4.1) which is optimized for confidential clients such as Outlook Calendar Extension.

```{eval-rst}
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
       C->>A: Authorization code + client credentials
       A->>A: Validate authorization code + client credentials
       A->>C: Access token
       C->>R: Request a resource with access token (e.g. POST /conversations)
       R->>R: Validate access token with public key from auth server
       R->>C: Response
```

## OAuth client developer reference

### Registering an OAuth client

A new OAuth client can be register *only* via the internal API of `brig` by providing an application name and a redirect URL:

```curl
  curl -s -X POST server-internal.example.com/i/oauth/clients \
    -H "Content-Type: application/json" \
    -d '{
      "application_name":"Outlook Calendar Extension",
      "redirect_url":"https://client.example.com"
    }'
```

Parameters:

| Parameter          | Description                                                                                          |
| ------------------ | ---------------------------------------------------------------------------------------------------- |
| `redirect_url`     | The URL to which Wire app will redirect the browser after authorization has been granted by the user |
| `application_name` | The name of the application that will be shown on the consent page                                   |

Client credentials will be generated and returned by wire-server:

```json
{
  "clientId": "b9e65569-aa61-462d-915d-94c8d6ef17a7",
  "clientSecret": "3f6fbd62835859b2bac411b2a2a2a54699ec56504ee32099748de3a762d41a2d"
}
```

These credentials have to be stored in a safe place and cannot be recovered if they are lost.

### Get authorization code

When the user wants to use the 3rd party app for the first time, they need to authorize it to access Wire resources on their behalf.

They first need to click on the "Login" (or similar) button (1. in OAuth 2.0 authorization code flow diagram above) which will redirect them to a Wire login page to authenticate (2.-3. in diagram above). Once authenticated, they are redirected to the consent page.

If the user is already logged in the authentication will be skipped and they are directly shown the consent page.

On the consent page, the user is asked to authorize the client's access request. They can either grant or deny the request and the corresponding scope, a list of permissions to give to the 3rd party app, (4. in diagram above).

Example request:

```http
GET /authorize?
  scope=read%3Aself%20write%3Aconversation&
  response_type=code&
  client_id=b9e65569-aa61-462d-915d-94c8d6ef17a7&
  redirect_uri=https%3A%2F%2Fclient.example.com&
  state=foobar
```

Url encoded query parameters:

| Parameter       | Description                                                                                                                                                                                                                                                                       |
| --------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `scope`         | Required. The scope of the access request.                                                                                                                                                                                                                                        |
| `response_type` | Required. Value MUST be set to  `code`.                                                                                                                                                                                                                                           |
| `client_id`     | Required. The client identifier.                                                                                                                                                                                                                                                  |
| `redirect_url`  | Required. MUST match the URL that was provided during client registration                                                                                                                                                                                                         |
| `state`         | Required. An opaque value used by the client to maintain state between the request and callback.</br>The authorization server includes this value when redirecting the user-agent back to the client.</br>The parameter is used for preventing cross-site request forgery. |

Once the user consents, the browser will be redirected back to the 3rd party app, using the redirect URI provided during client registration, with an authorization code and the state value as query parameters (5. in diagram above). The authorization code can now be used by the 3rd party app to retrieve an access token and a refresh token and is good for one use.

Example response:

```http
HTTP/1.1 302 Found
Transfer-Encoding: chunked
Date: Thu, 23 Feb 2023 15:50:21 GMT
Server: Warp/3.3.23
Location: https://client.example.com?code=1395a1a44b72e0b81ec8fe6c791d2d3f22bc1c4df96857a88c3e2914bb687b7b&state=foobar
Vary: Accept-Encoding
```

### Retrieve access and refresh token

The 3rd party app sends the authorization code together with the client credentials and the parameters shown below using the `application/x-www-form-urlencoded` format with character encoding of UTF-8 to the authorization server (6. in diagram above) to retrieve an access token and a refresh token (7.-8. in diagram above):

```curl
curl -s -X POST server.example.com/oauth/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d 'code=1395a1a44b72e0b81ec8fe6c791d2d3f22bc1c4df96857a88c3e2914bb687b7b&client_id=b9e65569-aa61-462d-915d-94c8d6ef17a7&grant_type=authorization_code&redirect_uri=https%3A%2F%2Fclient.example.com&client_secret=3f6fbd62835859b2bac411b2a2a2a54699ec56504ee32099748de3a762d41a2d'
```

Parameters:

| Parameter       | Description                                                                             |
| --------------- | --------------------------------------------------------------------------------------- |
| `code`          | Required. The authorization code received from the authorization server.                |
| `client_id`     | Required. The client identifier.                                                        |
| `grant_type`    | Required. Value MUST be set to `authorization_code`.                                    |
| `redirect_uri`  | Required. The value MUST be identical to the one provided in the  authorization request |
| `client_secret` | Required. The client's secret.                                                          |

Example response:

```JSON
{
  "access_token": "eyJhbGciOiJFZERTQSJ9.eyJhdWQiOiJleGFtcGxlLmNvbSIsImV4cCI6MS42NzcyMzYyNDkxMTIzMTk3MWU5LCJpYXQiOjEuNjc3MjM2MjQ2MTEyMzE5NzFlOSwiaXNzIjoiZXhhbXBsZS5jb20iLCJzY29wZSI6InJlYWQ6c2VsZiIsInN1YiI6ImJhOTIxY2ZmLWU1ZWEtNDMxNS1iZTNkLWZiNjA3NTU0M2Y3MCJ9.5ksjS7msi9NSNat-qh7-Y5O-u9TcuYeLWTAsiAyes_oLwfjD_jYtGevUAuiVV6RXgPBO00VEMv-ZS86e7sd5Dg",
  "expires_in": 300,
  "refresh_token": "eyJhbGciOiJFZERTQSJ9.eyJzdWIiOiI4NzI1ZTRkNC01Njc5LTQwZGEtOTI3My03YTBkMmIwYjUwMGYifQ.59IICzGoli5nfwJ1ZwRH_b3T-lRgBrralE1EZZRtadI2eKrta0kaLIZpuMWPC2Icj6-LSEBsyYLXpxOm3cNaDw",
  "token_type": "Bearer"
}
```

### Accessing a resource

The access token, presented as `Bearer <token>` in the `Authorization` header, can now be used by the 3rd party app to access resources on behalf of the user (9.-11. in diagram above).

### Refresh access token

Access tokens are short lived and need to be refreshed regularly. To do so, the client makes a refresh request to the token endpoint by adding the parameters shown below using the `application/x-www-form-urlencoded` format with a character encoding of UTF-8 in the HTTP request entity-body.

Example request:

```curl
curl -s -X POST server.example.com/oauth/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d 'refresh_token=eyJhbGciOiJFZERTQSJ9.eyJzdWIiOiI4NzI1ZTRkNC01Njc5LTQwZGEtOTI3My03YTBkMmIwYjUwMGYifQ.59IICzGoli5nfwJ1ZwRH_b3T-lRgBrralE1EZZRtadI2eKrta0kaLIZpuMWPC2Icj6-LSEBsyYLXpxOm3cNaDw&client_id=b9e65569-aa61-462d-915d-94c8d6ef17a7&grant_type=refresh_token&client_secret=3f6fbd62835859b2bac411b2a2a2a54699ec56504ee32099748de3a762d41a2d'
```

Parameters:

| Parameter       | Description                                       |
| --------------- | ------------------------------------------------- |
| `refresh_token` | Required. The refresh token issued to the client. |
| `client_id`     | Required. The client identifier.                  |
| `grant_type`    | Required. Value MUST be set to `refresh_token`.   |
| `client_secret` | Required. The client's secret.                    |

Example response:

```json
{
  "access_token": "eyJhbGciOiJFZERTQSJ9.eyJhdWQiOiJleGFtcGxlLmNvbSIsImV4cCI6MS42NzcyMzc3MjkyMzAyMDU3NTJlOSwiaWF0IjoxLjY3NzIzNzcyNjIzMDIwNTc1MmU5LCJpc3MiOiJleGFtcGxlLmNvbSIsInNjb3BlIjoicmVhZDpzZWxmIiwic3ViIjoiYmE5MjFjZmYtZTVlYS00MzE1LWJlM2QtZmI2MDc1NTQzZjcwIn0.__fa8l5E2L-DNaxG5tJFLF8zf5y8lpCJI0F5MOFPdewaXbOLleTJ_eAueCjG7dB7yXNVY9Uiry_W5Mw7O19UCw",
  "expires_in": 300,
  "refresh_token": "eyJhbGciOiJFZERTQSJ9.eyJzdWIiOiJjMzQwOWU4OC0wYmE5LTQ3YjgtOWQ0My03YTVmMmM3YjhlNGYifQ.Padku_6pOUIlE469cW5TELtiNtO1mZnBrRpj8CRMgNVBTck7qxInz6LyXVYfSV7soAa44202yXzI0o4IcfZiBA",
  "token_type": "Bearer"
}
```

### Revoke a refresh token

A refresh token can be revoked as follows:

```curl
curl -i -s -X POST localhost:8080/oauth/revoke \
  -H "Content-Type: application/json" \
  -d '{
    "client_id": "31a605c5-b033-405a-ab05-f8307cf22d3f",
    "client_secret": "d2580e9b759eca52fdf3a21532ea5aae0e08706529a0a6e6fa4ab2a3d7b39da4",
    "refresh_token": "eyJhbGciOiJFZERTQSJ9.eyJzdWIiOiJjMzQwOWU4OC0wYmE5LTQ3YjgtOWQ0My03YTVmMmM3YjhlNGYifQ.Padku_6pOUIlE469cW5TELtiNtO1mZnBrRpj8CRMgNVBTck7qxInz6LyXVYfSV7soAa44202yXzI0o4IcfZiBA"
  }'
```

Parameters:

| Parameter      | Description                                       |
| -------------- | ------------------------------------------------- |
| `client_id`     | Required. The client identifier.                  |
| `refresh_token` | Required. The refresh token issued to the client. |
| `client_secret` | Required. The client's secret.                    |

Example response:

```http
HTTP/1.1 200 OK
(empty-response-body)
```

## Wire client developer reference (ZAuth authorized API)

### Retrieve OAuth client info

Authenticated endpoint to retrieve client information, necessary to display authorization prompt/user consent page.

  ```http
GET /oauth/client/:id HTTP/1.1
```

Example response:

```json
{
  "client_id": "922fcc9c-07a5-4306-86ce-7b6838442372",
  "application_name": "Outlook Calendar Extension",
  "redirect_url": "https://example.com"
}
```

### Retrieve a list of 3rd party apps with account access

Authenticated endpoint to retrieve a list of all applications that have account access via OAuth.

```http
GET /oauth/applications HTTP/1.1
```

Example response:

```json
[
  {
    "id": "31a605c5-b033-405a-ab05-f8307cf22d3f",
    "name": "Outlook Calendar Extension"
  },
  {
    "id": "c3a76a50-42a0-49d2-99df-87f0fcc12d20",
    "name": "Secure Alert"
  }
]
```

### Revoke account access

3rd party app access can be revoked, by invalidating all active refresh tokens, as follows:

```http
DELETE /oauth/applications/{cid} HTTP/1.1
```

## Site admin reference (Configuration)

### Enable/disable OAuth

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

Note that the JWK is a sensitive configuration value, so it is recommended to use Helm's support for managing secrets instead of including it in a plaintext `values.yaml` file.

### OAuth authorization code, access token, and refresh token expiration

The the OAuth authorization code expiration and access and refresh token expiration can be overridden in the Helm file as follows:

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

## Enable 3rd party apps for teams

3rd party apps are enabled based on the team's payment plan by `ibis`.

## Implementation details

### Token handling

#### Access token

Access tokens are self-contained JSON Web Tokens (JWT) that contain the following claims:

- `iss`: The issuer, e.g. `wire-server`
- `aud`: The resource server (in our case the same as `iss`)
- `iat`: The time at which the token was issued
- `sub`: Identifier of the resource owner, the Wire user ID
- `exp`: The expiration time of the token
- `scope`: A whitespace separated list of permissions

Example token payload:

```json
{
  "iss": "server.example.com",
  "aud": "server.example.com",
  "iat": 1311280970,
  "sub": "7cf24b6c-8c7e-4788-a532-2c998d20ce4a",
  "exp": 1311281970,
  "scope": "write:conversations write:conversations_code"
}
```

- The access tokens are created and signed by `brig`.
- When accessing a resource `nginz` validates the token and forwards the request to `wire-server` with the `Z-User` header containing the user ID taken from the `sub` claim.
- Token validation includes signature, expiration, and scope validation.
- Access tokens are short lived.
- Access tokens are bearer tokens and cannot be revoked directly, therefore 3rd party access revocation will entail the token expiration.

#### Refresh token

refresh tokens are assigned to a user and a 3rd party app (OAuth client)

a user can have more than one active refresh token for the same 3rd party app (e.g. they might use multiple devices, replace devices, or run multiple instances of the app somehow)

the maximum number of active refresh tokens per user and app should be limited (what is a sensitive number, 10?)

once a new token is requested and the limit is exceeded, the oldest refresh token will be deleted/invalidated

once a refresh token is used, it will be invalidated and a new refresh token will be generated and returned as part of the response (token rotation)

for now, we will not yet implement re-use detection, but in the future this should be possible

the refresh token is given to the client/app as a signed JWT containing only the refresh token ID

refresh tokens are long-lived, and the expiration should be configurable on the server level (default 3-6 months?)

### Scopes

### Public/private keys

New OAuth JWK keys have to be provided.

Acceptance criteria:

A secure JWK key has been generated

JWK key including the private key has been set in brig secrets for prod and staging

Public JWK key has been set in galley options

OAuth end-to-end test works with new JWK key

### Default configs
