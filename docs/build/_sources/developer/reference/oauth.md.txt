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

`wire-server` currently only supports the [Authorization Code Flow with Proof Key for Code Exchange (PKCE)](https://www.rfc-editor.org/rfc/rfc7636) which is optimized for public clients such as Outlook Calendar Extension.

```{image} oauth.svg
```

## OAuth client developer reference

### Registering an OAuth client

A new OAuth client can be register *only* via the internal API of `brig` by providing an application name and a redirect URL:

```shell
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

### Authorization request

When the user wants to use the 3rd party app for the first time, they need to authorize it to access Wire resources on their behalf.

They first need to click on the "Login" (or similar) button (1. in OAuth 2.0 authorization code flow diagram above) which will redirect them to a Wire login page to authenticate (2.-3. in diagram above). Once authenticated, they are redirected to the consent page.

If the user is already logged in the authentication will be skipped and they are directly shown the consent page.

On the consent page, the user is asked to authorize the client's access request. They can either grant or deny the request and the corresponding scope, a list of permissions to give to the 3rd party app, (4. in diagram above).

The client needs to create a unique `code_verifier` as described in [RFC 7636 section 4.1](https://www.rfc-editor.org/rfc/rfc7636#section-4.1) and send a `code_challenge`, which is the unpadded base64url-encoded SHA256 hash of the code verifier as described in [RFC 7636 section 4.2](https://www.rfc-editor.org/rfc/rfc7636#section-4.2). The `code_challenge` must be included in the request. The `S256` code challenge method is mandatory. The `code_verifier` must not be included in the request.

Example request:

```
GET /authorize?
  scope=read%3Aself%20write%3Aconversation&
  response_type=code&
  client_id=b9e65569-aa61-462d-915d-94c8d6ef17a7&
  redirect_uri=https%3A%2F%2Fclient.example.com&
  state=foobar&
  code_challenge=qVrqDTN8ivyWEEw6wyfUc3bwhCA2RE4V2fbiC4mC7ofqAF4t&
  code_challenge_method=S256 HTTP/1.1
```

Url encoded query parameters:

| Parameter               | Description                                                                                                                                                                                                                                                                |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `scope`                 | Required. The scope of the access request.                                                                                                                                                                                                                                 |
| `response_type`         | Required. Value MUST be set to `code`.                                                                                                                                                                                                                                     |
| `client_id`             | Required. The client identifier.                                                                                                                                                                                                                                           |
| `redirect_url`          | Required. MUST match the URL that was provided during client registration                                                                                                                                                                                                  |
| `state`                 | Required. An opaque value used by the client to maintain state between the request and callback.</br>The authorization server includes this value when redirecting the user-agent back to the client.</br>The parameter is used for preventing cross-site request forgery. |
| `code_challenge`        | Required. Generated by the client from the `code_verifier`                                                                                                                                                                                                                 |
| `code_challenge_method` | Required. It MUST be set to `S256`                                                                                                                                                                                                                                         |

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

```shell
curl -s -X POST server.example.com/oauth/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d 'code=1395a1a44b72e0b81ec8fe6c791d2d3f22bc1c4df96857a88c3e2914bb687b7b&client_id=b9e65569-aa61-462d-915d-94c8d6ef17a7&grant_type=authorization_code&redirect_uri=https%3A%2F%2Fclient.example.com&code_verifier=2dae11ce5e162e2c01180ae4f8b55103b8297408b8aab12f99f63df3c2415234'
```

Parameters:

| Parameter       | Description                                                                             |
| --------------- | --------------------------------------------------------------------------------------- |
| `code`          | Required. The authorization code received from the authorization server.                |
| `client_id`     | Required. The client identifier.                                                        |
| `grant_type`    | Required. Value MUST be set to `authorization_code`.                                    |
| `redirect_uri`  | Required. The value MUST be identical to the one provided in the  authorization request |
| `code_verifier` | Required. The code verifier as described above.                                        |

Example response:

```JSON
{
  "access_token": "eyJhbGciOiJFZERTQSJ9.eyJhdWQiOiJleGFtcGxlLmNvbSIsImV4cCI6MS42NzcyMzYyNDkxMTIzMTk3MWU5LCJpYXQiOjEuNjc3MjM2MjQ2MTEyMzE5NzFlOSwiaXNzIjoiZXhhbXBsZS5jb20iLCJzY29wZSI6InJlYWQ6c2VsZiIsInN1YiI6ImJhOTIxY2ZmLWU1ZWEtNDMxNS1iZTNkLWZiNjA3NTU0M2Y3MCJ9.5ksjS7msi9NSNat-qh7-Y5O-u9TcuYeLWTAsiAyes_oLwfjD_jYtGevUAuiVV6RXgPBO00VEMv-ZS86e7sd5Dg",
  "expires_in": 300,
  "refresh_token": "eyJhbGciOiJFZERTQSJ9.eyJzdWIiOiI4NzI1ZTRkNC01Njc5LTQwZGEtOTI3My03YTBkMmIwYjUwMGYifQ.59IICzGoli5nfwJ1ZwRH_b3T-lRgBrralE1EZZRtadI2eKrta0kaLIZpuMWPC2Icj6-LSEBsyYLXpxOm3cNaDw",
  "token_type": "Bearer"
}
```

The expiration time in the response (`expires_in`) refers to the expiration time of the access token.

### Accessing a resource

The access token, presented as `Bearer <token>` in the `Authorization` header, can now be used by the 3rd party app to access resources on behalf of the user (9.-11. in diagram above).

### Refresh access token

Access tokens are short lived and need to be refreshed regularly. To do so, the client makes a refresh request to the token endpoint by adding the parameters shown below using the `application/x-www-form-urlencoded` format with a character encoding of UTF-8 in the HTTP request entity-body.

Example request:

```shell
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

```shell
curl -i -s -X POST localhost:8080/oauth/revoke \
  -H "Content-Type: application/json" \
  -d '{
    "client_id": "31a605c5-b033-405a-ab05-f8307cf22d3f",
    "client_secret": "d2580e9b759eca52fdf3a21532ea5aae0e08706529a0a6e6fa4ab2a3d7b39da4",
    "refresh_token": "eyJhbGciOiJFZERTQSJ9.eyJzdWIiOiJjMzQwOWU4OC0wYmE5LTQ3YjgtOWQ0My03YTVmMmM3YjhlNGYifQ.Padku_6pOUIlE469cW5TELtiNtO1mZnBrRpj8CRMgNVBTck7qxInz6LyXVYfSV7soAa44202yXzI0o4IcfZiBA"
  }'
```

Parameters:

| Parameter       | Description                                       |
| --------------- | ------------------------------------------------- |
| `client_id`     | Required. The client identifier.                  |
| `refresh_token` | Required. The refresh token issued to the client. |
| `client_secret` | Required. The client's secret.                    |

Example response:

```
HTTP/1.1 200 OK
(empty-response-body)
```

## Wire client developer reference (ZAuth authorized API)

### Retrieve OAuth client info

Authenticated endpoint to retrieve client information, necessary to display authorization prompt/user consent page.

See [swagger docs](https://staging-nginz-https.zinfra.io/api/swagger-ui/#/default/get_oauth_clients__OAuthClientId_).

### Retrieve a list of 3rd party apps with account access

Authenticated endpoint to retrieve a list of all applications that have account access via OAuth.

See [swagger docs](https://staging-nginz-https.zinfra.io/api/swagger-ui/#/default/get_oauth_applications).

### Revoke account access

3rd party app access can be revoked, by invalidating all active refresh tokens, as follows:

See [swagger docs](https://staging-nginz-https.zinfra.io/api/swagger-ui/#/default/delete_oauth_applications__OAuthClientId_).

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

#### Setting up public and private keys

To use the OAuth functionality, you will need to set up a public and private JSON web key pair (JWK) in the wire-server Helm chart. This key pair will be used to sign and verify OAuth access tokens.

Key can be generated e.g. with [jwx](https://github.com/lestrrat-go/jwx/tree/develop/v2/cmd/jwx) like this:

```shell
jwx jwk generate --type OKP --curve Ed25519 | jq -c
```

`jwx` is available via nix: `nix-shell -p jwx`.

To configure the JWK, go to the wire-server Helm chart and provide the JWK information, private and public key set for `brig` and the public key for `nginz`, as in the examples below:

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

```yaml
# values.yaml or secrets.yaml
nginz:
  secrets:
    oAuth:
      publicKeys: |
        {
          "kty": "OKP",
          "crv": "Ed25519",
          "x": "..."
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

#### Authorization code

The authorization code is stored as plain text rather than a "scrypted" hash because it is the key to look up the associated information like the client ID, the user ID, the scope and the redirect URL. An authorization code can only be used once and has a very short time to live.

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

- A refresh token is always associated with
  - a user
  - a 3rd party app (the OAuth client)
  - and a scope (list of permissions given to the app)
- A user can have more than one active refresh token for the same 3rd party app (e.g. they might use multiple devices, replace devices, or run multiple instances of the app somehow)
- The maximum number of active refresh tokens per user and app is limited (see `values.yaml` for default settings)
- Once a new refresh token is requested and the limit is exceeded, the oldest refresh token will be deleted/invalidated
  - If the bearer of the invalidated token is not identical to the requester, it could mean that the bearer of the invalidated token needs to re-authorize
- Once a refresh token is used, it will be invalidated and a new refresh token will be generated and returned as part of the response (token rotation)
- For now, we will not yet implement re-use detection, but in the future this should be possible
- The refresh token is given to the client/app as a signed JWT containing only the refresh token ID which is used internally to look up the refresh token info
- Refresh tokens are long-lived, and the expiration is configurable on the server level

### Scopes

Endpoints that support OAuth have the required scope listed in the swagger documentation.

#### Scope implementation details

To enable OAuth access for a resource a scope has to be defined in the nginx location config that matches the endpoint's path.

The current convention is that scope names should match the resource's paths separated by an underscore. E.g. `/conversations/:cid/code` becomes `conversations_code` (path parameters are omitted).

Furthermore, the scope must be prefixed (separated by a colon) with

- `admin`, `write`, or `read` for endpoints with HTTP method `GET`
- `admin`, or `write` for endpoints with HTTP methods `POST` or `PUT`
- and `admin` for endpoints with HTTP method `DELETE`

E.g. the required scope for `POST /conversations/:cid/code` is `write:conversations_code`.

### Steps for adding a new scope (making an endpoint accessible via OAuth)

- Add a new constructor to the type `OAuthScope` in `/home/leif/Repositories/wire-server/libs/wire-api/src/Wire/API/OAuth.hs`
- Implement `IsOAuthScope`
- Update `ToByteString` and `FromByteString` instances and verify that the roundtrip tests run successfully
- Add the servant combinator `DescriptionOAuthScope` to the endpoint in question which will render the correct swagger description
- Finally assign the scope name (without the prefix) to the location config via the `charts/nginz/values.yaml` file to the `oauth_scope` as shown in the example below

Example:

```haskell
type SelfAPI =
  Named
    "get-self"
    ( Summary "Get your own profile"
        :> DescriptionOAuthScope 'ReadSelf
        :> ZUser
        :> "self"
        :> Get '[JSON] SelfProfile
    )
```

```nginx
    - path: /self$ # Matches exactly /self
      oauth_scope: self
      envs:
```

For local development and integration tests, add the scope to `services/nginz/integration-test/conf/nginz/nginx.conf` as follows

```nginx
    location ~* ^(/v[0-9]+)?/self$ {
      include common_response_with_zauth.conf;
      oauth_scope self;
      proxy_pass http://brig;
    }
```

### Public/private keys

- Public and private keys are provided as JSON Web Keys (JWK) or key sets
- The keys can be generated using [jwx](https://github.com/lestrrat-go/jwx/tree/develop/v2/cmd/jwx#jwx-jwk-generate)
- Keys are provided as secrets. Details depend on the type of deployment.
- `brig` needs to be in possession of the public and private key and `nginz` needs to be provided with the public key only
