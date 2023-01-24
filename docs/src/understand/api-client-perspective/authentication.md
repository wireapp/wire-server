# Authentication

% useful vim replace commands when porting markdown -> restructured text:

% :%s/.. raw:: html//g

% :%s/   <a name="\(.*\)"\/>/.. _\1:/gc

## Access Tokens

The authentication protocol used by the API is loosely inspired by the
[OAuth2 protocol](http://oauth.net/2/). As such, API requests are
authorised through so-called [bearer
tokens](https://tools.ietf.org/html/rfc6750). For as long as a bearer
token is valid, it grants access to the API under the identity of the
user whose credentials have been used for the [login]. The
current validity of access tokens is `15 minutes`, however, that may
change at any time without prior notice.

In order to obtain new access tokens without having to ask the user for
his credentials again, so-called "user tokens" are issued which are
issued in the form of a `zuid` HTTP
[cookie](https://en.wikipedia.org/wiki/HTTP_cookie). These cookies
have a long lifetime (if {ref}`persistent <login-persistent>` typically
at least a few months) and their use is strictly limited to the
{ref}`/access <token-refresh>` endpoint used for token refresh.
{ref}`Persistent <login-persistent>` access cookies are regularly
refreshed as part of an {ref}`access token refresh <token-refresh>`.

An access cookie is obtained either directly after registration or through a
subsequent {ref}`login <login>`. A successful login provides both an access
cookie and and access token. Both access token and cookie must be stored safely
and kept confidential. User passwords should not be stored.

As of yet, there is no concept of authorising third-party applications to
perform operations on the API on behalf of a user (Notable exceptions:
{ref}`sso`). Such functionality may be provided in the future through
standardised OAuth2 flows.

To authorise an API request, the access token must be provided via the
HTTP `Authorization` header with the `Bearer` scheme as follows:

```
Authorization: Bearer fmmLpDSjArpksFv57r5rDrzZZlj...
```

While the API currently also supports passing the access token in the
query string of a request, this approach is highly discouraged as it
unnecessarily exposes access tokens (e.g. in server logs) and thus might
be removed in the future.

(login)=

## Login - `POST /login`

A login is the process of authenticating a user either through a known secret in
a {ref}`password login <login-password>` or by proving ownership of a verified
phone number associated with an account in an {ref}`SMS login <login-sms>`. The
response to a successful login contains an access cookie in a `Set-Cookie`
header and an access token in the JSON response body.

(login-cookies)=

### Cookies

There is a hard limit for the number of session-scoped access cookies and the same
amount of persistent access cookies per user account. When this number is
reached, old cookies are removed when new ones are issued. Thereby, the cookies
with the oldest expiration timestamp are removed first. The removal takes the
type of the cookie to issue into account. I.e. session cookies are replaced by
session cookies, persistent cookies are replaced by persistent cookies.

To prevent performance issues and malicious usages of the API, there is a
throttling mechanism in place. When the maximum number of cookies of one type
are issued, it's checked that login calls don't happen too frequently (too
quickly after one another.)

In case of throttling no cookie gets issued. The error response ([HTTP status
code 429](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/429)) has
a `Retry-After` header which specifies the time to wait before accepting the
next request in Seconds.

Being throttled is a clear indicator of incorrect API usage. There is no need to
login many times in a row on the same device. Instead, the cookie should be
re-used.

The corresponding backend configuration settings are described in:
{ref}`auth-cookie-config` .

(login-password)=

### Password Login

To perform a password login, send a `POST` request to the `/login`
endpoint, providing either a verified email address or phone number and
the corresponding password. For example:

```
POST /login HTTP/1.1
[headers omitted]

{
    "email": "me@wire.com",
    "password": "Quo2Booz"
}
```

If a phone number is used, the `phone` field is used instead of
`email`. If a @handle is used, the `handle` field is used instead of
`email` (note that the handle value should be sent *without* the `@`
symbol). Assuming the credentials are correct, the API will respond with
a `200 OK` and an access token and cookie:

```
HTTP/1.1 200 OK
zuid=...; Expires=Fri, 02-Aug-2024 09:15:54 GMT; Domain=zinfra.io; Path=/access; HttpOnly; Secure
[other headers omitted]

{
    "expires_in": 900,
    "access_token": "fmmLpDSjArpksFv57r5rDrzZZlj...",
    "token_type": "Bearer"
}
```

%

> The `Domain` of the cookie will be different depending on the
> environment.

The value of `expires_in` is the number of seconds that the
`access_token` is valid from the moment it was issued.

As of yet, the `token_type` is always `Bearer`.

(login-sms)=

### SMS Login

To perform an SMS login, first request an SMS code to be sent to a
verified phone number:

```
POST /login/send HTTP/1.1
[headers omitted]

{
    "phone": "+1234567890"
}
```

An SMS with a short-lived login code will be sent. Upon receiving the
SMS and extracting the code from it, the login can be performed using
the `phone` and `code` as follows:

```
POST /login HTTP/1.1
[headers omitted]

{
    "phone": "+1234567890",
    "code": "123456"
}
```

A successful response is identical to that of a {ref}`password
login <login-password>`.

(login-persistent)=

### Persistent Logins

By default, access cookies are issued as [session
cookies](https://en.wikipedia.org/wiki/HTTP_cookie#Session_cookie)
with a validity of 1 week. Furthermore, these session cookies are not
refreshed as part of an {ref}`access token refresh <token-refresh>`. To
request a `persistent` access cookie which does get refreshed, specify
the `persist=true` parameter during a login:

```
POST /login?persist=true HTTP/1.1
[headers omitted]

{
    "phone": "+1234567890",
    "code": "123456"
}
```

All access cookies returned on registration are persistent.

(token-refresh)=

### FAQ: is my cookie a persistent cookie or a session cookie?

When you log in **without** the `persist=true` query parameter, or
with persist=false, you get a `session cookie`, which means it has no
expiration date set, and will expire when you close the browser (and on
the backend has a validity of max 1 day or 1 week (configurable, see
current config in [hegemony](https://github.com/zinfra/hegemony)).
Example **session cookie**:

```
POST /login?persist=false

Set-Cookie: zuid=(redacted); Path=/access; Domain=zinfra.io; HttpOnly; Secure
```

When you log in **with** `persist=true`, you get a persistent cookie,
which means it has *some* expiration date. In production this is
currently 56 days (again, configurable, check current config in
[hegemony](https://github.com/zinfra/hegemony)) and can be renewed
during token refresh. Example **persistent cookie**:

```
POST /login?persist=true

Set-Cookie: zuid=(redacted); Path=/access; Expires=Thu, 10-Jan-2019 10:43:28 GMT; Domain=zinfra.io; HttpOnly; Secure
```

## Token Refresh - `POST /access`

Since access tokens have a relatively short lifetime to limit the time
window of abuse for a captured token, they need to be regularly
refreshed. In order to refresh an access token, send a `POST` reques
to `/access`, including the access cookie in the `Cookie` header and
the old (possibly expired) access token in the `Authorization` header:

```
POST /access HTTP/1.1
Authorization: Bearer fmmLpDSjArpksFv57r5rDrzZZlj...
Cookie: zuid=...
[other headers omitted]

<empty body>
```

Providing the old access token is not required but strongly recommended
as it will link the new access token to the old, enabling the API to see
the new access token as a continued session of the same client.

As part of an access token refresh, the response may also contain a new
`zuid` access cookie in form of a `Set-Cookie` header. A client must
expect a new `zuid` cookie as part of any access token refresh and
replace the existing cookie appropriately.

(cookies-1)=

## Cookie Management

(cookies-logout)=

### Logout - `POST /access/logout`

An explicit logout effectively deletes the cookie used to perform the
operation:

```
POST /access/logout HTTP/1.1
Authorization: Bearer fmmLpDSjArpksFv57r5rDrzZZlj...
Cookie: zuid=...
[other headers omitted]

<empty body>
```

Afterwards, the cookie that was sent as part of the `Cookie` header is
no longer valid.

If a client offers an explicit logout, this operation must be performed.
An explicit logout is especially important for Web clients.

(cookies-labels)=

### Labels

Cookies can be labeled by specifying a `label` during login or
registration, e.g.:

```
POST /login?persist=true HTTP/1.1
[headers omitted]

{
    "phone": "+1234567890",
    "code": "123456",
    "label": "Google Nexus 5"
}
```

Specifying a label is recommended as it helps to identify the cookies in a
user-friendly way and allows {ref}`selective revocation <cookies-revoke>` based
on the labels.

(cookies-list)=

### Listing Cookies - `GET /cookies`

To list the cookies currently associated with an account, send a `GET`
request to `/cookies`. The response will contain a list of cookies,
e.g.:

```
HTTP/1.1 200 OK
[other headers omitted]

{
  "cookies": [
    {
      "time": "2015-06-04T14:29:23.000Z",
      "id": 967153183,
      "type": "session",
      "label": null
    },
    {
      "time": "2015-06-04T14:44:23.000Z",
      "id": 942451749,
      "type": "session",
      "label": null
    },
    ...
  ]
}
```

Note that expired cookies are not automatically removed when they
expire, only as new cookies are issued.

(cookies-revoke)=

### Revoking Cookies - `POST /cookies/remove`

Cookies can be removed individually or in bulk either by specifying the full
cookie structure as it is returned by {ref}`GET /cookies <cookies-list>` or only
by their labels in a `POST` request to `/cookies/remove`, alongside with the
user's credentials:

```
POST /cookies/remove HTTP/1.1
[headers omitted]

{
    "ids": [{<cookie1>}, {<cookie2>}, ...],
    "labels": ["<label1>", "<label2>", ...]
    "email": "me@wire.com",
    "password": "secret"
}
```

Cookie removal currently requires an account with an email address and
password.

(password-reset)=

## Password Reset - `POST /password-reset`

A password reset can be used to set a new password if the existing password
associated with an account has been forgotten. This is not to be confused with
the act of merely changing your password for the purpose of password rotation or
if you suspect your current password to be compromised.

### Initiate a Password Reset

To initiate a password reset, send a `POST` request to
`/password-reset`, specifying either a verified email address or phone
number for the account in question:

```
POST /password-reset HTTP/1.1
[headers omitted]

{
    "phone": "+1234567890"
}
```

For a phone number, the `phone` field would be used instead. As a
result of a successful request, either a password reset key and code is
sent via email or a password reset code is sent via SMS, depending on
whether an email address or a phone number was provided. Password reset
emails will contain a link to the [wire.com](https://www.wire.com/)
website which will guide the user through the completion of the password
reset, which means that the website will perform the necessary requests
to complete the password reset. To complete a password reset initiated
with a phone number, the completion of the password reset has to happen
from the mobile client application itself.

Once a password reset has been initiated for an email address or phone
number, no further password reset can be initiated for the same email
address or phone number before the prior reset is completed or times
out. The current timeout for an initiated password reset is
`10 minutes`.

### Complete a Password Reset

To complete a password reset, the password reset code, together with the
new password and the `email` or `phone` used when initiating the
reset (or the opaque `key` sent by mail) are sent to
`/password-reset/complete` in a `POST` request:

```
POST /password-reset/complete HTTP/1.1
[headers omitted]

{
    "phone": "+1234567890",
    "code": "123456",
    "password": "new-secret-password"
}
```

There is a maximum of `3` attempts at completing a password reset,
after which the password reset code becomes invalid and a new password
reset must be initiated.

A completed password reset results in all access cookies to be revoked,
requiring the user to {ref}`login <login>`.

## Related topics: SSO, Legalhold

(sso)=

### Single Sign-On

Users that are part of a team, for which a team admin has configured SSO (Single Sign On), authentication can happen through SAML.

More information:

- {ref}`FAQ <trouble-shooting-faq>`
- [setup howtos for various IdP vendors](https://docs.wire.com/how-to/single-sign-on/index.html)
- [a few fragments that may help admins](https://github.com/wireapp/wire-server/blob/develop/docs/reference/spar-braindump.md)

### LegalHold

Users that are part of a team, for which a team admin has configured "LegalHold", can add a so-called "LegalHold" device. The endpoints in use to authenticate for a "LegalHold" Device are the same as for regular users, but the access tokens they get can only use a restricted set of API endpoints. See also [legalhold documentation on wire-server](https://github.com/wireapp/wire-server/blob/develop/docs/reference/team/legalhold.md)
