# User Registration

<a id="refregistration"></a>

*Authors: Artyom Kazak, Matthias Fischmann*

---

This page describes the “normal” user registration flow. Autoprovisioning is covered separately.

## Summary

<a id="refregistrationsummary"></a>

The vast majority of our API is only available to Wire users. Unless a user is autoprovisioned, they have to register an account by calling the `POST /register` endpoint.

Most users also go through [activation](activation.md) – sharing and verifying
an email address with Wire. This can happen either before or after registration.
[Certain functionality](activation.md#refactivationbenefits) is only available to activated
users.

## Standard registration flow

<a id="refregistrationstandard"></a>

During the standard registration flow, the user first calls [`POST /activate/send`](activation.md#refactivationrequest) to pre-verify their email address.

After receiving a six-digit activation code via email message, it can be submitted with the registration request via `POST /register`. If the code is correct, the account will be activated immediately. Here is a sample request and response:

```default
POST /register

{
    // The name is mandatory
    "name": "Pink",

    // 'email' has to be provided
    "email": "pink@example.com",

    // The password is optional
    "password": "secret",

    // 6-digit 'email_code'
    "email_code": "123456"
}
```

```default
201 Created
Set-Cookie: zuid=...

{
    "accent_id": 0,
    "assets": [],
    "email": "pink@example.com",
    "id": "4e1b823a-7961-4e70-bff5-30c08555c89f",
    "locale": "en",
    "managed_by": "wire",
    "name": "Pink",
    "picture": []
}
```

The response contains an access cookie that can be used to access the rest of the backend API. The cookie can be assigned a label by adding a `"label": <...>` field when calling `/register`.

If the code is incorrect or if an incorrect code has been tried enough times, the response will be as follows:

```default
404 Not Found

{
    "code": 404,
    "label": "invalid-code",
    "message": "Invalid activation code"
}
```

## Registration without pre-verification

<a id="refregistrationnopreverification"></a>

*NOTE: This flow is currently not used by any clients. At least this was the state on 2020-05-28*

It is also possible to call `POST /register` without verifying the email
address, in which case the account will have to be activated later by calling
[`POST /activate`](activation.md#refactivationsubmit). Sample API request and response:

```default
POST /register

{
    // The name is mandatory
    "name": "Pink",

    // 'email'  has to be provided
    "email": "pink@example.com",

    // The password is optional
    "password": "secret"
}
```

```default
201 Created
Set-Cookie: zuid=...

{
    "accent_id": 0,
    "assets": [],
    "email": "pink@example.com",
    "id": "c193136a-55fb-4534-ad72-d02a72bb16af",
    "locale": "en",
    "managed_by": "wire",
    "name": "Pink",
    "picture": []
}
```

A verification email will be sent to the email address (if provided).

## Anonymous registration, aka “Wireless”

<a id="refregistrationwireless"></a>

A user can be created without email, in which case only `"name"` is required.
The `"name"` does not have to be unique. This feature is used for [guest
rooms](https://wire.com/en/features/encrypted-guest-rooms/).

An anonymous, non-activated account is only usable for a period of time specified in `brig.yaml` at `zauth.authSettings.sessionTokenTimeout`, which is set to 1 day for Wire production. (The access cookie returned by `/register` can not be refreshed, and an anonymous user can not use `/login` to get a new cookie.)

Sample API request and response:

```default
POST /register

{
    "name": "Pink"
}
```

```default
201 Created
Set-Cookie: zuid=...

{
    "accent_id": 0,
    "assets": [],
    "expires_at": "2019-04-18T14:09:43.732Z",
    "id": "1914b4e6-eb72-4943-8925-06314b24ed68",
    "locale": "en",
    "managed_by": "wire",
    "name": "Pink"
    "picture": [],
}
```

## Blocking creation of personal users, new teams {#RefRestrictRegistration}

[moved here](https://docs.wire.com/how-to/install/configuration-options.html#blocking-creation-of-personal-users-new-teams)

### Details

You can find the exhaustive list of all routes here:

https://github.com/wireapp/wire-server-deploy/blob/de7e3e8c709f8baaae66b1540a1778871044f170/charts/nginz/values.yaml#L35-L371

The paths not cryptographically authenticated can be found by searching for the `disable_zauth:` flag (must be true for `env: prod` or `env: all`).

Two of them allow users to create new users or teams:

- `/register`
- `/activate`

These end-points support 5 flows:

1. new team account
2. new personal (teamless) account
3. invitation code from team, new member
4. ephemeral user
5. [not supported by clients] new *inactive* user account

We need an option to block 1, 2, 5 on-prem; 3, 4 should remain available (no block option).  There are also provisioning flows via SAML or SCIM, which are not critical. In short, this could refactored into:

* Allow team members to register (via email or SSO)
* Allow ephemeral users

During registration, we can take advantage of [NewUserOrigin](https://github.com/wireapp/wire-server/blob/a89b9cd818997e7837e5d0938ecfd90cf8dd9e52/libs/wire-api/src/Wire/API/User.hs#L625); we’re particularly interested in `NewUserOriginTeamUser` –> only `NewTeamMember` or `NewTeamMemberSSO` should be accepted. In case this is a `Nothing`, we need to check if the user expires, i.e., if the user has no identity (and thus `Ephemeral`).

So `/register` should only succeed iff at least one of these conditions is true:

```default
import Brig.Types.User
isNewUserTeamMember || isNewUserEphemeral
```

The rest of the unauthorized end-points is safe:

- `/password-reset`
- `/delete`: similar to password reset, for deleting a personal account with password.
- `/login`
- `/login/send`
- `/access`
- `/sso/initiate-login`: authenticated via IdP.
- `/sso/finalize-login`: authenticated via IdP.
- `/sso`: authenticated via IdP or ok to expose to world (`/metadata`)
- `/scim/v2`: authenticated via HTTP simple auth.
- `~* ^/teams/invitations/info$`: only `GET`; requires invitation code.
- `~* ^/teams/invitations/by-email$`: only `HEAD`.
- `/invitations/info`: discontinued feature, can be removed from nginz config.
- `/conversations/code-check`: link validation for ephemeral/guest users.
- `/provider/*`: bots need to be registered to a team before becoming active.  so if an attacker does not get access to a team, they cannot deploy a bot.
- `~* ^/custom-backend/by-domain/([^/]*)$`: only `GET`; only exposes a list of domains that has is maintained through an internal end-point.  used to redirect stock clients from the cloud instance to on-prem instances.
- `~* ^/teams/api-docs`: only `GET`; swagger for part of the rest API.  safe: it is trivial to identify the software that is running on the instance, and from there it is trivial to get to the source on github, where this can be obtained easily, and more.
- `/billing`: separate billing service, usually not installed for on-prem instances.
- `/calling-test`: separate testing service that has its own authentication.
