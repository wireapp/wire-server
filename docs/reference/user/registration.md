# Registration {#RefRegistration}

_Author: Artyom Kazak_

---

This page describes the "normal" user registration flow. Autoprovisioning is covered separately.

## Summary {#RefRegistrationSummary}

The vast majority of our API is only available to Wire users. Unless a user is autoprovisioned, they have to register an account by calling the `POST /register` endpoint.

Most users also go through [activation](activation.md) -- sharing and verifying an email address and/or phone number with Wire. This can happen either before or after registration. [Certain functionality](activation.md#RefActivationBenefits) is only available to activated users.

## Standard registration flow {#RefRegistrationStandard}

During the standard registration flow, the user first calls [`POST /activate/send`](activation.md#RefActivationRequest) to pre-verify their email address or phone number. Phone numbers must be in [E.164][] format.

[E.164]: https://en.wikipedia.org/wiki/E.164

After receiving a six-digit activation code via email/text message, it can be submitted with the registration request via `POST /register`. If the code is correct, the account will be activated immediately. Here is a sample request and response:

```
POST /register

{
    // The name is mandatory
    "name": "Pink",

    // 'email', 'phone', or both have to be provided
    "email": "pink@example.com",

    // The password is optional
    "password": "secret",

    // 6-digit 'email_code' or 'phone_code'
    "email_code": 123456
}
```

```
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

```
404 Not Found

{
    "code": 404,
    "label": "invalid-code",
    "message": "Invalid activation code"
}
```

## Registration without pre-verification {#RefRegistrationNoPreverification}

_NOTE: This flow is currently not used by any clients. At least this was the state on 2020-05-28_

It is also possible to call `POST /register` without verifying the email address or phone number, in which case the account will have to be activated later by calling [`POST /activate`](activation.md#RefActivationSubmit). Sample API request and response:

```
POST /register

{
    // The name is mandatory
    "name": "Pink",

    // 'email', 'phone', or both have to be provided
    "email": "pink@example.com",

    // The password is optional
    "password": "secret"
}
```

```
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

A verification email will be sent to the email address (if provided), and a verification text message will be sent to the phone number (also, if provided).

## Anonymous registration, aka "Wireless" {#RefRegistrationWireless}

A user can be created without either email or phone number, in which case only `"name"` is required. The `"name"` does not have to be unique. This feature is used for [guest rooms](https://wire.com/en/features/encrypted-guest-rooms/).

An anonymous, non-activated account is only usable for a period of time specified in `brig.yaml` at `zauth.authSettings.sessionTokenTimeout`, which is set to 1 day for Wire production. (The access cookie returned by `/register` can not be refreshed, and an anonymous user can not use `/login` to get a new cookie.)

Sample API request and response:

```
POST /register

{
    "name": "Pink"
}
```

```
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
