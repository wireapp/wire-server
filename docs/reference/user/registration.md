# Registration {#RefRegistration}

_Author: Artyom Kazak_

---

This page describes the "normal" user registration flow. Autoprovisioning is covered separately.

## Summary {#RefRegistrationSummary}

The vast majority of our API is only available to Wire users. Unless a user is autoprovisioned, they have to register an account by calling the `POST /register` endpoint.

The next step after registration is [activation](activation.md) -- sharing and verifying an email address and/or phone number with Wire. [Certain functionality](activation.md#RefActivationBenefits) is only available to activated users. If you are implementing a Wire client, you can either force the user to go through the activation flow, or hide unavailable functionality until the user has been activated.

## Standard registration flow {#RefRegistrationStandard}

During the standard registration flow, the user provides their name, email/phone/both, and optionally a password. Phone numbers must be in [E.164][] format.

[E.164]: https://en.wikipedia.org/wiki/E.164

Sample API request and response:

```
POST /register

{
    // The name is mandatory
    "name": "Pink",

    // At least one of 'email' or 'phone' has to be provided
    "email": "pink@example.com",
    "phone": "+15417543010",

    // The password is optional
    "password": "secret"
}
```

```
Set-Cookie: zuid=...

{
    "accent_id": 0,
    "assets": [],
    "email": "pink@example.com",
    "id": "c193136a-55fb-4534-ad72-d02a72bb16af",
    "locale": "en",
    "managed_by": "wire",
    "name": "Pink",
    "phone": "+15417543010",
    "picture": []
}
```

A verification email will be sent to the email address (if provided), and a verification text message will be sent to the provided phone number (if provided). This code can be used to activate the account later; see [activating an existing account](activation.md#RefActivationSubmit).

### Preverified accounts {#RefRegistrationPreverified}

If the phone number or email address has already been verified by [requesting an activation code](activation.md#RefActivationRequest), this code can be added to the `POST /register` request. In this case the newly created account will be activated immediately.

```
POST /register

{
    "name": "Pink",
    "email": "pink@example.com",

    // 6-digit 'email_code' or 'phone_code'
    "email_code": 123456
}
```

> An account that has a verified phone number can perform a login via email. A phone number can be added to the account later.
>
> To register with only a phone number, send the `phone` field together with the `name`. An activation code will be sent to the provided phone number via SMS. Alternatively, a phone activation code can be sent to the phone prior to the registration request, by sending an upfront `POST` request to `/activate/send`. The activation code will be sent to the provided email address and can then be provided as part of the registration request:
>
> An account that has a verified phone number can perform a login via SMS. An email address can be added to the account later.

## Anonymous registration, aka "Wireless" {#RefRegistrationWireless}

A user can be created without either email or phone number, in which case only `"name"` is required. The `"name"` does not have to be unique. This feature is used for [guest rooms](https://wire.com/en/features/encrypted-guest-rooms/).

An anonymous, non-activated user is only "usable" for a period of time specified in `brig.yaml` at `zauth.authSettings.sessionTokenTimeout`, which is set to 1 day for Wire production. (The access cookie returned by `/register` can not be refreshed, and an anonymous user can not use `/login` to get a new cookie.)

Sample API request and response:

```
POST /register

{
    "name": "Pink"
}
```

```
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

The response contains an access cookie that can be used to access the rest of the backend API. The cookie can be assigned a label by adding a `"label": <...>` field when calling `/register`.

TODO link to cookie and label documentation, once it's written.
