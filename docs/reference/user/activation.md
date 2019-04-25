# Activation {#RefActivation}

_Author: Artyom Kazak_

---

A user is called _activated_ they have a verified identity -- e.g. a phone number that has been verified via a text message, or an email address that has been verified by sending an activation code to it.

A user that has been provisioned via single sign-on is always considered to be activated.

## Activated vs. non-activated users {#RefActivationBenefits}

Most functionality is available to all users, but non-activated users can not [connect](connection.md) to others, nor can connection requests be made to anonymous accounts from verified accounts. As a result:

* A non-activated user cannot add other users to conversations. The only way to participate in a conversation is to either create a new conversation with link access or to use a link provided by another user.

* A non-activated user cannot get auto-connected to others as a result of an address book upload.

* A non-activated user can only ever see the public profiles of other users, i.e. no email addresses, phone numbers or other personal information.

## API {#RefActivationApi}

### Requesting an activation code {#RefActivationRequest}

During the [standard registration flow](registration.md#RefRegistrationStandard), an activation code will be sent to provided email/phone. However, it is also possible to request an activation code _before_ registering an account:

```
POST /activate/send

{
    // Either 'email' or 'phone'
    "phone": "+1234567890"
}
```

The activation code can then be submitted during registration, see [preverified accounts](registration.md#RefRegistrationPreverified).

The same endpoint can also be used to re-request an activation code. Please use this sparingly! To avoid unnecessary activation code requests, users should be warned that it might take up to a few minutes for an email or text message to arrive.

### Activating an existing account {#RefActivationSubmit}

An existing account can be activated by submitting an activation code to `POST /activate`. Sample request and response:

```
POST /activate

{
    // One of 'phone', 'email', or 'key'
    "phone": "+1234567890",

    // 6-digit activation code
    "code": "123456",

    // Verify the 'code' but don't activate the account (the 3-attempt limit
    // on failed verification attempts still applies)
    "dryrun": false
}
```

```
{
    "phone": "+1234567890",

    // Whether it is the first successful activation for the user
    "first": true
}
```

If the email or phone has been verified already, `POST /activate` will return status code `204 No Content`. If the code is invalid, `POST /activate` will return status code `404 Not Found` with `"label": "invalid-code"`.

There is a maximum of 3 activation attempts per activation code. On the third failed attempt the code is invalidated and a new one must be requested.

### Activation event {#RefActivationEvent}

When the user becomes activated, they receive an event:

```json
{
    "type": "user.activate",
    "user": <self profile>
}
```

### Detecting activation in the self profile {#RefActivationProfile}

In addition to the [activation event](#RefActivationEvent), activation can be detected by polling the self profile:

```
GET /self

{
    "accent_id": 0,
    "assets": [],
    "email": "pink@example.com",
    "id": "2f7e582b-9d99-4d50-bbb0-e659d63491d9",
    "locale": "en",
    "managed_by": "wire",
    "name": "Pink",
    "picture": []
}
```

If the profile includes `"email"` or `"phone"`, the account is activated.

## Automating activation via email {#RefActivationEmailHeaders}

Our email verification messages contain headers that can be used to automate the activation process. One of these sets of headers will be present:

```
X-Zeta-Purpose: Activation
X-Zeta-Key: ...
X-Zeta-Code: 123456
```

```
X-Zeta-Purpose: Verification
X-Zeta-Code: 123456
```

In the first case, the opaque `"key"` might be used instead of `"email"` in the `POST /activate` request.

TODO: why? When do we send the key vs. just the 6-digit code?

TODO: would be nice to give more motivation for the headers.

## Phone/email whitelist {#RefActivationWhitelist}

The backend can be configured to only allow specific phone numbers or email addresses to register. The following options have to be set in `brig.yaml`:

```yaml
optSettings:
  setWhitelist:
    whitelistUrl: ...     # Checker URL
    whitelistUser: ...    # Basic auth username
    whitelistPass: ...    # Basic auth password
```

When those options are present, the backend will do a GET request at `<whitelistUrl>?email=...` or `<whitelistUrl>?mobile=...` for every activation request it receives. It will expect either status code 200 ("everything good") or 404 ("provided email/phone is not on the whitelist").

If an email address or phone number are rejected by the whitelist, `POST /activate/send` or `POST /register` will return `403 Forbidden`:

```json
{
    "code": 403,
    "label": "unauthorized",
    "message": "Unauthorized e-mail address or phone number."
}
```

Currently emails at `@wire.com` are always considered whitelisted, regardless of the whitelist service's response.
