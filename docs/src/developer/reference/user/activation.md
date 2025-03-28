# User Activation

Reference: {#RefActivation}

*Author: Artyom Kazak*

---

A user is called *activated* when they have a verified identity – an email
address that has been verified by sending an activation code to it.

A user that has been provisioned via single sign-on is always considered to be activated.

## Activated vs. non-activated users

<a id="refactivationbenefits"></a>

Non-activated users can not [connect](connection.md) to others, nor can connection requests be made to anonymous accounts from verified accounts. As a result:

* A non-activated user cannot add other users to conversations. The only way to participate in a conversation is to either create a new conversation with link access or to use a link provided by another user.

The only flow where it makes sense for non-activated users to exist is the [wireless flow](registration.md#refregistrationwireless) used for [guest rooms](https://wire.com/en/features/encrypted-guest-rooms/)

## API

<a id="refactivationapi"></a>

### Requesting an activation code

<a id="refactivationrequest"></a>

During the [standard registration flow](registration.md#refregistrationstandard), the user
submits an email address by making a request to `POST /activate/send`. A
six-digit activation code will be sent to that email address. Sample request and
response:

```default
POST /activate/send

{
    // the user's 'email' address
    "email": "pink@example.com"
}
```

```default
200 OK
```

The user can submit the activation code during registration to prove that they
own the email address.

The same `POST /activate/send` endpoint can be used to re-request an activation
code. Please use this ability sparingly! To avoid unnecessary activation code
requests, users should be warned that it might take up to a few minutes for an
email to arrive.

### Activating an existing account

<a id="refactivationsubmit"></a>

If the account [has not been activated during verification](registration.md#refregistrationnopreverification), it can be activated afterwards by submitting an activation code to `POST /activate`. Sample request and response:

```default
POST /activate

{
    // One of 'email', 'key'
    "email": "pink@example.com",

    // 6-digit activation code
    "code": "123456",

    // Verify the 'code' but don't activate the account (the 3-attempt limit
    // on failed verification attempts still applies)
    "dryrun": false
}
```

```default
200 OK

{
    "email": "pink@example.com",

    // Whether it is the first successful activation for the user
    "first": true
}
```

If the email has been verified already, `POST /activate` will return status code
`204 No Content`. If the code is invalid, `POST /activate` will return status
code `404 Not Found` with `"label": "invalid-code"`.

There is a maximum of 3 activation attempts per activation code. On the third failed attempt the code is invalidated and a new one must be requested.

### Activation event

<a id="refactivationevent"></a>

When the user becomes activated, they receive an event:

```default
{
    "type": "user.activate",
    "user": <self profile>
}
```

### Detecting activation in the self profile

<a id="refactivationprofile"></a>

In addition to the [activation event](#refactivationevent), activation can be detected by polling the self profile:

```default
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

If the profile includes `"email"`, the account is activated.

## Automating activation via email

<a id="refactivationemailheaders"></a>

Our email verification messages contain headers that can be used to automate the activation process.

An email caused by `POST /activate/send` will contain this set of headers:

```default
X-Zeta-Purpose: Verification
X-Zeta-Code: 123456
```

An email caused by `POST /register` will contain this set of headers (the opaque `"key"` might be used instead of `"email"` in the `POST /activate` request):

```default
X-Zeta-Purpose: Activation
X-Zeta-Key: ...
X-Zeta-Code: 123456
```

## Email whitelist

<a id="refactivationallowlist"></a>

The backend can be configured to only allow specific email address domains to register. The following option has to be set in `brig.yaml`:

```yaml
optSettings:
  setAllowlistEmailDomains:
    - wire.com
    - example.com
    - notagoodexample.com
```

When those options are present, the backend will match every activation request against these lists.

If an email address is rejected by the whitelist, `POST /activate/send` or `POST /register` will return `403 Forbidden`:

```json
{
    "code": 403,
    "label": "unauthorized",
    "message": "Unauthorized e-mail address"
}
```
