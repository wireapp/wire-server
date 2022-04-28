# Feature settings

## 2nd factor password challenge

By default Wire enforces a 2nd factor authentication for certain user operations like e.g. activating an account, changing email or password, or deleting an account.

If the `sndFactorPasswordChallenge` feature is enabled, a 6 digit verification code will be send per email to authenticate for additional user operations like e.g. for login, adding a new client, generating SCIM tokens, or deleting a team.

Usually the default is what you want. If you explicitly want to enable additional password challenges, add the following to your Helm overrides in `values/wire-server/values.yaml`:

```yaml
galley:
  # ...
  config:
    # ...
    settings:
      # ...
      featureFlags:
        # ...
        sndFactorPasswordChallenge:
          defaults:
            status: enabled
            lockStatus: locked # note that the lockstatus is required and should be locked
```
