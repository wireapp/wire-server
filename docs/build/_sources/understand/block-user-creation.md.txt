# Block personal user creation

## In Brig

There are some unauthenticated end-points that allow arbitrary users on the open internet to do things like create a new team.  This is desired in the cloud, but if you run an on-prem setup that is open to the world, you may want to block this.

Brig has a server option for this:

```yaml
optSettings:
  setRestrictUserCreation: true
```

If `setRestrictUserCreation` is `true`, creating new personal users or new teams on your instance from outside your backend installation is impossible.  (If you want to be more technical: requests to `/register` that create a new personal account or a new team are answered with `403 forbidden`.)

On instances with restricted user creation, the site operator with access to the internal REST API can still circumvent the restriction: just log into a brig service pod via ssh and follow the steps in `hack/bin/create_test_team_admins.sh.`

```{note}
Once the creation of new users and teams has been disabled, it will still be possible to use the [team creation process](https://support.wire.com/hc/en-us/articles/115003858905-Create-a-team) (enter the new team name, email, password, etc), but it will fail/refuse creation late in the creation process (after the «Create team» button is clicked).
```

## In the WebApp

Another way of disabling user registration is by this webapp setting, in `values.yaml`, changing this value from `true` to `false`:

```yaml
FEATURE_ENABLE_ACCOUNT_REGISTRATION: "false"
```

```{note}
If you only disable the creation of users in the webapp, but do not do so in Brig/the backend, a malicious user would be able to use the API to create users, so make sure to disable both.
```


