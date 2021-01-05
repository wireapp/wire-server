Team settings are part of a private repo. As such, this chart expects a secret named `wire-teamsettings-readonly-pull-secret` to be made available as a secret. Check the [values file](values.yaml) for more info.

kubectl create -f wire-teamsettings-readonly-pull-secret.yml --namespace=<namespace>

If you want to get access to it, get in [touch with us](https://wire.com/pricing/).
