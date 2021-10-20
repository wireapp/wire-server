# ldap-scim-bridge

To do a test deployment on a existing cluster from a machine able to deploy helm charts…
```bash
git clone wire-server
cd wire-server
# deploy test instance of openldap with preloaded users
helm upgrade --install -n wire openldap charts/openldap/
# deploy ldap-scim-bridge with default chart values
helm upgrade --install -n wire ldap-scim-bridge charts/ldap-scim-bridge -f charts/ldap-scim-bridge/values.yaml
```

The kubernetes cronjob resource will spawn a new `ldap-scim-bridge-XXXXXX` pod every minute. Logs for the pod can be gathered with `kubectl log`.
```
kubectl get pods -n wire
kubectl logs ldap-scim-bridge-XXXXXX -n wire
```
