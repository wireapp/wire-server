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
# with AD

## Add Certificate
add your certificate until it looks like the following:

```
ucc@s-admin-host:~/Wire-Server$ d kubectl describe configmap ca-ad-pemstore
Name:         ca-ad-pemstore
Namespace:    default
Labels:       <none>
Annotations:  <none>

Data
====
ad-root.crt:
----
-----BEGIN CERTIFICATE-----
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
<CERTIFICATE DATA GOES HERE>
-----END CERTIFICATE-----

Events:  <none>
```

## use certificate

add the following patch after deployment to use the AD certificate.
kubectl patch cronjob ldap-scim-bridge-team-1 "$(cat add-ad-patch.patch)"
```
spec:
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: ldap-scim-bridge
            volumeMounts:
            - name: ca-ad-pemstore
	          mountPath: /etc/ssl/certs/ad-root.crt
	          subPath: ad-root.crt
	          readOnly: false
	    volumes:
	    - name: ca-ad-pemstore
	      configMap:
	        name: ca-ad-pemstore
```
