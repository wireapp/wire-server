This is the PostgreSQL 13 chart from Bitnami.

To deploy the chart, first we need to create a persistant volume and persistant volume claim which will be used by the Postgres deployment.

To create the pv and pvc, run
```
kubectl create -f pv_pvc.yaml
```

You can edit the storage size in the pv_pvc.yaml file.

Now configure the values.yaml file to create the database, username, password and other configuration.

Set of all configuration variables can be found here - https://github.com/bitnami/charts/blob/main/bitnami/postgresql/values.yaml

Now, deploy the helm chart - 
```
helm install postgresql external-charts/postgresql-11.9.8/postgresql-11.9.8.tgz --values external-charts/postgresql-11.9.8/values.yaml
```
