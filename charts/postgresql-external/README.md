This is the PostgreSQL chart.

Configure the values.yaml file to create the database, username, password and other configuration.

To deploy the helm chart - 
```
helm install postgresql charts/postgresql-external --values charts/postgresql-external/values.yaml
```
