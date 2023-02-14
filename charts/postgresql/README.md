This is the PostgreSQL Bitnami chart.

Configure the values.yaml file to create the database, username, password and other configuration.
List of parameters available - https://artifacthub.io/packages/helm/bitnami/postgresql#parameters

For persisting data, you can configure your choice of storageClass with your provisioner.

Or, create a storage class and of PVs based on no. of replicas with no provisioner - 
```
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: local-storage
provisioner: kubernetes.io/no-provisioner
volumeBindingMode: WaitForFirstConsumer
reclaimPolicy: Retain
allowVolumeExpansion: true

---

Prepare volume on host and give right permissions - 
mkdir -p /data/local-storage
chmod -R 775 /data/local-storage

---
apiVersion: v1
kind: PersistentVolume
metadata:
  name: local-pv
spec:
  capacity:
    storage: 10Gi
  accessModes:
  - ReadWriteMany
  persistentVolumeReclaimPolicy: Retain
  storageClassName: local-storage
  hostPath:
    path: /data/local-storage
```

To deploy the helm chart - 
```
helm install postgresql charts/postgresql --values charts/postgresql/values.yaml
```
