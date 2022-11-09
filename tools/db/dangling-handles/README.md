# How to run and make sense of data

1. Build image

```
make build-image-dangling-handles
```
2. Push image

```
docker tag <image-built-in-step1> <image-in-your-personal-docker-repo>
```

3. Run it in K8s using this pod yaml **Update image field appropriately**:

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: dangling-handles
  labels:
    app: dangling-handles
spec:
  restartPolicy: Never
  containers:
  - name: dangling-handles
    image: <image-in-your-personal-docker-repo>
    imagePullPolicy: Always
    args:
    - handle-less-users
    - --cassandra-host-brig
    - brig-brig-eks-service.databases
    - --cassandra-keyspace-brig
    - brig
    - --inconsistencies-file
    - /inconsistencies.log
```

4. Wait for the process to finish. Watch logs, it will say something like "sleeping for 4 hours" and then close all connections to cassandra.

5. Copy the logs using `kubectl cp`

```
kubectl cp dangling-handles:/inconsistencies.log inconsistencies.log
```

6. **IMPORTANT:** Delete the pod.

7. Convert logs into CSV:

```bash
cat <log-file> |
    jq -r '[.userId, .status.value, .status.writetime, .userHandle.value, .userHandle.writetime, .handleClaimUser.value, .handleClaimUser.writetime] | @csv' >! handle-less-users.csv
```
