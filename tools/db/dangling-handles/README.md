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
    - --cassandra-host-brig
    - brig-brig-eks-service.databases
    - --cassandra-keyspace-brig
    - brig
    - --log-file
    - /dangling-handles.log
```

4. Wait for the process to finish. Go into the container and `tail -f /dangling-handles.log`. Once it stops, it can be assumed that the script finished and is waiting in case someone wants to copy the logs out.

5. Copy the logs using `kubectl cp`

6. **IMPORTANT:** Delete the pod.

7. Convert logs into CSV:

```bash
cat <log-file> |
    jq -c -r 'select (.handle)' |
    jq -r '(map(keys) | add | unique | map(select(. != "msgs")) ) as $cols | map(. as $row | $cols | map($row[.])) as $rows | $cols, $rows[] | @csv' > dangling-handles.csv
```
