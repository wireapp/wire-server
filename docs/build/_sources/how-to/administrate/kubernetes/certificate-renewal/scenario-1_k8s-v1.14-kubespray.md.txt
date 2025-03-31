# How to renew certificates on kubernetes 1.14.x

Kubernetes-internal certificates by default (see assumptions) expire after one year. Without renewal, your installation will cease to function.
This page explains how to renew certificates.

## Assumptions

- Kubernetes version 1.14.x

- installed with the help of [Kubespray](https://github.com/kubernetes-sigs/kubespray)

  - This page was tested using kubespray release 2.10 branch from 2019-05-20, i.e. commit `e2f5a9748e4dbfe2fdba7931198b0b5f1f4bdc7e`.

- setup: 3 scheduled nodes, each hosting master (control plane) +
  worker (kubelet) + etcd (cluster state, key-value database)

*NOTE: due to Kubernetes being installed with Kubespray, the Kubernetes
CAs (expire after 10yr) as well as certificates involved in etcd
communication (expire after 100yr) are not required to be renewed (any
time soon).*

**Official documentation:**

- [Certificate Management with kubeadm (v1.14)](https://v1-14.docs.kubernetes.io/docs/tasks/administer-cluster/kubeadm/kubeadm-certs/)
- [PKI certificates and requirements (v1.14)](https://v1-14.docs.kubernetes.io/docs/setup/best-practices/certificates/)

## High-level description

1. verify current expiration date
2. issue new certificates
3. generate new client configuration (aka kubeconfig file)
4. restart control plane
5. drain node - restart kubelet - uncordon node again
6. repeat 3-5 on all other nodes

## Step-by-step instructions

*Please note, that the following instructions may require privileged
execution. So, either switch to a privileged user or prepend following
statements with \`\`sudo\`\`. In any case, it is most likely that every
newly created file has to be owned by \`\`root\`\`, depending on kow
Kubernetes was installed.*

1. Verify current expiration date on each node

```bash
export K8S_CERT_DIR=/etc/kubernetes/pki
export ETCD_CERT_DIR=/etc/ssl/etcd/ssl
export KUBELET_CERT_DIR=/var/lib/kubelet/pki


for crt in ${K8S_CERT_DIR}/*.crt; do
    expirationDate=$(openssl x509 -noout -text -in ${crt} | grep After | sed -e 's/^[[:space:]]*//')
    echo "$(basename ${crt}) -- ${expirationDate}"
done


for crt in $(ls ${ETCD_CERT_DIR}/*.pem | grep -v 'key'); do
    expirationDate=$(openssl x509 -noout -text -in ${crt} | grep After | sed -e 's/^[[:space:]]*//')
    echo "$(basename ${crt}) -- ${expirationDate}"
done

echo "kubelet-client-current.pem -- $(openssl x509 -noout -text -in ${KUBELET_CERT_DIR}/kubelet-client-current.pem | grep After | sed -e 's/^[[:space:]]*//')"
echo "kubelet.crt -- $(openssl x509 -noout -text -in ${KUBELET_CERT_DIR}/kubelet.crt | grep After | sed -e 's/^[[:space:]]*//')"


# MASTER: api-server cert
echo -n | openssl s_client -connect localhost:6443 2>&1 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' | openssl x509 -text -noout | grep Not
# MASTER: controller-manager cert
echo -n | openssl s_client -connect localhost:10257 2>&1 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' | openssl x509 -text -noout | grep Not
# MASTER: scheduler cert
echo -n | openssl s_client -connect localhost:10259 2>&1 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' | openssl x509 -text -noout | grep Not

# WORKER: kubelet cert
echo -n | openssl s_client -connect localhost:10250 2>&1 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' | openssl x509 -text -noout | grep Not
```

2. Allocate a terminal session on one node and backup existing
   certificates & configurations

```bash
cd /etc/kubernetes

cp -r ./ssl ./ssl.bkp

cp admin.conf admin.conf.bkp
cp controller-manager.conf controller-manager.conf.bkp
cp scheduler.conf scheduler.conf.bkp
cp kubelet.conf kubelet.conf.bkp
```

3. Renew certificates on that very node

```bash
kubeadm alpha certs renew apiserver
kubeadm alpha certs renew apiserver-kubelet-client
kubeadm alpha certs renew front-proxy-client
```

*Looking at the timestamps of the certificates, it is indicated, that apicerver, kubelet & proxy-client have been
renewed. This can be confirmed, by executing parts of (1).*

```
root@kubenode01:/etc/kubernetes$ ls -al ./ssl
total 56
drwxr-xr-x 2 kube root 4096 Mar 20 17:09 .
drwxr-xr-x 5 kube root 4096 Mar 20 17:08 ..
-rw-r--r-- 1 root root 1517 Mar 20 15:12 apiserver.crt
-rw------- 1 root root 1675 Mar 20 15:12 apiserver.key
-rw-r--r-- 1 root root 1099 Mar 20 15:13 apiserver-kubelet-client.crt
-rw------- 1 root root 1675 Mar 20 15:13 apiserver-kubelet-client.key
-rw-r--r-- 1 root root 1025 Sep 23 14:53 ca.crt
-rw------- 1 root root 1679 Sep 23 14:53 ca.key
-rw-r--r-- 1 root root 1038 Sep 23 14:53 front-proxy-ca.crt
-rw------- 1 root root 1679 Sep 23 14:53 front-proxy-ca.key
-rw-r--r-- 1 root root 1058 Mar 20 15:13 front-proxy-client.crt
-rw------- 1 root root 1675 Mar 20 15:13 front-proxy-client.key
-rw------- 1 root root 1679 Sep 23 14:53 sa.key
-rw------- 1 root root  451 Sep 23 14:53 sa.pub
```

4. Based on those renewed certificates, generate new kubeconfig files

The first command assumes it's being executed on a master node. You may need to swap `masters` with `nodes` in
case you are on a different sort of machines.

```bash
kubeadm alpha kubeconfig user --org system:masters --client-name kubernetes-admin  > /etc/kubernetes/admin.conf
kubeadm alpha kubeconfig user --client-name system:kube-controller-manager > /etc/kubernetes/controller-manager.conf
kubeadm alpha kubeconfig user --client-name system:kube-scheduler > /etc/kubernetes/scheduler.conf
```

*Again, check if ownership and permission for these files are the same
as all the others around them.*

And, in case you are operating the cluster from the current node, you may want to replace the user's kubeconfig.
Afterwards, compare the backup version with the new one, to see if any configuration (e.g. pre-configured *namespace*)
might need to be moved over, too.

```bash
mv ~/.kube/config ~/.kube/config.bkp
cp /etc/kubernetes/admin.conf ~/.kube/config
chown $(id -u):$(id -g) ~/.kube/config
chmod 770 ~/.kube/config
```

5. Now that certificates and configuration files are in place, the
   control plane must be restarted. They typically run in containers, so
   the easiest way to trigger a restart, is to kill the processes
   running in there. Use (1) to verify, that the expiration dates indeed
   have been changed.

```bash
kill -s SIGHUP $(pidof kube-apiserver)
kill -s SIGHUP $(pidof kube-controller-manager)
kill -s SIGHUP $(pidof kube-scheduler)
```

6. Make *kubelet* aware of the new certificate

1) Drain the node

```
kubectl drain --delete-local-data --ignore-daemonsets $(hostname)
```

2. Stop the kubelet process

```
systemctl stop kubelet
```

3. Remove old certificates and configuration

```
mv /var/lib/kubelet/pki{,old}
mkdir /var/lib/kubelet/pki
```

4. Generate new kubeconfig file for the kubelet

```
kubeadm alpha kubeconfig user --org system:nodes --client-name system:node:$(hostname) > /etc/kubernetes/kubelet.conf
```

5. Start kubelet again

```
systemctl start kubelet
```

6. \[Optional\] Verify kubelet has recognized certificate rotation

```
sleep 5 && systemctl status kubelet
```

7. Allow workload to be scheduled again on the node

```
kubectl uncordon $(hostname)
```

7. Copy certificates over to all the other nodes

Option A - you can ssh from one kubernetes node to another

```bash
# set the ip or hostname:
export NODE2=root@ip-or-hostname
export NODE3=...

scp ./ssl/apiserver.* "${NODE2}:/etc/kubernetes/ssl/"
scp ./ssl/apiserver.* "${NODE3}:/etc/kubernetes/ssl/"

scp ./ssl/apiserver-kubelet-client.* "${NODE2}:/etc/kubernetes/ssl/"
scp ./ssl/apiserver-kubelet-client.* "${NODE3}:/etc/kubernetes/ssl/"

scp ./ssl/front-proxy-client.* "${NODE2}:/etc/kubernetes/ssl/"
scp ./ssl/front-proxy-client.* "${NODE3}:/etc/kubernetes/ssl/"
```

Option B - copy via local administrator's machine

```bash
# set the ip or hostname:
export NODE1=root@ip-or-hostname
export NODE2=
export NODE3=

scp -3 "${NODE1}:/etc/kubernetes/ssl/apiserver.*" "${NODE2}:/etc/kubernetes/ssl/"
scp -3 "${NODE1}:/etc/kubernetes/ssl/apiserver.*" "${NODE3}:/etc/kubernetes/ssl/"

scp -3 "${NODE1}:/etc/kubernetes/ssl/apiserver-kubelet-client.*" "${NODE2}:/etc/kubernetes/ssl/"
scp -3 "${NODE1}:/etc/kubernetes/ssl/apiserver-kubelet-client.*" "${NODE3}:/etc/kubernetes/ssl/"

scp -3 "${NODE1}:/etc/kubernetes/ssl/front-proxy-client.*" "${NODE2}:/etc/kubernetes/ssl/"
scp -3 "${NODE1}:/etc/kubernetes/ssl/front-proxy-client.*" "${NODE3}:/etc/kubernetes/ssl/"
```

8. Continue again with (4) for each node that is left
