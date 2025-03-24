(ansible-kubernetes)=

# Installing kubernetes for a demo installation (on a single virtual machine)

## How to set up your hosts.ini file

Assuming a single virtual machine with a public IP address running Ubuntu 18.04, with at least 5 CPU cores and at least 8 GB of memory.

Move to `wire-server-deploy/ansible`:

```shell
cd ansible/
```

Then:

```{eval-rst}
.. include:: includes/ansible-authentication-blob.rst
```

## Passwordless authentication

Presuming a fresh default Ubuntu 18.04 installation, the following steps will enable the Ansible playbook to run without specifying passwords.

This presumes you named your default Ubuntu user "wire", and X.X.X.X is the IP or domain name of the target server Ansible will install Kubernetes on.

On the client (from `wire-server-deploy/ansible`), run:

```shell
ssh-keygen -f /root/.ssh/id_rsa -t rsa -P
ssh-copy-id wire@X.X.X.X
sed -i 's/# ansible_user = .../ansible_user = wire/g' inventory/demo/hosts.ini
```

And on the server (X.X.X.X), run:

```shell
echo 'wire ALL=(ALL) NOPASSWD:ALL' | sudo tee -a /etc/sudoers
```

Then on the client:

```shell
cp inventory/demo/hosts.example.ini inventory/demo/hosts.ini
```

Open hosts.ini and replace `X.X.X.X` with the IP address of your virtual machine that you use for ssh access.  You can try using:

```shell
sed -i 's/X.X.X.X/1.2.3.4/g' inventory/demo/hosts.ini
```

## Minio setup

In the `inventory/demo/hosts.ini` file, edit the minio variables in `[minio:vars]` (`prefix`, `domain` and `deeplink_title`)
by replacing `example.com` with your own domain.

## How to install kubernetes

From `wire-server-deploy/ansible`:

```
ansible-playbook -i inventory/demo/hosts.ini kubernetes.yml -vv
```

When the playbook finishes correctly (which can take up to 20 minutes), you should have a folder `artifacts` containing a file `admin.conf`. Copy this file:

```
mkdir -p ~/.kube
cp artifacts/admin.conf ~/.kube/config
KUBECONFIG=~/.kube/config
```

Make sure you can reach the server:

```
kubectl version
```

should give output similar to this:

```
Client Version: version.Info{Major:"1", Minor:"14", GitVersion:"v1.14.2", GitCommit:"66049e3b21efe110454d67df4fa62b08ea79a19b", GitTreeState:"clean", BuildDate:"2019-05-16T16:23:09Z", GoVersion:"go1.12.5", Compiler:"gc", Platform:"linux/amd64"}
Server Version: version.Info{Major:"1", Minor:"14", GitVersion:"v1.14.2", GitCommit:"66049e3b21efe110454d67df4fa62b08ea79a19b", GitTreeState:"clean", BuildDate:"2019-05-16T16:14:56Z", GoVersion:"go1.12.5", Compiler:"gc", Platform:"linux/amd64"}
```
