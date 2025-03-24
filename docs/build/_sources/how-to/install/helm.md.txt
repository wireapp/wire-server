(helm)=

# Installing wire-server (demo) components using helm

## Introduction

The following will install a demo version of all the wire-server components including the databases. This setup is not recommended in production but will get you started.

Demo version means

- easy setup - only one single machine with kubernetes is needed (make sure you have at least 4 CPU cores and 8 GB of memory available)
- no data persistence (everything stored in memory, will be lost)

### What will be installed?

- wire-server (API)
  \-  user accounts, authentication, conversations
  \-  assets handling (images, files, ...)
  \-  notifications over websocket
- wire-webapp, a fully functioning web client (like `https://app.wire.com`)
- wire-account-pages, user account management (a few pages relating to e.g. password reset)

### What will not be installed?

- notifications over native push notifications via [FCM](https://firebase.google.com/docs/cloud-messaging/)/[APNS](https://developer.apple.com/notifications/)
- audio/video calling servers using {ref}`understand-restund`)
- team-settings page

## Prerequisites

You need to have access to a kubernetes cluster, and the `helm` local binary on your PATH.

- If you don't have a kubernetes cluster, you have two options:

  - You can get access to a managed kubernetes cluster with the cloud provider of your choice.
  - You can install one if you have ssh access to a virtual machine, see {ref}`ansible-kubernetes`

- If you don't have `helm` yet, see [Installing helm](https://helm.sh/docs/using_helm/#installing-helm).

Type `helm version`, you should, if everything is configured correctly, see a result like this:

```
version.BuildInfo{Version:"v3.1.1", GitCommit:"afe70585407b420d0097d07b21c47dc511525ac8", GitTreeState:"clean", GoVersion:"go1.13.8"}
```

In case `kubectl version` shows both Client and Server versions, but `helm version` does not show a Server version, you may need to run `helm init`. The exact version (assuming `v2.X.X` - at the time of writing v3 is not yet supported) matters less as long as both Client and Server versions match (or are very close).

## How to start installing charts from wire

Enable the wire charts helm repository:

```shell
helm repo add wire https://s3-eu-west-1.amazonaws.com/public.wire.com/charts
```

(You can see available helm charts by running `helm search repo wire/`. To see
new versions as time passes, you may need to run `helm repo update`)

Great! Now you can start installing.

```{note}
all commands below can also take an extra `--namespace <your-namespace>` if you don't want to install into the default kubernetes namespace.
```

## Watching changes as they happen

Open a terminal and run

```shell
kubectl get pods -w
```

This will block your terminal and show some things happening as you proceed through this guide. Keep this terminal open and open a second terminal.

## How to install in-memory databases and external components

In your second terminal, first install databases:

```shell
helm upgrade --install databases-ephemeral wire/databases-ephemeral --wait
```

You should see some pods being created in your first terminal as the above command completes.

You can do the following two steps (mock aws services and demo smtp
server) in parallel with the above in two more terminals, or
sequentially after database-ephemeral installation has succeeded.

```shell
helm upgrade --install fake-aws wire/fake-aws --wait
helm upgrade --install smtp wire/demo-smtp --wait
```

## How to install wire-server itself

```{note}
The following makes use of overrides for helm charts. You may wish to read {ref}`understand-helm-overrides` first.
```

Change back to the wire-server-deploy directory.  Copy example demo values and secrets:

```shell
mkdir -p wire-server && cd wire-server
cp ../values/wire-server/demo-secrets.example.yaml secrets.yaml
cp ../values/wire-server/demo-values.example.yaml values.yaml
```

Or, if you are not in wire-server-deploy, download example demo values and secrets:

```shell
mkdir -p wire-server && cd wire-server
curl -sSL https://raw.githubusercontent.com/wireapp/wire-server-deploy/master/values/wire-server/demo-secrets.example.yaml > secrets.yaml
curl -sSL https://raw.githubusercontent.com/wireapp/wire-server-deploy/master/values/wire-server/demo-values.example.yaml > values.yaml
```

Open `values.yaml` and replace `example.com` and other domains and subdomains with domains of your choosing. Look for the `# change this` comments. You can try using `sed -i 's/example.com/<your-domain>/g' values.yaml`.

Generate some secrets (if you are using the docker image from {ref}`ansible-kubernetes`, you should open a shell on the host system for this):

```shell
openssl rand -base64 64 | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 42 > restund.txt
docker run --rm quay.io/wire/alpine-intermediate /dist/zauth -m gen-keypair -i 1 > zauth.txt
```

1. Add the generated secret from restund.txt to secrets.yaml under `brig.secrets.turn.secret`
2. add **both** the public and private parts from zauth.txt to secrets.yaml under `brig.secrets.zAuth`
3. Add the public key from zauth.txt **also** to secrets.yaml under `nginz.secrets.zAuth.publicKeys`

You can do this with an editor, or using sed:

```shell
sed -i 's/secret:$/secret: content_of_restund.txt_file/' secrets.yaml
sed -i 's/publicKeys: "<public key>"/publicKeys: "public_key_from_zauth.txt_file"/' secrets.yaml
sed -i 's/privateKeys: "<private key>"/privateKeys: "private_key_from_zauth.txt_file"/' secrets.yaml
```

Great, now try the installation:

```shell
helm upgrade --install wire-server wire/wire-server -f values.yaml -f secrets.yaml --wait
```

```{eval-rst}
.. include:: includes/helm_dns-ingress-troubleshooting.inc.rst
```
