# Implementation plan

There are two types of implementation: demo and production.

## Demo installation (trying functionality out)

Please note that there is no way to migrate data from a demo
installation to a production installation - it is really meant as a way
to try things out.

Please note your data will be in-memory only and may disappear at any given moment!

What you need:

- a way to create **DNS records** for your domain name (e.g.
  `wire.example.com`)

- a way to create **SSL/TLS certificates** for your domain name (to allow
  connecting via `https://`)

- Either one of the following:

  - A kubernetes cluster (some cloud providers offer a managed
    kubernetes cluster these days).
  - One single virtual machine running ubuntu 18.04 with at least 20 GB of disk, 8 GB of memory, and 8 CPU cores.

A demo installation will look a bit like this:

```{figure} img/architecture-demo.png
Demo installation (1 VM)
```

### Next steps for demo installation

If you already have a kubernetes cluster, your next step will be {ref}`helm`, otherwise, your next step will be {ref}`ansible-kubernetes`

(planning-prod)=

## Production installation (persistent data, high-availability)

What you need:

- a way to create **DNS records** for your domain name (e.g. `wire.example.com`)
- a way to create **SSL/TLS certificates** for your domain name (to allow connecting via `https://wire.example.com`)
- A **kubernetes cluster with at least 3 worker nodes and at least 3 etcd nodes** (some cloud providers offer a managed kubernetes cluster these days)
- minimum **17 virtual machines** for components outside kubernetes (cassandra, minio, elasticsearch, redis, restund)

A recommended installation of Wire-server in any regular data centre,
configured with high-availability will require the following virtual
servers:

```{eval-rst}
.. include:: includes/vm-table.rst
```

A production installation will look a bit like this:

```{figure} img/architecture-server-ha.png
Production installation in High-Availability mode
```

If you use a private datacenter (not a cloud provider), the easiest is
to have three physical servers, each with one virtual machine for each
server component (cassandra, minio, elasticsearch, redis, kubernetes,
restund)

It's up to you how you create these VMs - kvm on a bare metal machine,
VM on a cloud provider, etc. Make sure they run ubuntu 18.04.

Ensure that your VMs have IP addresses that do not change.

Avoid `10.x.x.x` network address schemes, and instead use something like `192.168.x.x` or `172.x.x.x`. This is because internally, Kubernetes already uses a `10.x.x.x` address scheme, creating a potential conflict.

### Next steps for high-available production installation

Your next step will be {ref}`ansible-vms`
