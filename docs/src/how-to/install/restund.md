(install-restund)=

# Installing Restund

## Background

Restund servers allow two users on different networks to have a Wire audio or video call.

Please refer to the following {ref}`section to better understand Restund and how it works <understand-restund>`.

## Installation instructions

To Install Restund, do the following:

1. In your `hosts.ini` file, in the `[restund:vars]` section, set
   the `restund_network_interface` to the name of the interface
   you want restund to talk to clients on. This value defaults to the
   `default_ipv4_address`, with a fallback to `eth0`.
2. (optional) `restund_peer_udp_advertise_addr=Y.Y.Y.Y`: set this to
   the IP to advertise for other restund servers if different than the
   ip on the 'restund_network_interface'. If using
   'restund_peer_udp_advertise_addr', make sure that UDP (!) traffic
   from any restund server (including itself) can reach that IP (for
   `restund <-> restund` communication). This should only be necessary
   if you're installing restund on a VM that is reachable on a public IP
   address but the process cannot bind to that public IP address
   directly (e.g. on AWS VPC VM). If unset, `restund <-> restund` UDP
   traffic will default to the IP in the `restund_network_interface`.

```ini
[all]
(...)
restund01         ansible_host=X.X.X.X

(...)

[all:vars]
## Set the network interface name for restund to bind to if you have more than one network interface
## If unset, defaults to the ansible_default_ipv4 (if defined) otherwise to eth0
restund_network_interface = eth0

(see `defaults/main.yml <https://github.com/wireapp/ansible-restund/blob/master/defaults/main.yml>`__ for a full list of variables to change if necessary)
```

3. Place a copy of the PEM formatted certificate and key you are going
   to use for TLS communication to the restund server in
   `/tmp/tls_cert_and_priv_key.pem`. Remove it after you have
   completed deploying restund with ansible.
4. Use Ansible to actually install using the restund playbook:

```bash
ansible-playbook -i hosts.ini restund.yml -vv
```

For information on setting up and using ansible-playbook to install Wire components, see {ref}`this page <ansible-vms>`.

### Private Subnets

By default, Restund is configured with a firewall that filters-out CIDR networks.

If you need to enable Restund to connect to a CIDR addressed host or network, you can specify a list of private subnets in [CIDR format](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing), which will override Restund's firewall's default settings of filtering-out CIDR networks.

You do this by setting the `restund_allowed_private_network_cidrs` option of the `[restund:vars]` section of the ansible inventory file ([for example this file](https://github.com/wireapp/wire-server-deploy/blob/master/ansible/inventory/prod/hosts.example.ini#L72)):

```ini
[restund:vars]
## Set the network interface name for restund to bind to if you have more than one network interface
## If unset, defaults to the ansible_default_ipv4 (if defined) otherwise to eth0
# restund_network_interface = eth0
restund_allowed_private_network_cidrs=192.168.0.1/32
```

This is needed, for example, to allow talking to the logging server if it is on a separate network:

The private subnets only need to override the RFC-defined private networks, which Wire firewalls off by default:

- 192.168.x.x
- 10.x.x.x
- 172.16.x.x - 172.31.x.x
- Etc...
