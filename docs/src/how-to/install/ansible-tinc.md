(tinc)=

# tinc

Installing [tinc mesh vpn](http://tinc-vpn.org/) is *optional and
experimental*. It allows having a private network interface `vpn0` on
the target VMs.

```{warning}
We currently only use tinc for test clusters and have not made sure if the default settings it comes with provide adequate security to protect your data. If using tinc and the following tinc.yml playbook, make your own checks first!
```

```{note}
Ensure to run the tinc.yml playbook first if you use tinc, before
other playbooks.
```

From `wire-server-deploy/ansible`, where you created a `hosts.ini` file.

- Add a `vpn_ip=Z.Z.Z.Z` item to each entry in the hosts file with a
  (fresh) IP range if you wish to use tinc.
- Add a group `vpn`:

```ini
# this is a minimal example
[all]
server1 ansible_host=X.X.X.X vpn_ip=10.10.1.XXX
server2 ansible_host=X.X.X.X vpn_ip=10.10.1.YYY

[cassandra]
server1
server2

[vpn:children]
cassandra
# add other server groups here as necessary
```

Also ensure subsequent playbooks make use of the newly-created interface by setting:

```ini
[all:vars]
minio_network_interface = vpn0
cassandra_network_interface = vpn0
elasticsearch_network_interface = vpn0
redis_network_interface = vpn0
```

Configure the physical network interface inside tinc.yml if it is not
`eth0`. Then:

```
ansible-playbook -i hosts.ini tinc.yml -vv
```
