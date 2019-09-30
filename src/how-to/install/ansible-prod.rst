ansible-based configuration
===========================

In a production environment, some parts of the wire-server
infrastructure (such as e.g. cassandra databases) are best configured
outside kubernetes. Additionally, kubernetes can be rapidly set up with
kubespray, via ansible. The documentation and code under this folder is
meant to help with that.

Status
------

work-in-progress

-  [ ] document networking setup
-  [ ] diagram
-  [ ] other assumptions?
-  [x] install kubernetes with kubespray
-  [x] install cassandra
-  [x] install elasticsearch
-  [x] install minio
-  [ ] install redis
-  [x] install restund servers
-  [ ] polish

Assumptions
-----------

This document assumes

-  a bare-metal setup (no cloud provider)
-  a production SLA where 30 minutes of downtime is unacceptable
-  about 1000 active users
-  all machines run ubuntu 16.04 or ubuntu 18.04

Dependencies
------------

Poetry
~~~~~~

First, we're going to install `Poetry <https://poetry.eustace.io/>`__.
We'll be using it to run ansible playbooks later. These directions
assume you're using python 2.7 (if you only have python3 available, you
may need to find some workarounds):

To install poetry:

::

   sudo apt install -y python2.7 python-pip
   curl -sSL https://raw.githubusercontent.com/sdispater/poetry/master/get-poetry.py > get-poetry.py
   python2.7 get-poetry.py
   source $HOME/.poetry/env
   ln -s /usr/bin/python2.7 $HOME/.poetry/bin/python

During the installation, answer 'Y' to allow the Path variable for this
user to be modified.

Ansible
~~~~~~~

-  Install the python dependencies to run ansible.

::

   git clone https://github.com/wireapp/wire-server-deploy.git
   cd wire-server-deploy/ansible
   ## (optional) if you need ca certificates other than the default ones:
   # export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
   poetry install

Note: the 'make download-cli-binaries' part of 'make download' requires
either that you have run this all as root, or that the user you are
running these scripts can 'sudo' without being prompted for a password.
I run 'sudo ls', get prompted for a password, THEN run 'make download'.

-  Download the ansible roles necessary to install databases and
   kubernetes:

::

   make download

Provisioning machines
---------------------

Create the following:

+---------------+--------+-----+--------+--------+
| Name          | Amount | CPU | memory | disk   |
+===============+========+=====+========+========+
| cassandra     | 3      | 2   | 4 GB   | 80 GB  |
+---------------+--------+-----+--------+--------+
| minio         | 3      | 1   | 2 GB   | 100 GB |
+---------------+--------+-----+--------+--------+
| elasticsearch | 3      | 1   | 2 GB   | 10 GB  |
+---------------+--------+-----+--------+--------+
| redis         | 3      | 1   | 2 GB   | 10 GB  |
+---------------+--------+-----+--------+--------+
| kubernetes    | 3      | 4   | 8 GB   | 20 GB  |
+---------------+--------+-----+--------+--------+
| turn          | 2      | 1   | 2 GB   | 10 GB  |
+---------------+--------+-----+--------+--------+

It's up to you how you create these machines - kvm on a bare metal
machine, VM on a cloud provider, a real physical machine, etc. Make sure
they run ubuntu 16.04/18.04.

Ensure that the machines have IP addresses that do not change.

Preparing to run ansible
------------------------

.. _adding-ips-to-hostsini:

Adding IPs to hosts.ini
~~~~~~~~~~~~~~~~~~~~~~~

Copy the example hosts file:

``cp hosts.example.ini hosts.ini``

-  Edit the hosts.ini, setting the permanent IPs of the hosts you are
   setting up wire on.
-  replace the ``ansible_host`` values (``X.X.X.X``) with the IPs that
   you can reach by SSH. these are the 'internal' addresses of the
   machines, not what a client will be connecting to.
-  replace the ``ip`` values (``Y.Y.Y.Y``) with the IPs which you wish
   kubernetes to provide services to clients on.

There are more settings in this file that we will set in later steps.

WARNING: host re-use
^^^^^^^^^^^^^^^^^^^^

Some of these playbooks mess with the hostnames of their targets. You
MUST pick different hosts for playbooks that rename the host. If you
e.g. attempt to run Cassandra and k8s on the same 3 machines, the
hostnames will be overwritten by the second installation playbook,
breaking the first.

At the least, we know that the cassandra and kubernetes playbooks are
both guilty of hostname manipulation.

Authentication
^^^^^^^^^^^^^^

Password authentication
'''''''''''''''''''''''

-  if you want to use passwords both for ansible authenticating to a
   machine, and for ansible to gain root priveledges:

::

   sudo apt install sshpass

-  in hosts.ini, uncomment the 'ansible_user = ...' line, and change
   '...' to the user you want to login as.
-  in hosts.ini, uncomment the 'ansible_ssh_pass = ...' line, and change
   '...' to the password for the user you are logging in as.
-  in hosts.ini, uncomment the 'ansible_become_pass = ...' line, and
   change the ... to the password you'd enter to sudo.

Configuring SSH keys
''''''''''''''''''''

(from https://linoxide.com/how-tos/ssh-login-with-public-key/) If you
want a bit higher security, you can copy SSH keys between the machine
you are administrating with, and the machines you are managing with
ansible.

-  Create an SSH key.

::

   ssh-keygen -t rsa

-  Install your SSH key on each of the machines you are managing with
   ansible, so that you can SSH into them without a password:

::

   ssh-copy-id -i ~/.ssh/id_rsa.pub $USERNAME@$IP

Replace ``$USERNAME`` with the username of the account you set up when
you installed the machine.

Sudo without password
'''''''''''''''''''''

Ansible can be configured to use a password for switching from the
unpriviledged $USERNAME to the root user. This involves having the
password lying about, so has security problems. If you want ansible to
not be prompted for any administrative command (a different security
problem!):

-  As root on each of the nodes, add the following line at the end of
   the /etc/sudoers file:

::

   <ANSIBLE_LOGIN_USERNAME>     ALL=(ALL) NOPASSWD:ALL

Replace ``<ANSIBLE_LOGIN_USERNAME>`` with the username of the account
you set up when you installed the machine.

Ansible pre-kubernetes
^^^^^^^^^^^^^^^^^^^^^^

Now that you have a working hosts.ini, and you can access the host, run
any ansible scripts you need, in order for the nodes to have internet
(proxy config, ssl certificates, etc).

Installing kubernetes
~~~~~~~~~~~~~~~~~~~~~

Kubernetes is installed via ansible.

-  To deploy kubernetes:

::

   poetry run ansible-playbook -i hosts.ini kubernetes.yml -vv

Cassandra
~~~~~~~~~

-  Set variables in the hosts.ini file under ``[cassandra:vars]``. Most
   defaults should be fine, except maybe for the cluster name and the
   network interface to use:

.. code:: ini

   [cassandra:vars]
   ## set to True if using AWS
   is_aws_environment = False
   # cassandra_clustername: default

   [all:vars]
   ## Set the network interface name for cassandra to bind to if you have more than one network interface
   # cassandra_network_interface = eth0

(see
`defaults/main.yml <https://github.com/wireapp/ansible-cassandra/blob/master/defaults/main.yml>`__
for a full list of variables to change if necessary)

Install cassandra:

::

   poetry run ansible-playbook -i hosts.ini cassandra.yml -vv

ElasticSearch
~~~~~~~~~~~~~

-  In your 'hosts.ini' file, in the ``[elasticsearch:vars]`` section,
   set 'elasticsearch_network_interface' to the name of the interface
   you want elasticsearch nodes to talk to each other on. For example:

.. code:: ini

   [all:vars]
   # default first interface on ubuntu on kvm:
   elasticsearch_network_interface=ens3

-  Use poetry to run ansible, and deploy ElasticSearch:

::

   poetry run ansible-playbook -i hosts.ini elasticsearch.yml -vv

Minio
~~~~~

-  In your 'hosts.ini' file, in the ``[all:vars]`` section, make sure
   you set the 'minio_network_interface' to the name of the interface
   you want minio nodes to talk to each other on. The default from the
   playbook is not going to be correct for your machine. For example:

.. code:: ini

   [all:vars]
   # Default first interface on ubuntu on kvm:
   minio_network_interface=ens3

-  In your 'hosts.ini' file, in the ``[minio:vars]`` section, ensure you
   set minio_access_key and minio_secret key.

-  Use poetry to run ansible, and deploy Minio:

::

   poetry run ansible-playbook -i hosts.ini minio.yml -vv

Restund
~~~~~~~

Set other variables in the hosts.ini file under ``[restund:vars]``. Most
defaults should be fine, except for the network interfaces to use:

-  set ``ansible_host=X.X.X.X`` under the ``[all]`` section to the IP
   for SSH access.
-  (recommended) set ``restund_network_interface =`` under the
   ``[restund:vars]`` section to the interface name you wish the process
   to use. Defaults to the default_ipv4_address, with a fallback to
   ``eth0``.
-  (optional) ``restund_peer_udp_advertise_addr=Y.Y.Y.Y``: set this to
   the IP to advertise for other restund servers if different than the
   ip on the 'restund_network_interface'. If using
   'restund_peer_udp_advertise_addr', make sure that UDP (!) traffic
   from any restund server (including itself) can reach that IP (for
   ``restund <-> restund`` communication). This should only be necessary
   if you're installing restund on a VM that is reachable on a public IP
   address but the process cannot bind to that public IP address
   directly (e.g. on AWS VPC VM). If unset, ``restund <-> restund`` UDP
   traffic will default to the IP in the ``restund_network_interface``.

.. code:: ini

   [all]
   (...)
   restund01         ansible_host=X.X.X.X

   (...)

   [all:vars]
   ## Set the network interface name for restund to bind to if you have more than one network interface
   ## If unset, defaults to the ansible_default_ipv4 (if defined) otherwise to eth0
   restund_network_interface = eth0

(see
`defaults/main.yml <https://github.com/wireapp/ansible-restund/blob/master/defaults/main.yml>`__
for a full list of variables to change if necessary)

Install restund:

::

   poetry run ansible-playbook -i hosts.ini restund.yml -vv

Installing helm charts - prerequisites
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``helm_external.yml`` playbook can be used to write the IPs of the
databases into the ``values/cassandra-external/values.yaml`` file, and
thus make them available for helm and the ``...-external`` charts (e.g.
``cassandra-external``).

Ensure to define the following in your hosts.ini under ``[all:vars]``:

.. code:: ini

   [all:vars]
   minio_network_interface = ...
   cassandra_network_interface = ...
   elasticsearch_network_interface = ...
   redis_network_interface = ...

::

   poetry run ansible-playbook -i hosts.ini -vv --diff helm_external.yml

Now you can install the helm charts.

tinc
~~~~

Installing `tinc mesh vpn <http://tinc-vpn.org/>`__ is **optional and
experimental**. It allows having a private network interface ``vpn0`` on
the target VMs.

*Note: Ensure to run the tinc.yml playbook first if you use tinc, before
other playbooks.*

-  Add a ``vpn_ip=Z.Z.Z.Z`` item to each entry in the hosts file with a
   (fresh) IP range if you wish to use tinc.
-  Add a group ``vpn``:

.. code:: ini

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

Configure the physical network interface inside tinc.yml if it is not
``eth0``. Then:

::

   poetry run ansible-playbook -i hosts.ini tinc.yml -vv

