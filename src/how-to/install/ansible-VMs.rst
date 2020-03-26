.. _ansible_vms:

Installing kubernetes and databases on VMs with ansible
=======================================================

Introduction
------------

In a production environment, some parts of the wire-server
infrastructure (such as e.g. cassandra databases) are best configured
outside kubernetes. Additionally, kubernetes can be rapidly set up with
kubespray, via ansible. This section covers installing VMs with ansible.

Assumptions
-----------

- A bare-metal setup (no cloud provider)
- All machines run ubuntu 16.04 or ubuntu 18.04
- All machines have static IP addresses
- Time on all machines is being kept in sync
- You have the following virtual machines:

.. include:: includes/vm-table.rst

(It's up to you how you create these machines - kvm on a bare metal
machine, VM on a cloud provider, real physical machines, etc.)

Preparing to run ansible
------------------------

.. include:: ansible-dependencies.rst

.. _adding-ips-to-hostsini:

.. TODO: section header unifications/change

Adding IPs to hosts.ini
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Go to your checked-out wire-server-deploy/ansible folder::

   cd wire-server-deploy/ansible

Copy the example hosts file::

   cp hosts.example.ini hosts.ini

-  Edit the hosts.ini, setting the permanent IPs of the hosts you are
   setting up wire on.
-  replace the ``ansible_host`` values (``X.X.X.X``) with the IPs that
   you can reach by SSH. these are the 'internal' addresses of the
   machines, not what a client will be connecting to.
-  replace the ``ip`` values (``Y.Y.Y.Y``) with the IPs which you wish
   kubernetes to provide services to clients on.

There are more settings in this file that we will set in later steps.

.. TODO: remove this warning, and remove the hostname run from the cassandra playbook, or find another way to deal with it.

.. warning::

    Some of these playbooks mess with the hostnames of their targets. You
    MUST pick different hosts for playbooks that rename the host. If you
    e.g. attempt to run Cassandra and k8s on the same 3 machines, the
    hostnames will be overwritten by the second installation playbook,
    breaking the first.

    At the least, we know that the cassandra and kubernetes playbooks are
    both guilty of hostname manipulation.

Authentication
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: includes/ansible-authentication-blob.rst

Running ansible to install software on your machines
-----------------------------------------------------

You can install kubernetes, cassandra, restund, etc in any order.

.. note::

   In case you only have a single network interface with public IPs but wish to protect inter-database communication, you may use the ``tinc.yml`` playbook to create a private network interface. In this case, ensure tinc is setup BEFORE running any other playbook. See :ref:`tinc`

Installing kubernetes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Kubernetes is installed via ansible. To install kubernetes:

From ``wire-server-deploy/ansible``::

   poetry run ansible-playbook -i hosts.ini kubernetes.yml -vv

When the playbook finishes correctly (which can take up to 20 minutes), you should have a folder ``artifacts`` containing a file ``admin.conf``. Copy this file::

  mkdir -p ~/.kube
  cp artifacts/admin.conf ~/.kube/config

Make sure you can reach the server::

  kubectl version

should give output similar to this::

  Client Version: version.Info{Major:"1", Minor:"14", GitVersion:"v1.14.2", GitCommit:"66049e3b21efe110454d67df4fa62b08ea79a19b", GitTreeState:"clean", BuildDate:"2019-05-16T16:23:09Z", GoVersion:"go1.12.5", Compiler:"gc", Platform:"linux/amd64"}
  Server Version: version.Info{Major:"1", Minor:"14", GitVersion:"v1.14.2", GitCommit:"66049e3b21efe110454d67df4fa62b08ea79a19b", GitTreeState:"clean", BuildDate:"2019-05-16T16:14:56Z", GoVersion:"go1.12.5", Compiler:"gc", Platform:"linux/amd64"}

Cassandra
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

- Use poetry to run ansible, and deploy Cassandra:

::

   poetry run ansible-playbook -i hosts.ini cassandra.yml -vv

ElasticSearch
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  In your 'hosts.ini' file, in the ``[all:vars]`` section, set
   'elasticsearch_network_interface' to the name of the interface you
   want elasticsearch nodes to talk to each other on. For example:

.. code:: ini

   [all:vars]
   # default first interface on ubuntu on kvm:
   elasticsearch_network_interface=ens3

-  If you are performing an offline install, or for some other reason
   are using an APT mirror to retrieve elasticsearch-oss packages from,
   you need to specify that mirror. In the 'ELASTICSEARCH' section of
   hosts.ini, add two lines forcing elasticsearch to use a given APT
   mirror, with a given GPG key.

.. code:: ini

   es_apt_key = "https://<mymirror>/linux/ubuntu/gpg"
   es_apt_url = "deb [trusted=yes] https://<mymirror>/apt bionic stable"

-  Use poetry to run ansible, and deploy ElasticSearch:

::

   poetry run ansible-playbook -i hosts.ini elasticsearch.yml -vv

Minio
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

IMPORTANT checks
^^^^^^^^^^^^^^^^

 After running the above playbooks, it is important to ensure that everything is setup correctly. Please have a look at the post install checks in the section :ref:`checks`

::

   poetry run ansible-playbook -i hosts.ini cassandra-verify-ntp.yml -vv

Installing helm charts - prerequisites
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``helm_external.yml`` playbook can be used locally to write or update the IPs of the
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

Next steps for high-available production installation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Your next step will be :ref:`helm_prod`
