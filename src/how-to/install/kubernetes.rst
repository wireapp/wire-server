.. _ansible-kubernetes:

Installing kubernetes for a demo installation (on a single virtual machine)
============================================================================

.. include:: ansible-dependencies.rst

How to set up your hosts.ini file
-------------------------------------

Assuming a single virtual machine with a public IP address running Ubuntu 16.04 or 18.04, with at least 4 CPU cores and at least 8 GB of memory.

From ``wire-server-deploy/ansible``:

.. code:: shell

  cp hosts.example-demo.ini hosts.ini

Open hosts.ini and replace `X.X.X.X` with the IP address of your virtual machine that you use for ssh access.  You can try using ``sed -i 's/X.X.X.X/1.2.3.4/g' hosts.ini``.



.. include:: includes/ansible-authentication-blob.rst

How to install kubernetes
--------------------------

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

Next steps
-----------

See :ref:`helm`
