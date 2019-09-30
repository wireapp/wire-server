.. _helm:

Installing wire-server (demo) components using helm
======================================================

Introduction
-----------------

The following will install a demo version of all the wire-server components including the databases. This setup is not recommended in production but will get you started.

Demo version means

* easy setup - only one single machine with kubernetes is needed (make sure you have at least 4 CPU cores and 8 GB of memory available)
* no data persistence (everything stored in memory, will be lost)

Prerequisites
--------------------------------

You need to have access to a kubernetes cluster, and the ``helm`` local binary on your PATH.

* If you don't have a kubernetes cluster, you have two options:

  * You can get access to a managed kubernetes cluster with the cloud provider of your choice.
  * You can install one if you have ssh access to a virtual machine, see :ref:`ansible-kubernetes`

* If you don't have ``helm`` yet, see `Installing helm <https://helm.sh/docs/using_helm/#installing-helm>`__.

Type ``helm version``, you should, if everything is configured correctly, see a result like this:

::

    Client: &version.Version{SemVer:"v2.13.1", GitCommit:"618447cbf203d147601b4b9bd7f8c37a5d39fbb4", GitTreeState:"clean"}
    Server: &version.Version{SemVer:"v2.13.1", GitCommit:"618447cbf203d147601b4b9bd7f8c37a5d39fbb4", GitTreeState:"clean"}


In case ``kubectl version`` shows both Client and Server versions, but ``helm version`` does not show a Server version, you may need to run ``helm init``. The exact version (assuming `v2.X.X` - at the time of writing v3 is not yet supported) matters less as long as both Client and Server versions match (or are very close).

How to start installing charts from wire
--------------------------------------------------

Enable the wire charts helm repository:

.. code:: shell

   helm repo add wire https://s3-eu-west-1.amazonaws.com/public.wire.com/charts

(You can see available helm charts by running ``helm search wire/``. To see
new versions as time passes, you may need to run ``helm repo update``)

Great! Now you can start installing.

Note: all commands below can also take an extra ``--namespace <your-namespace>`` if you don't want to install into the default kubernetes namespace.

Watching changes as they happen
--------------------------------------------------

Open a terminal and run

.. code:: shell

    kubectl get pods -w

This will block your terminal and show some things happening as you proceed through this guide. Keep this terminal open and open a second terminal.

How to install in-memory databases and external components
--------------------------------------------------------------

In your second terminal, first install databases:

.. code:: shell

   helm upgrade --install databases-ephemeral wire/databases-ephemeral --wait

You should see some pods being created in your first terminal as the above command completes. When you're done, ins

Next, install aws service mocks:

.. code:: shell

   helm upgrade --install fake-aws wire/fake-aws --wait

Next, install an smtp server:

.. code:: shell

   helm upgrade --install smtp wire/demo-smtp --wait

How to install wire-server itself
---------------------------------------

Download example demo values and secrets:

.. code:: shell

   mkdir -p wire-server && cd wire-server
   curl -sSL https://raw.githubusercontent.com/wireapp/wire-server-deploy/master/values/wire-server/demo-secrets.example.yaml > secrets.yaml
   curl -sSL https://raw.githubusercontent.com/wireapp/wire-server-deploy/master/values/wire-server/demo-values.example.yaml > values.yaml

Open ``values.yaml`` and replace ``example.com`` and other domains and subdomains with domains of your choosing. Look for the ``# change this`` comments. You can try using ``sed -i 's/example.com/<your-domain>/g' values.yaml``.

Generate some secrets:

.. code:: shell

  openssl rand -base64 64 | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 42 > restund.txt
  docker run --rm quay.io/wire/alpine-intermediate /dist/zauth -m gen-keypair -i 1 > zauth.txt

1. Add the generated secret from restund.txt to secrets.yaml under ``brig.secrets.turn.secret``
2. add **both** the public and private parts from zauth.txt to secrets.yaml under ``brig.zauth``
3. Add the public key from zauth.txt **also** to secrets.yaml under ``nginz.secrets.zAuth.publicKeys``

Great, now try the installation:

.. code:: shell

   helm upgrade --install wire-server wire/wire-server -f values.yaml -f secrets.yaml --wait


How to direct traffic to your cluster
------------------------------------------

There are a few options available. The easiest option is to use an ingress with a node port, as this works everywhere and doesn't need a special setup.

.. code:: shell

   # (assuming you're in the wire-server directory from the subsection above)
   cd ..
   mkdir -p nginx-lb-ingress && cd nginx-lb-ingress
   curl -sSL https://raw.githubusercontent.com/wireapp/wire-server-deploy/master/values/nginx-lb-ingress/demo-secrets.example.yaml > secrets.yaml
   curl -sSL https://raw.githubusercontent.com/wireapp/wire-server-deploy/master/values/nginx-lb-ingress/demo-values.example.yaml > values.yaml

You should now have the following directory structure:

::

  .
  ├── nginx-lb-ingress
  │   ├── secrets.yaml
  │   └── values.yaml
  └── wire-server
      ├── secrets.yaml
      └── values.yaml

Inside the ``nginx-lb-ingress`` directory, open ``values.yaml`` and replace ``example.com`` with a domain of your choosing. You can try using ``sed -i 's/example.com/<your-domain>/g' values.yaml``.

Next, open ``secrets.yaml`` and add a TLS wildcard certificate and private key matching your domain. For ``example.com``, you need a certficate for ``*.example.com``. The easiest and cheapest option is to use `Let's Encrypt <https://letsencrypt.org/getting-started/>`__

Install the nodeport nginx ingress:

.. code:: shell

   helm upgrade --install nginx-lb-ingress wire/nginx-lb-ingress -f values.yaml -f secrets.yaml --wait

Next, we want to redirect port 443 for https to the port the nginx https ingress nodeport is listening on (31773), and port 80 to the nginz http port (31772). To do that, you have two options:

* Option 1: ssh into your kubernetes node, then execute: ``iptables -t nat -A PREROUTING -p tcp --dport 443 -j REDIRECT --to-port 31773``
* Option 2: Use ansible to do that, run the `iptables playbook <https://github.com/wireapp/wire-server-deploy/blob/master/ansible/iptables.yml>`__

How to set up DNS records
----------------------------

You need to set up DNS records (A records) such that the following domain names are pointing to the IP address of your kubernetes node (assuming you only have one):

* nginz-https.<domain>
* nginz-ssl.<domain>
* webapp.<domain>
* s3.<domain>
* team.<domain>
* account.<domain>

(Yes, they all need to point to the same IP address - this is necessary for the nginx ingress to know how to do internal routing based on virtual hosting)


Trying things out
---------------------------

At this point, with a bit of luck, everything should be working (if not, see :ref:`helm_troubleshooting`)

Can you reach the nginz server?

::

    curl -i https://nginz-https.<domain>/status

You should get a 200 return code

::

    HTTP/1.1 200 OK
    Content-Type: text/plain
    Date: ...
    Server: nginx
    Content-Length: 0

Can you access the webapp? Open https://webapp.<your-domain> in your browser (Firefox/Chrome/Safari only)

.. _helm_troubleshooting:

Troubleshooting
--------------------

Helm install / upgrade failed
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As long as nobody is using your cluster yet, you can safely delete and re-create a specific helm release (list releases with ``helm list --all``). Example delete the ``wire-server`` helm release:

.. code:: shell

    helm delete --purge wire-server

It doesn't work, but my problem isn't listed here. Help!
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Feel free to open a github issue or pull request `here <https://github.com/wireapp/wire-docs>` and we'll try to improve the documentation.
