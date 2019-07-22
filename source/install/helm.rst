.. _helm:

Installing wire-server components using helm
======================================================

Prerequisites
--------------------------------

Type `helm version`, to ensure you're connected to a kubernetes cluster.

  * If this fails, see :ref:`deps` and :ref:`ansible-kubernetes`

Enable the wire charts helm repository:

.. code:: shell

   helm repo add wire https://s3-eu-west-1.amazonaws.com/public.wire.com/charts

(You can see available helm charts by running ``helm search wire/``. To see
new versions as time passes, you may need to run ``helm repo update``)


How to install a demo version of wire-server on an existing kubernetes cluster
-----------------------------------------------------------------------------------------

Demo version means

* easiest setup - only one single machine with kubernetes is needed
* no data persistence (everything stored in memory, will be lost)

Note: all commands below can also take an extra ``--namespace <your-namespace>`` if you don't want to install into the default kubernetes namespace.

Watching changes as they happen
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Open a terminal and run

.. code:: shell

    kubectl get pods -w

This will block your terminal and show some things happening as you proceed through this guide. Keep this terminal open and open a second terminal.

How to install in-memory databases and external components
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

TODO: use an ingress (easiest), explain other options

How to set up DNS records
^^^^^^^^^^^^^^^^^^^^^^^^^^^

TODO: generic instructions, list of URLs, move to other page:

* nginz-https.<domain>
* nginz-ssl.<domain>
* app.<domain>
* team.<domain>
* account.<domain>


* turn01.<domain>


Troubleshooting
--------------------

Helm install / upgrade failed
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As long as nobody is using your cluster yet, you can safely delete and re-create a specific helm release (list releases with ``helm list --all``). Example delete the ``wire-server`` helm release:

.. code:: shell

    helm delete --purge wire-server
