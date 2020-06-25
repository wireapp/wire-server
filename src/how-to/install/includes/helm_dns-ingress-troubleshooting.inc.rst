How to set up DNS records
----------------------------

An installation needs 5 or 6 domain names (5 without audio/video support, 6 with audio/video support):

You need

* two DNS names for the so-called "nginz" component of wire-server (the main REST API entry point), these are usually called `nginz-https.<domain>` and `nginz-ssl.<domain>`.
* one DNS name for the asset store (images, audio files etc. that your users are sharing); usually `assets.<domain>` or `s3.<domain>`.
* one DNS name for the webapp (equivalent of https://app.wire.com, i.e. the javascript app running in the browser), usually called `webapp.<domain>`.
* one DNS name for the account pages (hosts some html/javascript pages for e.g. password reset), usually called `account.<domain>`.
* (optional) one DNS name for team settings (to manage team membership if using PRO accounts), usually called `teams.<domain>`
* (optional) one DNS name for a audio/video calling server, usually called `restund01.<domain>`.

If you are on the most recent charts from wire-server-deploy, these are your names:

* nginz-https.<domain>
* nginz-ssl.<domain>
* webapp.<domain>
* assets.<domain>
* account.<domain>
* teams.<domain>

(Yes, they all need to point to the same IP address - this is necessary for the nginx ingress to know how to do internal routing based on virtual hosting.)

You may be happy with skipping the DNS setup and just make sure that the ``/etc/hosts`` on your client machine points all the above names to the right IP address:

::

   1.2.3.4 nginz-https.<domain> nginz-ssl.<domain> assets.<domain> webapp.<domain> teams.<domain> account.<domain>


How to direct traffic to your cluster
------------------------------------------

There are a few options available. The easiest option is to use an ingress with a node port, as this works everywhere and doesn't need a special setup.

.. code:: shell

   # (assuming you're in the root directory of wire-server-deploy)
   mkdir -p nginx-ingress-services && cd nginx-ingress-services
   cp ../values/nginx-ingress-services/demo-secrets.example.yaml secrets.yaml
   cp ../values/nginx-ingress-services/demo-values.example.yaml values.yaml

You should now have the following directory structure:

::

  .
  ├── nginx-ingress-services
  │   ├── secrets.yaml
  │   └── values.yaml
  └── my-wire-server
      ├── secrets.yaml
      └── values.yaml

Inside the ``nginx-ingress-services`` directory, open ``values.yaml`` and replace ``example.com`` with a domain of your choosing. You can try using ``sed -i 's/example.com/<your-domain>/g' values.yaml``.

Next, open ``secrets.yaml`` and add a TLS wildcard certificate and private key matching your domain. For ``example.com``, you need a certificate for ``*.example.com``. The easiest and cheapest option is `Let's Encrypt <https://letsencrypt.org/getting-started/>`__

.. note::

    `Let's Encrypt <https://letsencrypt.org/getting-started/>`__ & `cert-manager <https://cert-manager.io/docs/tutorials/acme/http-validation/>`__

    As an alternative to providing your own certificate, you may want to allow for automated certificate issuing through
    Let's Encrypt. For this, you have to install the *cert-manager* first:

    .. code:: shell

        helm upgrade --install -n cert-manager-ns --set 'installCRDs=true' cert-manager jetstack/cert-manager

    Afterwards, you have to make some minor adjustments to the ``nginx-ingress-services/values.yaml`` you have just copied
    and edited. Make sure the following properties are set accordingly:

    .. code:: yaml

        tls:
          enabled: true
          useCertManager: true

        certManager:
          # NOTE: You may set this to `true` when deploying the first time, just to make
          #       sure everything is order, and only to `false` before deploying again, so
          #       that a valid certificate is actually issued.
          inTestMode: false
          certmasterEmail: "ADD-VALID-ADDRESS-HERE"


    Please note, in this case, you can omit the ``secrets.yaml`` file entirely.


Install the nodeport nginx ingress:

.. code:: shell

   helm upgrade --install nginx-ingress-controller wire/nginx-ingress-controller --wait
   helm upgrade --install nginx-ingress-services wire/nginx-ingress-services -f values.yaml -f secrets.yaml --wait

Next, we want to redirect port 443 to the port the nginx https ingress nodeport is listening on (31773), and, redirect port 80 to the nginz http port (31772) (for redirects only). To do that, you have two options:

* Option 1: ssh into your kubernetes node, then execute:

  * ``iptables -t nat -A PREROUTING -p tcp --dport 443 -j REDIRECT --to-port 31773``
  * ``iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 31772``

* Option 2: Use ansible to do that, run the `iptables playbook <https://github.com/wireapp/wire-server-deploy/blob/master/ansible/iptables.yml>`__

Trying things out
---------------------------

At this point, with a bit of luck, everything should be working (if not, see :ref:`helm_prod_troubleshooting`)

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

.. _helm_prod_troubleshooting:

Troubleshooting
--------------------

Which version am I on?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are multiple artifacts which combine to form a running wire-server
deployment; these include:

-  docker images for each service
-  Kubernetes configs for each deployment (from helm charts)
-  configuration maps for each deployment (from helm charts)

If you wish to get some information regarding the code currently running
on your cluster you can run the following from ``wire-server-deploy`` (if you don't have wire-server-deploy, ``git clone https://github.com/wireapp/wire-server-deploy && cd wire-server-deploy`` first)::

   ./bin/deployment-info.sh <namespace> <deployment-name (e.g. brig)>

Example run:

::

   ./deployment-info.sh demo brig
   docker_image:               quay.io/wire/brig:2.50.319
   chart_version:              wire-server-0.24.9
   wire_server_commit:         8ec8b7ce2e5a184233aa9361efa86351c109c134
   wire_server_link:           https://github.com/wireapp/wire-server/releases/tag/image/2.50.319
   wire_server_deploy_commit:  01e0f261ca8163e63860f8b2af6d4ae329a32c14
   wire_server_deploy_link:    https://github.com/wireapp/wire-server-deploy/releases/tag/chart/wire-server-0.24.9

Note you'll need ``kubectl``, ``git`` and ``helm`` installed

It will output the running docker image; the corresponding wire-server
commit hash (and link) and the wire-server helm chart version which is
running. This will be helpful for any support requests.

Helm install / upgrade failed
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Usually, you want to run::

    kubectl get pods --all-namespaces

And look for any pods that are not ``Running``. Then you can::

    kubectl --namespace <namespace> logs <name-of-pod>

and/or::

    kubectl --namespace <namespace> describe <name-of-pod>

to know more.

As long as nobody is using your cluster yet, you can safely delete and re-create a specific Helm release (list releases with ``helm list --all``). Example delete the ``wire-server`` Helm release:

.. code:: shell

    helm delete --purge wire-server

It doesn't work, but my problem isn't listed here. Help!
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Feel free to open a github issue or pull request `here <https://github.com/wireapp/wire-docs>`_ and we'll try to improve the documentation.
