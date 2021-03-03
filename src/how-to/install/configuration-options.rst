.. _configuration_options:

Part 3 - configuration options in a production setup
====================================================================

This contains instructions to configure specific aspects of your production setup depending on your needs.

Depending on your use-case and requirements, you may need to
configure none, or only a subset of the following sections.

Redirect some traffic through a http(s) proxy
---------------------------------------------

In case you wish to use http(s) proxies, you can add a configuration like this to the wire-server services in question:

Assuming your proxy can be reached from within Kubernetes at ``http://proxy:8080``, add the following for each affected service (e.g. ``gundeck``) to your Helm overrides in ``values/wire-server/values.yaml`` :

.. code:: yaml

    gundeck:
      # ...
      config:
        # ...
        proxy:
          httpProxy: "http://proxy:8080"
          httpsProxy: "http://proxy:8080"
          noProxyList:
            - "localhost"
            - "127.0.0.1"
            - "10.0.0.0/8"
            - "elasticsearch-external"
            - "cassandra-external"
            - "redis-ephemeral"
            - "fake-aws-sqs"
            - "fake-aws-dynamodb"
            - "fake-aws-sns"
            - "brig"
            - "cargohold"
            - "galley"
            - "gundeck"
            - "proxy"
            - "spar"
            - "federator"
            - "cannon"
            - "cannon-0.cannon.default"
            - "cannon-1.cannon.default"
            - "cannon-2.cannon.default"

Depending on your setup, you may need to repeat this for the other services like ``brig`` as well.


Enable push notifications using the public appstore / playstore mobile Wire clients
-----------------------------------------------------------------------------------

1. You need to get in touch with us. Please talk to sales or customer support - see https://wire.com
2. If a contract agreement has been reached, we can set up a separate AWS account for you containing the necessary AWS SQS/SNS setup to route push notifications through to the mobile apps. We will then forward some configuration / access credentials that looks like:

.. code::

    push_notification_settings = {
      "aws_account_id" = "REDACTED"
      "gundeck_access_key" = "REDACTED"
      "gundeck_access_secret" = "REDACTED"
      "notification_queue_name" = "<environment>-gundeck-events"
      "sns_endpoint" = "https://sns.<region>.amazonaws.com"
      "sqs_endpoint" = "https://sqs.<region>.amazonaws.com"
    }

To make use of those, first test the credentials are correct, e.g. using the ``aws`` command-line tool (for more information on how to configure credentials, please refer to the `official docs <https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-quickstart.html#cli-configure-quickstart-precedence>`__):

.. code::

    AWS_REGION=<region>
    AWS_ACCESS_KEY_ID=<...>
    AWS_SECRET_ACCESS_KEY=<...>
    ENV=<environment> #e.g staging

    aws sqs get-queue-url --queue-name "$ENV-gundeck-events"

You should get a result like this:

.. code::

    {
        "QueueUrl": "https://<region>.queue.amazonaws.com/<aws-account-id>/<environment>-gundeck-events"
    }

Then add them to your gundeck configuration overrides:

.. code:: yaml

   # in values/wire-server/values.yaml

    gundeck:
      # ...
      config:
        aws:
          queueName: # e.g. staging-gundeck-events
          account: # <aws-account-id>, e.g. 123456789
          region: # e.g. eu-central-1
          snsEndpoint: # e.g. https://sns.eu-central-1.amazonaws.com
          sqsEndpoint: # e.g. https://sqs.eu-central-1.amazonaws.com
          arnEnv: # e.g. staging - this must match the environment name (first part of queueName)

.. code:: yaml

   # in values/wire-server/secrets.yaml

    gundeck:
      # ...
      secrets:
        awsKeyId: CHANGE-ME
        awsSecretKey: CHANGE-ME


After making this change and applying it to gundeck (ensure gundeck pods have restarted to make use of the updated configuration - that should happen automatically), make sure to reset the push token on any mobile devices that you may have in use.


Blocking creation of personal users, new teams
--------------------------------------------------------------------------

There are some unauthenticated end-points that allow arbitrary users on the open internet to do things like create a new team.  This is desired in the cloud, and not an issue on many on-prem solutions (eg. all of those that are not exposed to the global IP address space).  However, if you run an on-prem setup that is open to the world, you may want to block this.

Brig has a server option for this:

.. code:: yaml

    optSettings:
      setRestrictUserCreation: true

If `setRestrictUserCreation` is `true`, creating new personal users or new teams on your instance from outside your backend installation is impossible.  (If you want to be more technical: requests to `/register` that create a new personal account or a new team are answered with `403 forbidden`.)

If you operate an instance with restricted user creation, you can still create new teams (and, if you really want to, personal users): see https://github.com/wireapp/wire-server/blob/b9a84f9b654a69c9a296761b36c042dc993236d3/deploy/services-demo/create_test_team_admins.sh for examples.


You may want
--------------

-  more server resources to ensure
   `high-availability <#persistence-and-high-availability>`__
-  an email/SMTP server to send out registration emails
-  depending on your required functionality, you may or may not need an
   `AWS account <https://aws.amazon.com/>`__. See details about
   limitations without an AWS account in the following sections.
-  one or more people able to maintain the installation
-  official support by Wire (`contact us <https://wire.com/pricing/>`__)

.. warning::

   As of 2020-08-10, the documentation sections below are partially out of date and need to be updated.

Metrics/logging
---------------

* :ref:`monitoring`
* :ref:`logging`

SMTP server
-----------

**Assumptions**: none

**Provides**:

-  full control over email sending

**You need**:

-  SMTP credentials (to allow for email sending; prerequisite for
   registering users and running the smoketest)

**How to configure**:

-  *if using a gmail account, ensure to enable* `'less secure
   apps' <https://support.google.com/accounts/answer/6010255?hl=en>`__
-  Add user, SMTP server, connection type to ``values/wire-server``'s
   values file under ``brig.config.smtp``
-  Add password in ``secrets/wire-server``'s secrets file under
   ``brig.secrets.smtpPassword``

Load balancer on bare metal servers
-----------------------------------

**Assumptions**:

-  You installed kubernetes on bare metal servers or virtual machines
   that can bind to a public IP address.
-  **If you are using AWS or another cloud provider, see**\ `Creating a
   cloudprovider-based load
   balancer <#load-balancer-on-cloud-provider>`__\ **instead**

**Provides**:

-  Allows using a provided Load balancer for incoming traffic
-  SSL termination is done on the ingress controller
-  You can access your wire-server backend with given DNS names, over
   SSL and from anywhere in the internet

**You need**:

-  A kubernetes node with a *public* IP address (or internal, if you do
   not plan to expose the Wire backend over the Internet but we will
   assume you are using a public IP address)
-  DNS records for the different exposed addresses (the ingress depends
   on the usage of virtual hosts), namely:

   -  ``nginz-https.<domain>``
   -  ``nginz-ssl.<domain>``
   -  ``assets.<domain>``
   -  ``webapp.<domain>``
   -  ``account.<domain>``
   -  ``teams.<domain>``

-  A wildcard certificate for the different hosts (``*.<domain>``) - we
   assume you want to do SSL termination on the ingress controller

**Caveats**:

-  Note that there can be only a *single* load balancer, otherwise your
   cluster might become
   `unstable <https://metallb.universe.tf/installation/>`__

**How to configure**:

::

   cp values/metallb/demo-values.example.yaml values/metallb/demo-values.yaml
   cp values/nginx-ingress-services/demo-values.example.yaml values/nginx-ingress-services/demo-values.yaml
   cp values/nginx-ingress-services/demo-secrets.example.yaml values/nginx-ingress-services/demo-secrets.yaml

-  Adapt ``values/metallb/demo-values.yaml`` to provide a list of public
   IP address CIDRs that your kubernetes nodes can bind to.
-  Adapt ``values/nginx-ingress-services/demo-values.yaml`` with correct URLs
-  Put your TLS cert and key into
   ``values/nginx-ingress-services/demo-secrets.yaml``.

Install ``metallb`` (for more information see the
`docs <https://metallb.universe.tf>`__):

.. code:: sh

   helm upgrade --install --namespace metallb-system metallb wire/metallb \
       -f values/metallb/demo-values.yaml \
       --wait --timeout 1800

Install ``nginx-ingress-[controller,services]``:

::
   helm upgrade --install --namespace demo demo-nginx-ingress-controller wire/nginx-ingress-controller \
       --wait

   helm upgrade --install --namespace demo demo-nginx-ingress-services wire/nginx-ingress-services \
       -f values/nginx-ingress-services/demo-values.yaml \
       -f values/nginx-ingress-services/demo-secrets.yaml \
       --wait

Now, create DNS records for the URLs configured above.


Load Balancer on cloud-provider
-------------------------------

AWS
~~~

`Upload the required
certificates <https://aws.amazon.com/premiumsupport/knowledge-center/import-ssl-certificate-to-iam/>`__.
Create and configure ``values/aws-ingress/demo-values.yaml`` from the
examples.

::

   helm upgrade --install --namespace demo demo-aws-ingress wire/aws-ingress \
       -f values/aws-ingress/demo-values.yaml \
       --wait

To give your load balancers public DNS names, create and edit
``values/external-dns/demo-values.yaml``, then run
`external-dns <https://github.com/helm/charts/tree/master/stable/external-dns>`__:

::

   helm repo update
   helm upgrade --install --namespace demo demo-external-dns stable/external-dns \
       --version 1.7.3 \
       -f values/external-dns/demo-values.yaml \
       --wait

Things to note about external-dns:

-  There can only be a single external-dns chart installed (one per
   kubernetes cluster, not one per namespace). So if you already have
   one running for another namespace you probably don't need to do
   anything.
-  You have to add the appropriate IAM permissions to your cluster (see
   the
   `README <https://github.com/helm/charts/tree/master/stable/external-dns>`__).
-  Alternatively, use the AWS route53 console.

Other cloud providers
~~~~~~~~~~~~~~~~~~~~~

This information is not yet available. If you'd like to contribute by
adding this information for your cloud provider, feel free to read the
`contributing guidelines <../CONTRIBUTING.md>`__ and open a PR.

Real AWS services
-----------------

**Assumptions**:

-  You installed kubernetes and wire-server on AWS

**Provides**:

-  Better availability guarantees and possibly better functionality of
   AWS services such as SQS and dynamoDB.
-  You can use ELBs in front of nginz for higher availability.
-  instead of using a smtp server and connect with SMTP, you may use
   SES. See configuration of brig and the ``useSES`` toggle.

**You need**:

-  An AWS account

**How to configure**:

-  Instead of using fake-aws charts, you need to set up the respective
   services in your account, create queues, tables etc. Have a look at
   the fake-aws-\* charts; you'll need to replicate a similar setup.

   -  Once real AWS resources are created, adapt the configuration in
      the values and secrets files for wire-server to use real endpoints
      and real AWS keys. Look for comments including
      ``if using real AWS``.

-  Creating AWS resources in a way that is easy to create and delete
   could be done using either `terraform <https://www.terraform.io/>`__
   or `pulumi <https://pulumi.io/>`__. If you'd like to contribute by
   creating such automation, feel free to read the `contributing
   guidelines <../CONTRIBUTING.md>`__ and open a PR.

Persistence and high-availability
---------------------------------

Currently, due to the way kubernetes and cassandra
`interact <https://github.com/kubernetes/kubernetes/issues/28969>`__,
cassandra cannot reliably be installed on kubernetes. Some people have
tried, e.g. `this
project <https://github.com/instaclustr/cassandra-operator>`__ though at
the time of writing (Nov 2018), this does not yet work as advertised. We
recommend therefore to install cassandra, (possibly also elasticsearch
and redis) separately, i.e. outside of kubernetes (using 3 nodes each).

For further higher-availability:

-  scale your kubernetes cluster to have separate etcd and master nodes
   (3 nodes each)
-  use 3 instead of 1 replica of each wire-server chart

Security
--------

For a production deployment, you should, as a minimum:

-  Ensure traffic between kubernetes nodes, etcd and databases are
   confined to a private network
-  Ensure kubernetes API is unreachable from the public internet (e.g.
   put behind VPN/bastion host or restrict IP range) to prevent
   `kubernetes
   vulnerabilities <https://www.cvedetails.com/vulnerability-list/vendor_id-15867/product_id-34016/Kubernetes-Kubernetes.html>`__
   from affecting you
-  Ensure your operating systems get security updates automatically
-  Restrict ssh access / harden sshd configuration
-  Ensure no other pods with public access than the main ingress are
   deployed on your cluster, since, in the current setup, pods have
   access to etcd values (and thus any secrets stored there, including
   secrets from other pods)
-  Ensure developers encrypt any secrets.yaml files

Additionally, you may wish to build, sign, and host your own docker
images to have increased confidence in those images. We haved "signed
container images" on our roadmap.

Sign up with a phone number (Sending SMS)
-----------------------------------------

**Provides**:

-  Registering accounts with a phone number

**You need**:

-  a `Nexmo <https://www.nexmo.com/>`__ account
-  a `Twilio <https://www.twilio.com/>`__ account

**How to configure**:

See the ``brig`` chart for configuration.

.. _3rd-party-proxying:

3rd-party proxying
------------------

You need Giphy/Google/Spotify/Soundcloud API keys (if you want to
support previews by proxying these services)

See the ``proxy`` chart for configuration.
