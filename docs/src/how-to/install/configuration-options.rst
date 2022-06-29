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

.. code:: yaml

    "gundeck":
      "config":
        "aws":
          "account": "<REDACTED>"
          "arnEnv": "<REDACTED>"
          "queueName": "<REDACTED>-gundeck-events"
          "region": "<REDACTED>"
          "snsEndpoint": "https://sns.<REDACTED>.amazonaws.com"
          "sqsEndpoint": "https://sqs.<REDACTED>.amazonaws.com"
      "secrets":
        "awsKeyId": "<REDACTED>"
        "awsSecretKey": "<REDACTED>"

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

Then add them to your gundeck configuration overrides.

Keys below ``gundeck.config`` belong into ``values/wire-server/values.yaml``:

.. code:: yaml

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

Keys below ``gundeck.secrets`` belong into ``values/wire-server/secrets.yaml``:

.. code:: yaml

    gundeck:
      # ...
      secrets:
        awsKeyId: CHANGE-ME
        awsSecretKey: CHANGE-ME


After making this change and applying it to gundeck (ensure gundeck pods have restarted to make use of the updated configuration - that should happen automatically), make sure to reset the push token on any mobile devices that you may have in use.

Controlling the speed of websocket draining during cannon pod replacement
-------------------------------------------------------------------------

The 'cannon' component is responsible for persistent websocket connections.
Normally the default options would slowly and gracefully drain active websocket
connections over a maximum of ``(amount of cannon replicas * 30 seconds)`` during
the deployment of a new wire-server version. This will lead to a very brief
interruption for Wire clients when their client has to re-connect on the
websocket.

You're not expected to need to change these settings.

The following options are only relevant during the restart of cannon itself.
During a restart of nginz or ingress-controller, all websockets will get
severed. If this is to be avoided, see section :ref:`separate-websocket-traffic`

``drainOpts``: Drain websockets in a controlled fashion when cannon receives a
SIGTERM or SIGINT (this happens when a pod is terminated e.g. during rollout
of a new version). Instead of waiting for connections to close on their own,
the websockets are now severed at a controlled pace. This allows for quicker
rollouts of new versions.

There is no way to entirely disable this behaviour, two extreme examples below

* the quickest way to kill cannon is to set ``gracePeriodSeconds: 1`` and
  ``minBatchSize: 100000`` which would sever all connections immediately; but it's
  not recommended as you could DDoS yourself by forcing all active clients to
  reconnect at the same time. With this, cannon pod replacement takes only 1
  second per pod.
* the slowest way to roll out a new version of cannon without severing websocket
  connections for a long time is to set ``minBatchSize: 1``,
  ``millisecondsBetweenBatches: 86400000`` and ``gracePeriodSeconds: 86400``
  which would lead to one single websocket connection being closed immediately,
  and all others only after 1 day. With this, cannon pod replacement takes a
  full day per pod.

.. code:: yaml

   # overrides for wire-server/values.yaml
   cannon:
     drainOpts:
       # The following defaults drain a minimum of 400 connections/second
       # for a total of 10000 over 25 seconds
       # (if cannon holds more connections, draining will happen at a faster pace)
       gracePeriodSeconds: 25
       millisecondsBetweenBatches: 50
       minBatchSize: 20

.. _separate-websocket-traffic:

Separate incoming websocket network traffic from the rest of the https traffic
-------------------------------------------------------------------------------

By default, incoming network traffic for websockets comes through these network
hops:

Internet -> LoadBalancer -> kube-proxy -> nginx-ingress-controller -> nginz -> cannon

In order to have graceful draining of websockets when something gets restarted, as it is not easily
possible to implement the graceful draining on nginx-ingress-controller or nginz by itself, there is
a configuration option to get the following network hops:

Internet -> separate LoadBalancer for cannon only -> kube-proxy -> [nginz->cannon (2 containers in the same pod)]

.. code:: yaml

   # example on AWS when using cert-manager for TLS certificates and external-dns for DNS records
   # (see wire-server/charts/cannon/values.yaml for more possible options)

   # in your wire-server/values.yaml overrides:
   cannon:
     service:
       nginz:
         enabled: true
         hostname: "nginz-ssl.example.com"
         externalDNS:
           enabled: true
         certManager:
           enabled: true
         annotations:
           service.beta.kubernetes.io/aws-load-balancer-type: "nlb"
           service.beta.kubernetes.io/aws-load-balancer-scheme: "internet-facing"
   nginz:
     nginx_conf:
       ignored_upstreams: ["cannon"]

.. code:: yaml

   # in your wire-server/secrets.yaml overrides:
   cannon:
     secrets:
       nginz:
         zAuth:
           publicKeys: ... # same values as in nginz.secrets.zAuth.publicKeys

.. code:: yaml

   # in your nginx-ingress-services/values.yaml overrides:
   websockets:
     enabled: false


Blocking creation of personal users, new teams
----------------------------------------------

In Brig
~~~~~~~

There are some unauthenticated end-points that allow arbitrary users on the open internet to do things like create a new team.  This is desired in the cloud, but if you run an on-prem setup that is open to the world, you may want to block this.

Brig has a server option for this:

.. code:: yaml

    optSettings:
      setRestrictUserCreation: true

If `setRestrictUserCreation` is `true`, creating new personal users or new teams on your instance from outside your backend installation is impossible.  (If you want to be more technical: requests to `/register` that create a new personal account or a new team are answered with `403 forbidden`.)

On instances with restricted user creation, the site operator with access to the internal REST API can still circumvent the restriction: just log into a brig service pod via ssh and follow the steps in https://github.com/wireapp/wire-server/blob/b9a84f9b654a69c9a296761b36c042dc993236d3/deploy/services-demo/create_test_team_admins.sh.

.. note::
    Once the creation of new users and teams has been disabled, it will still be possible to use the `team creation process <https://support.wire.com/hc/en-us/articles/115003858905-Create-a-team>`__ (enter the new team name, email, password, etc), but it will fail/refuse creation late in the creation process (after the «Create team» button is clicked).

In the WebApp
~~~~~~~~~~~~~

Another way of disabling user registration is by this webapp setting, in `values.yaml`, changing this value from `true` to `false`:

.. code:: yaml

   FEATURE_ENABLE_ACCOUNT_REGISTRATION: "false"

.. note::
   If you only disable the creation of users in the webapp, but do not do so in Brig/the backend, a malicious user would be able to use the API to create users, so make sure to disable both.

You may want
------------

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
`contributing guidelines <https://github.com/wireapp/wire-server-deploy/blob/master/CONTRIBUTING.md>`__ and open a PR.

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
   guidelines <https://github.com/wireapp/wire-server-deploy/blob/master/CONTRIBUTING.md>`__ and open a PR.

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

Routing traffic to other namespaces via nginz
---------------------------------------------

If you have some components running in namespaces different from nginz. For
instance, the billing service (``ibis``) could be deployed to a separate
namespace, say ``integrations``. But it still needs to get traffic via
``nginz``. When this is needed, the helm config can be adjusted like this:

.. code:: yaml

   # in your wire-server/values.yaml overrides:
   nginz:
     nginx_conf:
       upstream_namespace:
         ibis: integrations

Marking an installation as self-hosted
--------------------------------------

In case your wire installation is self-hosted (on-premise, demo installs), it needs to be aware that it is through a configuration option.  As of release chart 4.15.0, `"true"` is the default behavior, and nothing needs to be done.

If that option is not set, team-settings will prompt users about "wire for free" and associated functions.

With that option set, all payment related functionality is disabled.

The option is `IS_SELF_HOSTED`, and you set it in your `values.yaml` file (originally a copy of `prod-values.example.yaml` found in `wire-server-deploy/values/wire-server/`).

In case of a demo install, replace `prod` with `demo`.

First set the option under the `team-settings` section, `envVars` sub-section:

.. code:: yaml

   # NOTE: Only relevant if you want team-settings
   team-settings:
     envVars:
       IS_SELF_HOSTED: "true"

Second, also set the option under the `account-pages` section:

.. code:: yaml

   # NOTE: Only relevant if you want account-pages
   account-pages:
     envVars:
       IS_SELF_HOSTED: "true"

Configuring searchability
-------------------------

You can configure how search is limited or not based on user membership in a given team.

There are two types of searches based on the direction of search:

* **Inbound** searches mean that somebody is searching for you. Configuring the inbound search visibility means that you (or some admin) can configure whether others can find you or not.
* **Outbound** searches mean that you are searching for somebody. Configuring the outbound search visibility means that some admin can configure whether you can find other users or not.

There are different types of matches: 

* **Exact handle** search means that the user is found only if the search query is exactly the user handle (e.g. searching for `mc` will find `@mc` but not `@mccaine`). This search returns zero or one results.
* **Full text** search means that the user is found if the search query contains some subset of the user display name and handle. (e.g. the query `mar` will find `Marco C`, `Omar`, `@amaro`)

Searching users on the same backend
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Search visibility is controlled by three parameters on the backend:

* A team outbound configuration flag, `TeamSearchVisibility` with possible values `SearchVisibilityStandard`, `SearchVisibilityNoNameOutsideTeam`
  * `SearchVisibilityStandard` means that the user can find other people outside of the team, if the searched-person inbound search allows it
  * `SearchVisibilityNoNameOutsideTeam` means that the user can not find any user outside the team by full text search (but exact handle search still works)
* A team inbound configuration flag, `SearchVisibilityInbound` with possible values `SearchableByOwnTeam`, `SearchableByAllTeams`
  * `SearchableByOwnTeam` means that the user can be found only by users in their own team.
  * `SearchableByAllTeams` means that the user can be found by users in any/all teams.
* A server configuration flag `searchSameTeamOnly` with possible values true, false. 
  * ``Note``: For the same backend, this affects inbound and outbound searches (simply because all teams will be subject to this behavior)
  * Setting this to `true` means that the all teams on that backend can only find users that belong to their team

These flag are set on the backend and the clients do not need to be aware of them. 

The flags will influence the behavior of the search API endpoint; clients will only need to parse the results, that are already filtered for them by the backend.

Table of possible outcomes
..........................

+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Is search-er (`uA`) in team (tA)?  | Is search-ed (`uB`) in a team?  | Backend flag `searchSameTeamOnly`  | Team `tA`'s flag `TeamSearchVisibility`  | Team tB's flag `SearchVisibilityInbound`  | Result of exact search for `uB`  | Result of full-text search for `uB`  |
+====================================+=================================+====================================+==========================================+===========================================+==================================+======================================+
| **Search within the same team**                                                                                                                                                                                                                                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, the same team `tA`         | Irrelevant                         | Irrelevant                               | Irrelevant                                | Found                            | Found                                |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| **Outbound search unrestricted**                                                                                                                                                                                                                                           |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, another team tB            | false                              | `SearchVisibilityStandard`               | `SearchableByAllTeams`                    | Found                            | Found                                |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, another team tB            | false                              | `SearchVisibilityStandard`               | `SearchableByOwnTeam`                     | Found                            | Not found                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| **Outbound search restricted**                                                                                                                                                                                                                                             |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, another team tB            | true                               | Irrelevant                               | Irrelevant                                | Not found                        | Not found                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, another team tB            | false                              | `SearchVisibilityNoNameOutsideTeam`      | Irrelevant                                | Found                            | Not found                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | No                              | false                              | `SearchVisibilityNoNameOutsideTeam`      | There’s no team B                         | Found                            | Not found                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+

Another way to express this is with the pseudocode:

.. code:: js

  def search(searcher: User, query: String) {
    val fullTextResults = searchFullText(query)
    val exactResults = findExactHandle(query)
  
    if(searcher.team == null) {
      return exactResults + fullTextResults.filter { foundUser => foundUser.team == null }
    } 
  
    // else: searcher team is NOT null
    if (backend.searchSameTeamOnly) {
      return exactResults.filter { foundUser => foundUser.team == searcher.team } + fullTextResults.filter { foundUser => foundUser.team == searcher.team}
    }
  
    // else: backend config is NOT searchSameTeamOnly
    if (searcher.team.TeamSearchVisibility == SearchVisibilityNoNameOutsideTeam) {
      return exactSearchResults + fullTextResults.filter { foundUser => foundUser.team == searcher.team }  
    }
  
    // else: searcher.team.TeamSearchVisibility == SearchVisibilityStandard
    return exactSearchResult + fullTextResults.filter { foundUser => foundUser.team == null || (foundUser.team.SearchVisibilityInbound == SearchableByAllTeams || foundUser.team == searcher.team) }
  }

Searching users on another (federated) backend
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For federated search the table above does not apply, see following table.

.. note::
	
	Incoming federated searches (i.e. searches from one backend to another) are considered always as being performed from a team user, even if they are performed from a personal user. 

	This is because the incoming search request does not carry the information whether the user performing the search was in a team or not. 
	
	So we have to make one assumption, and we assume that they were in a team.

Allowing search is done at the backend configuration level by the sysadmin:

* Outbound search restrictions (`searchSameTeamOnly`, `TeamSearchVisibility`) do not apply to federated searches
* A configuration setting `FederatedUserSearchPolicy` per federating domain with these possible values:
  * `no_search` The federating backend is not allowed to search any users (either by exact handle or full-text).  
  * `exact_handle_search` The federating backend may only search by exact handle
  * `full_search` The federating backend may search users by full text search on display name and handle. The search search results are additionally affected by `SearchVisibilityInbound` setting of each team on the backend.
* The `SearchVisibilityInbound` setting applies. Since the default value for teams is `SearchableByOwnTeam` this means that for a team to be full-text searchable by users on a federating backend both
  * `FederatedUserSearchPolicy` needs to be set to to full_search for the federating backend
  * Any team that wants to be full-text searchable needs to be set to `SearchableByAllTeams`

The configuration value `FederatedUserSearchPolicy` is per federated domain, e.g. in the values of the wire-server chart:

.. code:: yaml 

  brig:
    config:
      optSettings:
        setFederationDomainConfigs:
          - domain: a.example.com
            search_policy: no_search
          - domain: a.example.com
            search_policy: full_search

Table of possible outcomes
..........................

In the following table, user `uA` on backend A is searching for user `uB` on team `tB` on backend B. 

Any of the flags set for searching users on the same backend are ignored. 

It’s worth nothing that if two users are on two separate backend, they are also guaranteed to be on two separate teams, as teams can not spread across backends.

+-------------------------+---------------------------------------------+---------------------------------------------+----------------------------------+--------------------------------------+
| Who is searching        | Backend B flag `FederatedUserSearchPolicy`  | Team `tB`'s flag `SearchVisibilityInbound`  | Result of exact search for `uB`  | Result of full-text search for `uB`  |
+=========================+=============================================+=============================================+==================================+======================================+
| user `uA` on backend A  | `no_search`                                 | Irrelevant                                  | Not found                        | Not found                            |
+-------------------------+---------------------------------------------+---------------------------------------------+----------------------------------+--------------------------------------+
| user `uA` on backend A  | `exact_handle_search`                       | Irrelevant                                  | Found                            | Not found                            |
+-------------------------+---------------------------------------------+---------------------------------------------+----------------------------------+--------------------------------------+
| user `uA` on backend A  | `full_search`                               | SearchableByOwnTeam                         | Found                            | Not found                            |
+-------------------------+---------------------------------------------+---------------------------------------------+----------------------------------+--------------------------------------+
| user `uA` on backend A  | `full_search`                               | SearchableByAllTeams                        | Found                            | Found                                |
+-------------------------+---------------------------------------------+---------------------------------------------+----------------------------------+--------------------------------------+

Changing the settings for a given team
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you need to change searchabilility for a specific team (rather than the entire backend, as above), you need to make specific calls to the API.

Team searchVisibility
.....................

The team flag `searchVisibility` affects the outbound search of user searches. 

If it is set to `no-name-outside-team` for a team then all users of that team will no longer be able to find users that are not part of their team when searching. 

This also includes finding other users by by providing their exact handle. By default it is set to `standard`, which doesn't put any additional restrictions to outbound searches.

The setting can be changed via endpoint (for more details on how to make the API calls with `curl`, read further):

.. code::

  GET /teams/{tid}/search-visibility
    -- Shows the current TeamSearchVisibility value for the given team

  PUT /teams/{tid}/search-visibility
    -- Set specific search visibility for the team

  pull-down-menu "body":
    "standard"
    "no-name-outside-team"

The team feature flag `teamSearchVisibility` determines whether it is allowed to change the `searchVisibility` setting or not.

The default is `disabled-by-default`. 

.. note::

  Whenever this feature setting is disabled the `searchVisibility` will be reset to standard.

The default setting that applies to all teams on the instance can be defined at configuration

.. code:: yaml 

  settings:
    featureFlags:
      teamSearchVisibility: disabled-by-default # or enabled-by-default

TeamFeature searchVisibilityInbound
...................................

The team feature flag `searchVisibilityInbound` affects if the team's users are searchable by users from other teams. 

The default setting is `searchable-by-own-team` which hides users from search results by users from other teams. 

If it is set to `searchable-by-all-teams` then users of this team may be included in the results of search queries by other users.

.. note::

  The configuration of this flag does not affect search results when the search query matches the handle exactly. 
  
  If the handle is provdided then any user on the instance can find users.

This team feature flag can only by toggled by site-administrators with direct access to the galley instance (for more details on how to make the API calls with `curl`, read further):

.. code::

  PUT /i/teams/{tid}/features/search-visibility-inbound

With JSON body:

.. code:: json
  
  {"status": "enabled"} 
  
or

.. code:: json
  
  {"status": "disabled"}

Where `enabled` is equivalent to `searchable-by-all-teams` and `disabled` is equivalent to `searchable-by-own-team`.

The default setting that applies to all teams on the instance can be defined at configuration.

.. code:: yaml 

  searchVisibilityInbound:
    defaults:
      status: enabled # OR disabled

Individual teams can overwrite the default setting with API calls as per above.

Making the API calls
....................

To make API calls to set an explicit configuration for` TeamSearchVisibilityInbound` per team, you first need to know the Team ID, which can be found in the team settings app.

It is an `UUID<https://en.wikipedia.org/wiki/Universally_unique_identifier>` which has format like this  `dcbedf9a-af2a-4f43-9fd5-525953a919e1`. 

In the following we will be using this Team ID as an example, please replace it with your own team id.

Next find the name of a `galley` pod by looking at the output of running this command:

.. code:: sh

  kubectl -n wire get pods

The output will look something like this:

.. code::

  ...
  galley-5f4787fdc7-9l64n   ...
  galley-migrate-data-lzz5j ...
  ...

Select any of the galley pods, for example we will use `galley-5f4787fdc7-9l64n`.

Next, set up a port-forwarding from your local machine's port `9000` to the galley's port `8080` by running:

.. code:: sh

	kubectl port-forward -n wire galley-5f4787fdc7-9l64n 9000:8080

Keep this command running until the end of these instuctions. 

Please run the following commands in a seperate terminal while keeping the terminal which establishes the port-forwarding open.

To see team's current setting run:

.. code:: sh

  curl -XGET http://localhost:9000/i/teams/dcbedf9a-af2a-4f43-9fd5-525953a919e1/features/searchVisibilityInbound
  
  # {"lockStatus":"unlocked","status":"disabled"}

Where `disabled` corresponds to `SearchableByOwnTeam` and enabled corresponds to `SearchableByAllTeams`.

To change the `TeamSearchVisibilityInbound` to `SearchableByAllTeams` for the team run:

.. code:: sh

  curl -XPUT -H 'Content-Type: application/json' -d "{\"status\": \"enabled\"}" http://localhost:9000/i/teams/dcbedf9a-af2a-4f43-9fd5-525953a919e1/features/searchVisibilityInbound

To change the TeamSearchVisibilityInbound to SearchableByOwnTeam for the team run:

.. code:: sh

  curl -XPUT -H 'Content-Type: application/json' -d "{\"status\": \"disabled\"}" http://localhost:9000/i/teams/dcbedf9a-af2a-4f43-9fd5-525953a919e1/features/searchVisibilityInbound

 