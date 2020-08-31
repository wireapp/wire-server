Troubleshooting during installation
-------------------------------------

Problems with CORS on the web based applications (webapp, team-settings, account-pages)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have installed wire-server, but the web application page in your browser has connection problems and throws errors in the console such as `"Refused to connect to 'https://assets.example.com' because it violates the following Content Security Policies"`, make sure to check that you have configured the ``CSP_EXTRA_`` environment variables.

In the file that you use as override when running ``helm install/update -f <override values.yaml>`` (using the webapp as an example):

.. code:: yaml

   webapp:
      # ... other settings...
      envVars:
        # ... other environment variables ...
        CSP_EXTRA_CONNECT_SRC: "https://*.example.com, wss://*.example.com"
        CSP_EXTRA_IMG_SRC: "https://*.example.com"
        CSP_EXTRA_SCRIPT_SRC: "https://*.example.com"
        CSP_EXTRA_DEFAULT_SRC: "https://*.example.com"
        CSP_EXTRA_FONT_SRC: "https://*.example.com"
        CSP_EXTRA_FRAME_SRC: "https://*.example.com"
        CSP_EXTRA_MANIFEST_SRC: "https://*.example.com"
        CSP_EXTRA_OBJECT_SRC: "https://*.example.com"
        CSP_EXTRA_MEDIA_SRC: "https://*.example.com"
        CSP_EXTRA_PREFETCH_SRC: "https://*.example.com"
        CSP_EXTRA_STYLE_SRC: "https://*.example.com"
        CSP_EXTRA_WORKER_SRC: "https://*.example.com"

For more info, you can have a look at respective charts values files, i.e.:

  * https://github.com/wireapp/wire-server-deploy/blob/develop/charts/account-pages/values.yaml
  * https://github.com/wireapp/wire-server-deploy/blob/develop/charts/team-settings/values.yaml
  * https://github.com/wireapp/wire-server-deploy/blob/develop/charts/webapp/values.yaml

Problems with ansible and python versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If for instance the following fails::

    ansible all -i hosts.ini -m shell -a "echo hello"

If your target machine only has python 3 (not python 2.7), you can tell ansible to use python 3 by default, by specifying `ansible_python_interpreter`:

.. code:: ini

   # hosts.ini

   [all]
   server1 ansible_host=1.2.3.4


   [all:vars]
   ansible_python_interpreter=/usr/bin/python3

(python 3 may not be supported by all ansible modules yet)


Flaky issues with Cassandra (failed QUORUMs, etc.)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Cassandra is *very* picky about time! Ensure that NTP is properly set up on all nodes. Particularly for Cassandra *DO NOT* use anything else other than ntp. Here are some helpful blogs that explain why:

 * https://blog.rapid7.com/2014/03/14/synchronizing-clocks-in-a-cassandra-cluster-pt-1-the-problem/
 * https://blog.rapid7.com/2014/03/17/synchronizing-clocks-in-a-cassandra-cluster-pt-2-solutions/
 * https://www.digitalocean.com/community/tutorials/how-to-set-up-time-synchronization-on-ubuntu-16-04

How can I ensure that I have correctly setup NTP on my machine(s)? Have a look at `this ansible playbook <https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/cassandra-verify-ntp.yml>`_


I deployed the ``demo-smtp`` but I'm not receiving any verification emails
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Check whether brig deployed successfully (brig pod(s) should be in state *Running*) ::

    kubectl get pods -o wide

2. Inspect Brig logs ::

    kubectl logs $BRING_POD_NAME

3. The receiving email server might refuse to accept any email sent by the `demo-smtp` server, due to not being
   a trusted origin. You may want to set up one of the following email verification mechanisms.

* `SFP <https://en.wikipedia.org/wiki/Sender_Policy_Framework>`__
* `DKIM <https://en.wikipedia.org/wiki/DomainKeys_Identified_Mail>`__
* `DMARC <https://en.wikipedia.org/wiki/DMARC>`__


4. You may want to adjust the SMTP configuration for Brig (``wire-server/[values,secrets].yaml``).

.. code:: yaml

    brig:
      config:
        smtp:
          host: 'demo-smtp'
          port: 25
          connType: 'plain'


.. code:: yaml

    brig:
      secrets:
        smtpPassword: dummyPassword

(Don't forget to apply the changes with ``helm upgrade wire-server wire/wire-server -f values.yaml -f secrets.yaml``)
