.. _ntp-check:

Logs and Data Protection checks
===============================

On Wire.com, we keep logs for a maximum of 72 hours as described in the `privacy whitepaper <https://wire.com/en/security/>`_

We recommend you do the same and limit the amount of logs kept on your servers.

How can I see how far in the past access logs are still available on my servers?
--------------------------------------------------------------------------------

Look at the timestamps of your earliest nginz logs:

.. code:: sh

   kubectl logs -n wire -l wireService=nginz -c nginz | head -10

If the timestamp is more than 3 days in the past, your logs are kept for unnecessary long amount of time and you should configure log rotation.

I used your ansible scripts and prefer to have the default 72 hour maximum log availability configured automatically.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use `the kubernetes_logging.yml ansible playbook <https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/kubernetes_logging.yml>`_

I am not using ansible and like to SSH into hosts and configure things manually
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SSH into one of your kubernetes worker machines.

If you installed as per the instructions on docs.wire.com, then the default logging strategy is ``json-file`` with ``--log-opt max-size=50m --log-opt max-file=5`` storing logs in files under ``/var/lib/docker/containers/<container-id>/<container-id>.log``. You can check this with these commands:

.. code:: sh

   docker info --format '{{.LoggingDriver}}'
   ps aux | grep log-opt

(Options configured in ``/etc/systemd/system/docker.service.d/docker-options.conf``)

The default will thus keep your logs around until reaching 250 MB per pod, which is far longer than three days. Since docker logs don't allow a time-based log rotation, we can instead make use of `logrotate <https://linux.die.net/man/8/logrotate>`__ to rotate logs for us.

Create the file ``/etc/logrotate.d/podlogs`` with the following contents:

..
   NOTE: in case you change these docs, also make sure to update the actual code
   under https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/kubernetes_logging.yml
.. code::

   "/var/lib/docker/containers/*/*.log"
   {
     daily
     missingok
     rotate 2
     maxage 1
     copytruncate
     nocreate
     nocompress
     }

Repeat the same for all the other kubernetes worker machines, the file needs to exist on all of them.

There should already be a cron job for logrotate for other parts of the system, so this should be sufficent, you can stop here.

You can check for the cron job with::

   ls /etc/cron.daily/logrotate

And you can manually run a log rotation using::

   /usr/sbin/logrotate -v /etc/logrotate.conf

If you want to clear out old logs entirely now, you can force log rotation three times (again, on all kubernetes machines)::

   /usr/sbin/logrotate -v -f /etc/logrotate.conf
   /usr/sbin/logrotate -v -f /etc/logrotate.conf
   /usr/sbin/logrotate -v -f /etc/logrotate.conf
