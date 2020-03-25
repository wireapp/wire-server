.. _ntp-check:

NTP Checks
==========

Ensure that NTP is properly set up on all nodes. Particularly for Cassandra **DO NOT** use anything else other than ntp. Here are some helpful blogs that explain why:

 * https://blog.rapid7.com/2014/03/14/synchronizing-clocks-in-a-cassandra-cluster-pt-1-the-problem/
 * https://www.digitalocean.com/community/tutorials/how-to-set-up-time-synchronization-on-ubuntu-16-04

How can I see if NTP is correctly set up?
-----------------------------------------

This is an important part of your setup, particularly for your Cassandra nodes. You should use `ntpd` and our ansible scripts to ensure it is installed correctly - but you can still

I used your ansible scripts and prefer to have automated checks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Then the easiest way is to use `this ansible playbook <https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/cassandra-verify-ntp.yml>`_

I am not using ansible and like to SSH into hosts and checking things manually
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following shows how to check for existing servers connected to (assumes `ntpq` is installed)

.. code:: sh

  ntpq -pn

which should yield something like this:

.. code:: sh

        remote           refid      st t when poll reach   delay   offset  jitter
   ==============================================================================
    time.example.    .POOL.          16 p    -   64    0    0.000    0.000   0.000
   +<IP_ADDR_1>      <IP_ADDR_N>      2 u  498  512  377    0.759    0.039   0.081
   *<IP_ADDR_2>      <IP_ADDR_N>      2 u  412  512  377    1.251   -0.670   0.063

if your output shows _ONLY_ the entry with a `.POOL.` as `refid` and a lot of 0s, something is probably wrong, i.e.:

.. code:: sh

        remote           refid      st t when poll reach   delay   offset  jitter
   ==============================================================================
    time.example.    .POOL.          16 p    -   64    0    0.000    0.000   0.000

What should you do if this is the case? Ensure that `ntp` is installed and that the servers in the pool (typically at `/etc/ntp.conf`) are reachable.
