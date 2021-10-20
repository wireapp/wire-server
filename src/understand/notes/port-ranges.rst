:orphan:

Note on port ranges
===================

Some parts of the wire system use the port range 32768 to 61000 for some operations, for example SFT and Restund.

When setting up firewall rules, this entire range must be allowed for both UDP and TCP. 

If only TCP is allowed, but not UDP, Restund (Audio/Video) call quality will be degraded.

This range is defined by the system, and is set by the ``/proc/sys/net/ipv4/ip_local_port_range`` parameter.

You read this range for your system by running the following command:

.. code-block:: bash

    cat /proc/sys/net/ipv4/ip_local_port_range

Or by finding the following line in your ``/etc/sysctl.conf`` file, if it exists:

.. code-block:: bash

    # Allowed local port range
    net.ipv4.ip_local_port_range = 32768 61000

