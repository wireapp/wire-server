:orphan:

Note on port ranges
===================

The /proc/sys/net/ipv4/ip_local_port_range defines the local port range that is used by TCP and UDP traffic to choose the local port. 

You will see in the parameters of this file two numbers: The first number is the first local port allowed for TCP and UDP traffic on the server, the second is the last local port number.

When setting up firewall rules, this entire range must be allowed for both UDP and TCP. 

This range is defined by the system, and is set by the ``/proc/sys/net/ipv4/ip_local_port_range`` parameter.

You read this range for your system by running the following command:

.. code-block:: bash

    cat /proc/sys/net/ipv4/ip_local_port_range

Or by finding the following line in your ``/etc/sysctl.conf`` file, if it exists:

.. code-block::

    # Allowed local port range
    net.ipv4.ip_local_port_range = 32768 61000

To change the range, edit the ``/etc/sysctl.conf`` file or run the following command:

.. code-block:: bash

    echo "32768 61001" > /proc/sys/net/ipv4/ip_local_port_range

