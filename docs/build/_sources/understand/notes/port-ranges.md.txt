---
orphan: true
---

(port-ranges)=

# Note on port ranges

Some parts of Wire (SFT, Restund) related to conference calling and Audio/Video, establish outgoing connections in a range of UDP ports. Which ports are used is determined by the kernel using `/proc/sys/net/ipv4/ip_local_port_range`.

The /proc/sys/net/ipv4/ip_local_port_range defines the local port range that is used by TCP and UDP traffic to choose the local port.

You will see in the parameters of this file two numbers: The first number is the first local port allowed for TCP and UDP traffic on the server, the second is the last local port number.

When setting up firewall rules, this entire range must be allowed for both UDP and TCP.

This range is defined by the system, and is set by the `/proc/sys/net/ipv4/ip_local_port_range` parameter.

You read this range for your system by running the following command:

```bash
cat /proc/sys/net/ipv4/ip_local_port_range
```

Or by finding the following line in your `/etc/sysctl.conf` file, if it exists:

```
# Allowed local port range
net.ipv4.ip_local_port_range = 32768 61000
```

To change the range, edit the `/etc/sysctl.conf` file or run the following command:

```bash
echo "32768 61001" > /proc/sys/net/ipv4/ip_local_port_range
```
