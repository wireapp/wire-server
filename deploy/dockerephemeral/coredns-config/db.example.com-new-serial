$ORIGIN example.com.
@	1 IN	SOA sns.dns.icann.org. noc.dns.icann.org. (
				2023101617 ; serial
				1       ; refresh (2 hours)
				1       ; retry (1 hour)
				1    ; expire (2 weeks)
				1       ; minimum (1 hour)
				)

	1 IN NS a.iana-servers.net.
	1 IN NS b.iana-servers.net.

www     IN A     127.0.0.1
        IN AAAA  ::1
_wire-server-federator._tcp    IN SRV 0 0 8443 localhost.
_wire-server-federator._tcp.b  IN SRV 0 0 9443 localhost.
_wire-server-federator._tcp.d1  IN SRV 0 0 10443 localhost.
_wire-server-federator._tcp.d2  IN SRV 0 0 11443 localhost.
_wire-server-federator._tcp.d3  IN SRV 0 0 12443 localhost.
