$ORIGIN example.com.
@	3600 IN	SOA sns.dns.icann.org. noc.dns.icann.org. (
				2017042745 ; serial
				7200       ; refresh (2 hours)
				3600       ; retry (1 hour)
				1209600    ; expire (2 weeks)
				3600       ; minimum (1 hour)
				)

	3600 IN NS a.iana-servers.net.
	3600 IN NS b.iana-servers.net.

www     IN A     127.0.0.1
        IN AAAA  ::1
_wire-server-federator._tcp    IN SRV 0 0 8443 host.docker.internal.
_wire-server-federator._tcp.b  IN SRV 0 0 9443 host.docker.internal.
_wire-server-federator._tcp.d1  IN SRV 0 0 10443 host.docker.internal.
_wire-server-federator._tcp.d2  IN SRV 0 0 11443 host.docker.internal.
_wire-server-federator._tcp.d3  IN SRV 0 0 12443 host.docker.internal.
_wire-server-federator._tcp.federation-v0  IN SRV 0 0 21443 host.docker.internal.
_wire-server-federator._tcp.federation-v1  IN SRV 0 0 22443 host.docker.internal.
_wire-server-federator._tcp.federation-v2  IN SRV 0 0 23443 host.docker.internal.
