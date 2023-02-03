# 2023-01-04 - Outage of wire.com caused by a DoS attack

Last updated: 2023-01-19

## What happened?
On Tuesday, 2023-01-04, the Wire website wire.com was affected by an outage caused by a Denial-of-Service attack. This outage only concerns the wire.com website and none of the services provided by Wire. 

## What was the impact identified?
Several outages of short periods (7min, 2min, 3min, 4min) have been identified beginning from UTC 05:13.

## Are Wire installations affected?
Wire/wire-server was not affected by the wire.com website outage.

## Timeline

*2023-01-04 05:13*: The website monitor triggered an alert on a server issue.\
*2023-01-04 05:16*: The responsible team of the service provider responded and saw an outage for periods of 7 minutes beginning from UTC 05:13, 2 minutes (UTC 05:29), 3 minutes (UTC 05:36) and 4 minutes (UTC 05:43).\
*2023-01-04/11:34*: Wire was informed about an attempted DoS attack on wire.com by the service provider which manually blocked the IP address in the process.\
*2023-01-04 09:15*: Wire started an internal investigation to check whether other systems have been affected, which was not the case.\
*2023-01-04 14:30*: Wire contacted the service provider for more details on the incident as well as the corresponding log files.\
*2023-01-18 09:53*: Wire received the incident report from the service provider.\
