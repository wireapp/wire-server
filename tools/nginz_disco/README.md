# nginz-disco

Due to nginx not supporting DNS names for its list of upstream servers (unless you pay extra), the nginz-disco container is a simple bash script to do DNS lookups and write the resulting IPs to a file. Nginz reloads on changes to this file.

This is useful as a sidecar container to nginz in kubernetes. See also [the nginz helm chart](https://github.com/wireapp/wire-server/tree/develop/charts/nginz/)

