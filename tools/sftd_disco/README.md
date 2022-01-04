# sftd-disco

This DISCOvery docker image/bash script converts the result from an SRV DNS lookup of a kubernetes service to a file which can be served by nginx or similar.

This is useful as a sidecar container to the sftd chart in kubernetes to expose the full list of running sftd servers in cases where sftd runs independently from other backend services. See also [the sftd helm chart](https://github.com/wireapp/wire-server/tree/develop/charts/sftd/)
