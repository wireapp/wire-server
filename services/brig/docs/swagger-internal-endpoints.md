These Swagger docs document the internal API of `wire-server`. I.e. the
endpoints are only reachable inside the Wire cluster, usually used for
communication between services.

Request execution does not work as Swagger expects a single target host whereas
these endpoints are served by multiple hosts.
