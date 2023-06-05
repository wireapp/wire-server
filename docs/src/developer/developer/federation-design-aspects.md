# Federation Design Aspects

(configuring-remote-connections-dev-perspective)=

## Keeping track of federator remotes

**Since [PR#3260](https://github.com/wireapp/wire-server/pull/3260).**

Federation can start and end.  These events need handlers to be called
(like remove remote users from local conv), plus it is not convenient
to edit and re-deploy config files every time that happens.  Hence
remotes are stored in cassandra in brig, and every pod of every
service keeps a cache in an `IORef` in its `Env` (this information is
needed in many end-points, so it has to remain as fast as read access
to `Env`).

See {ref}`configure-federation-strategy-in-brig` for the
administrator's point of view.  If you haven't done so, go read that
section now!

The state is persisted in cassandra table `brig.federation_remotes`.
brig provides the contents via an internal CRUD API (see
{ref}`configure-federation-strategy-in-brig` for the links).  In the
future, we may decide that brig needs to cache the table itself, but
for now (`GET` is only used for the internal end-point to share it
with other services) we hope to get away with the simple solution and
always read from cassandra directly.

(More details to be added?)
