# Federation Design Aspects

(configuring-remote-connections-dev-perspective)=

## keeping track of federator remotes

Federation can start and end.  These events need handlers to be called
(like remove remote users from local conv), plus it is not convenient
to edit and re-deploy config files every time that happens.  Hence
remotes are stored in cassandra in brig, and every pod of every
service keeps a cache in a `TVar` (this information is needed in many
end-points).

This secion elaborates on the implementation.  See
{ref}`configuring-remote-connections` for the administrator's point of
view.  Go read that section now!

The state is persistent in cassandra table `brig.federation_remotes`
brig itself for performance keeps a `TVar` that it updates at regular
intervals.  Plus provides the contents of the `TVar` via an internal
CRUD API (see {ref}`configuring-remote-connections` for the links).

Update intervals are currently hard-wired into the code.

Introduced in
[PR#3260](https://github.com/wireapp/wire-server/pull/3260).
