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
view.

The state is persistent in cassandra table `brig.federation_remotes`
brig itself for performance keeps a `TVar` that it updates at regular
intervals.  Plus provides the contents of the `TVar` via an internal
[CRUD API](TODO: swagger docs).

Update intervals could be made configurable in config files, but we
chose to hard-wire this for now: values are [TODO].

Transition from config file to cassandra table: we consider the union
for now, and don't allow removing remote hosts that are (also) given
in the config file.  A future release will stop honoring the config
file altogether.  By then you'll have to be done getting the data into
cassandra.
