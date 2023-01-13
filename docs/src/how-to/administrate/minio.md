# Minio

```{eval-rst}
.. include:: includes/intro.rst
```

This section only covers the bare minimum, for more information, see the [minio documentation](https://docs.min.io/)

## Should you be using minio?

Minio can be used to emulate an S3-compatible setup. When a native S3-like
storage provider is already present in your network or cloud provider, we
advise using that instead.

## Setting up interaction with Minio

Minio can be installed on your servers using our provided ansible playbooks.
The ansible playbook will also install the minio client and configure it to
talk to the minio server running on the same node as it was installed.  For the
client-perspective, it does not matter to which server it is talking; all
actions will propagate around the cluster. One could for example configure
minio to run behind a loadbalancer like HAProxy, and configure the Minio client
to point to this loadbalancer instead.

Our ansible playbooks will also configure the minio client and adds the locally
reachable API under the `local` alias:

```
mc config host list
```

If it is not there, it can be added manually as follows:

```
mc config host add local http://localhost:9000 <YOUR-ACCESS-KEY> <YOUR-SECRET-KEY>
```

The status of the cluster can be requested by contacting any of the servers. In
our case we will contact the locally running server:

```
mc admin info local
```

## Minio maintenance

There will be times where one wants to take a minio server down for
maintenance. One might want to apply security patches, or want to take out a
broken disk and replace it with a fixed one.  Minio will not tell you the
health status of disks. You should have separate alerting and monitoring in
place to keep track of hardware health. For example, one could look at
S.M.A.R.T. values that the disks produce with Prometheus [node_exporter](https://github.com/prometheus-community/node-exporter-textfile-collector-scripts/blob/master/smartmon.sh)

Special care has to be taken when restarting Minio nodes, but it should be safe
to do so.  Minio can operate in read-write mode with (N/2) + 1 instances
available in the cluster and it can operate in read-only mode with N/2 nodes
available in the cluster.  To ensure normal functioning of the cluster without
downtime, we advice taking down servers and setting them up again one by one.

Note that stopping a server might potentially disrupt any API call that is
being made to Minio. If someone is writing to a bucket, and this API call
is being served by the server we are restarting, then this API call will be
interrupted and the user must retry.  When you shut down a node, one should
take precautions that subsequent API calls are sent to other nodes in the
cluster.

To stop a server, type:

```
systemctl stop minio-server
```

Writes that happen during the server being down will not be synced to the
server that is offline. It is important that once you bring the server back
online that you heal it. This will redistribute that data and parity
information such that the files that were written during the downtime are
redundantly stored again.  If one does not heal a server after being down, and continues
to restart servers, then data might end up not being redundantly stored and will but
unrecoverable in case of disk loss. It
is thus recommended to heal an instance immediately once it is back up; before attemtping to
restart any other instances.

Now that the server is offline, perform any maintenance that you want to do.
Afterwards, restart it with:

```
systemctl start minio-server
```

Now check:

```
mc admin info local
```

to see if the cluster is healthy.

Now that the server is back online, it has missed writes that have happened whilst
it was offline. Because of this we must heal the cluster now
A heal of the cluster is performed as follows:

```
mc admin heal -r local
```

Which will show a result page that looks like this:

```
◑  bunny
   0/0 objects; 0 B in 2s
   ┌────────┬───┬─────────────────────┐
   │ Green  │ 2 │  66.7% ████████     │
   │ Yellow │ 1 │  33.3% ████         │
   │ Red    │ 0 │   0.0%              │
   │ Grey   │ 0 │   0.0%              │
   └────────┴───┴─────────────────────┘
```

green - all good
yellow - healed partially
red - quorum missing
grey - more than quorum number shards are gone, means the object for some reason is not recoverable

When there are any yellow items, it usually means that not all servers have seen
the node come up properly again. Running the heal command with the `--json` option
will give you more verbose and precise information why the heal only happened partially.

```json
{
   "after" : {
      "online" : 5,
      "offline" : 1,
      "missing" : 0,
      "corrupted" : 0,
      "drives" : [
         {
            "endpoint" : "http://10.0.0.42:9091/var/lib/minio-server1",
            "state" : "offline",
            "uuid" : ""
         },
         {
            "uuid" : "",
            "endpoint" : "/var/lib/minio-server1",
            "state" : "ok"
         }
      ],
      "color" : "yellow"
   }
}
```

In our case, we see that the reason for the partial recovery was that
one the server was still considered offline. Rerunning the command yielded
a Green heal.

On a routine restart of the system, such a heal procedure should not take very
long, as there is already some data on the disk from before you shut the server
down. If you replaced an unhealthy disk during the downtime, healing
might take a bit longer, as there is no local data to recover from.

After the server has successfully been healed, you can continue restarting the
next server.  Repeat this process until all servers have been restarted and are
healthy.

Note that there are other reasons but servers restarts that can cause nodes to
become out of sync.  For example, if there is a network failure that causes
some of the nodes to not be reachable, writes will be less durable too. It is
thus important to have good monitoring in place and respond accordingly.  Minio
itself will auto-heal the cluster every month if the administrator doesn't
trigger a heal themselves.

## Rotate root credentials

In order to change the root credentials, one needs to restart minio once but
set with the old and the new credentials at the same time.
If you installed minio with the Ansible, the [role](https://github.com/wireapp/ansible-minio)
takes care of this. Just change the inventory accordingly and re-apply the
role.

For more information, please refer to the *Credentials* section in the [official documentation](https://docs.min.io/docs/minio-server-configuration-guide.html).

(check-the-health-of-a-minio-node)=

## Check the health of a MinIO node

This is the procedure to check a minio node's health

First log into the minio server

```sh
ssh <ip of minio node>
```

There, run the following commands:

```sh
env $(sudo grep KEY /etc/default/minio-server1 | xargs) bash
export MC_HOST_local="http://$MINIO_ACCESS_KEY:$MINIO_SECRET_KEY@127.0.0.1:9000"
mc admin info local
```

You should see a result similar to this:

```sh
*  192.168.0.12:9092
Uptime: 2 months
Version: 2020-10-28T08:16:50Z
Network: 6/6 OK
Drives: 1/1 OK

*  192.168.0.22:9000
Uptime: 2 months
Version: 2020-10-28T08:16:50Z
Network: 6/6 OK
Drives: 1/1 OK

*  192.168.0.22:9092
Uptime: 2 months
Version: 2020-10-28T08:16:50Z
Network: 6/6 OK
Drives: 1/1 OK

*  192.168.0.32:9000
Uptime: 2 months
Version: 2020-10-28T08:16:50Z
Network: 6/6 OK
Drives: 1/1 OK

*  192.168.0.32:9092
Uptime: 2 months
Version: 2020-10-28T08:16:50Z
Network: 6/6 OK
Drives: 1/1 OK

*  192.168.0.12:9000
Uptime: 2 months
Version: 2020-10-28T08:16:50Z
Network: 6/6 OK
Drives: 1/1 OK
```

Make sure you see `Network: 6/6 OK`.

Reboot the machine with:

```sh
sudo reboot
```

Then wait at least a minute.

If you go to ssh in, and get 'Connection refused', it just means you need to wait a bit longer.

Tip: You can automatically ask SSH to attempt to connect until it is succesful, by using the following command:

```sh
ssh -o 'ConnectionAttempts 3600' <ip of minio node> exit
```

Log into minio ( repeat the steps above ), and check again.

You should see a very low uptime value on two hosts now.

This is because we install minio 'twice' on each host.
