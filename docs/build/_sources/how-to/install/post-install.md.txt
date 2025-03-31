(checks)=
# Verifying your installation

After a successful installation of wire-server and its components, there are some useful checks to be run to ensure the proper functioning of the system. Here's a non-exhaustive list of checks to run on the hosts:


(ntp-check)=

## NTP Checks

Ensure that NTP is properly set up on all nodes. Particularly for Cassandra **DO NOT** use anything else other than ntp. Here are some helpful blogs that explain why:

> - <https://blog.rapid7.com/2014/03/14/synchronizing-clocks-in-a-cassandra-cluster-pt-1-the-problem/>
> - <https://www.digitalocean.com/community/tutorials/how-to-set-up-time-synchronization-on-ubuntu-16-04>

### How can I see if NTP is correctly set up?

This is an important part of your setup, particularly for your Cassandra nodes. You should use `ntpd` and our ansible scripts to ensure it is installed correctly - but you can still check it manually if you prefer. The following 2 sub-sections explain both approaches.

#### I used your ansible scripts and prefer to have automated checks

Then the easiest way is to use [this ansible playbook](https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/cassandra-verify-ntp.yml)

#### I am not using ansible and like to SSH into hosts and checking things manually

The following shows how to check for existing servers connected to (assumes `ntpq` is installed)

```sh
ntpq -pn
```

which should yield something like this:

```sh
     remote           refid      st t when poll reach   delay   offset  jitter
==============================================================================
 time.example.    .POOL.          16 p    -   64    0    0.000    0.000   0.000
+<IP_ADDR_1>      <IP_ADDR_N>      2 u  498  512  377    0.759    0.039   0.081
*<IP_ADDR_2>      <IP_ADDR_N>      2 u  412  512  377    1.251   -0.670   0.063
```

if your output shows \_ONLY\_ the entry with a `.POOL.` as `refid` and a lot of 0s, something is probably wrong, i.e.:

```sh
     remote           refid      st t when poll reach   delay   offset  jitter
==============================================================================
 time.example.    .POOL.          16 p    -   64    0    0.000    0.000   0.000
```

What should you do if this is the case? Ensure that `ntp` is installed and that the servers in the pool (typically at `/etc/ntp.conf`) are reachable.


(logrotation-check)=

## Logs and Data Protection checks

On Wire.com, we keep logs for a maximum of 72 hours as described in the [privacy whitepaper](https://wire-docs.wire.com/download/Wire+Privacy+Whitepaper.pdf).

We recommend you do the same and limit the amount of logs kept on your servers.

### How can I see how far in the past access logs are still available on my servers?

Look at the timestamps of your earliest nginz logs:

```sh
export NAMESPACE=default # this may be 'default' or 'wire'
kubectl -n "$NAMESPACE" get pods | grep nginz
# choose one of the resulting names, it might be named e.g. nginz-6d75755c5c-h9fwn
kubectl -n "$NAMESPACE" logs <name-from-previous-command> -c nginz | head -10
```

If the timestamp is more than 3 days in the past, your logs are kept for unnecessary long amount of time and you should configure log rotation.

### I used your ansible scripts and prefer to have the default 72 hour maximum log availability configured automatically.

You can use [the kubernetes_logging.yml ansible playbook](https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/kubernetes_logging.yml)

#### I am not using ansible and like to SSH into hosts and configure things manually

SSH into one of your kubernetes worker machines.

If you installed as per the instructions on docs.wire.com, then the default logging strategy is `json-file` with `--log-opt max-size=50m --log-opt max-file=5` storing logs in files under `/var/lib/docker/containers/<container-id>/<container-id>.log`. You can check this with these commands:

```sh
docker info --format '{{.LoggingDriver}}'
ps aux | grep log-opt
```

(Options configured in `/etc/systemd/system/docker.service.d/docker-options.conf`)

The default will thus keep your logs around until reaching 250 MB per pod, which is far longer than three days. Since docker logs don't allow a time-based log rotation, we can instead make use of [logrotate](https://linux.die.net/man/8/logrotate) to rotate logs for us.

Create the file `/etc/logrotate.d/podlogs` with the following contents:

% NOTE: in case you change these docs, also make sure to update the actual code
% under https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/kubernetes_logging.yml

```
"/var/lib/docker/containers/*/*.log"
{
  daily
  missingok
  rotate 2
  maxage 1
  copytruncate
  nocreate
  nocompress
  }
```

Repeat the same for all the other kubernetes worker machines, the file needs to exist on all of them.

There should already be a cron job for logrotate for other parts of the system, so this should be sufficent, you can stop here.

You can check for the cron job with:

```
ls /etc/cron.daily/logrotate
```

And you can manually run a log rotation using:

```
/usr/sbin/logrotate -v /etc/logrotate.conf
```

If you want to clear out old logs entirely now, you can force log rotation three times (again, on all kubernetes machines):

```
/usr/sbin/logrotate -v -f /etc/logrotate.conf
/usr/sbin/logrotate -v -f /etc/logrotate.conf
/usr/sbin/logrotate -v -f /etc/logrotate.conf
```
