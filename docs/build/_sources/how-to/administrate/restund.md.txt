# Restund (TURN)

```{eval-rst}
.. include:: includes/intro.rst
```

(allocations)=

## Wire-Server Configuration

The wire-server can either serve a static list of TURN servers to the clients or
it can discovery them using DNS SRV Records.

### Static List

To configure a static list of TURN servers to use, override
`values/wire-server/values.yaml` like this:

```yaml
# (...)

brig:
# (...)
  turnStatic:
    v1:
      # v1 entries can be ignored and are not in use anymore since end of 2018.
    v2:
    - turn:server1.example.com:3478 # server 1 UDP
    - turn:server1.example.com:3478?transport=tcp # server 1 TCP
    - turns:server1.example.com:5478?transport=tcp # server 1 TLS
    - turn:server2.example.com:3478 # server 2 UDP
    - turn:server2.example.com:3478?transport=tcp # server 2 TCP
    - turns:server2.example.com:5478?transport=tcp # server 2 TLS
  turn:
    serversSource: files
```

### DNS SRV Records

To configure wire-server to use DNS SRV records in order to discover TURN
servers, override `values/wire-server/values.yaml` like this:

```yaml
# (...)

brig:
# (...)
  turn:
    serversSource: dns
    baseDomain: prod.example.com
    discoveryIntervalSeconds: 10
```

When configured like this, the wire-server would look for these 3 SRV records
every 10 seconds:

1. `_turn._udp.prod.example.com` will be used to discover UDP hostnames and port for all the
   turn servers.
2. `_turn._tcp.prod.example.com` will be used to discover the TCP hostnames and port for all
   the turn servers.
3. `_turns._tcp.prod.example.com` will be used to discover the TLS hostnames and port for
   all the turn servers.

Entries with weight 0 will be ignored. Example:

```
dig +retries=3 +short SRV _turn._udp.prod.example.com

0 0 3478 turn36.prod.example.com
0 10 3478 turn34..prod.example.com
0 10 3478 turn35.prod.example.com
```

At least one of these 3 lookups must succeed for the wire-server to be able to
respond correctly when `GET /calls/config/v2` is called. All successful
responses are served in the result.

In addition, if there are any clients using the legacy endpoint, `GET
/calls/config`, (all versions of all mobile apps since 2018 no longer use this) they will be served by the servers listed in the
`_turn._udp.prod.example.com` SRV record. This endpoint, however, will not
serve the domain names received inside the SRV record, instead it will serve the
first `A` record that is associated with each domain name in the SRV record.

## How to see how many people are currently connected to the restund server

You can see the count of currently ongoing calls (also called "allocations"):

```sh
echo turnstats | nc -u 127.0.0.1 33000 -q1 | grep allocs_cur | cut -d' ' -f2
```

## How to restart restund (with downtime)

With downtime, it's very easy:

```
systemctl restart restund
```

```{warning}
Restarting `restund` means any user that is currently connected to it (i.e. having a call) will lose its audio/video connection. If you wish to have no downtime, check the next section\*
```

(rebooting-a-restund-node)=

## Rebooting a Restund node

If you want to reboot a restund node, you need to make sure the other restund nodes in the cluster are running, so that services are not interrupted by the reboot.

```{warning}
This procedure as described here will cause downtime, even if a second restund server is up; and kill any ongoing audio/video calls. The sections further up describe a downtime and a no-downtime procedure.
```

Presuming your two restund nodes are called:

- `restund-1`
- `restund-2`

To prepare for a reboot of `restund-1`, log into the other restund server (`restund-2`, for example here), and make sure the docker service is running.

List the running containers, to ensure restund is running, by executing:

```sh
ssh -t <ip of restund-2> sudo docker container ls
```

You should see the following in the results:

```sh
CONTAINER ID         IMAGE                                COMMAND         STATUS        PORTS    NAMES
<random hash>        quay.io/wire/restund:v0.4.16b1.0.53  22 seconds ago  Up 18 seconds          restund
```

Make sure you see this restund container, and it is running ("Up").

If it is not, you need to do troubleshooting work, if it is running, you can move forward and reboot restund-1.

Now log into the restund server you wish to reboot (`restund-1` in this example), and reboot it

```sh
ssh -t <ip of restund-1> sudo reboot
```

Wait at least a minute for the machine to restart, you can use this command to automatically retry SSH access until it is succesful:

```sh
ssh -o 'ConnectionAttempts 3600' <ip of restund-1 node> exit
```

Then log into the restund server (`restund-1`, in this example), and make sure the docker service is running:

```sh
ssh -t <ip of restund-1> sudo docker container ls
```

```sh
CONTAINER ID         IMAGE                                COMMAND         STATUS        PORTS    NAMES
<random hash>        quay.io/wire/restund:v0.4.16b1.0.53  22 seconds ago  Up 18 seconds          restund
```

Here again, make sure you see a restund container, and it is running ("Up").

If it is, you have succesfully reboot the restund server, and can if you need to apply the same procedure to the other restund servers in your cluster.

## How to restart restund without having downtime

For maintenance you may need to restart a restund server.

1. Remove that restund server you want to restart from the list of advertised nodes, by taking it out of the turn server list that brig advertises:

Go to the place where you store kubernetes configuration for your wire-server installation. This might be a directory on your admin laptop, or a directory on the kubernetes machine.

If your override configuration (`values/wire-server/values.yaml`) looks like the following:

```yaml
# (...)

brig:
# (...)
  turnStatic:
    v1:
      # v1 entries can be ignored and are not in use anymore since end of 2018.
    v2:
    - turn:server1.example.com:3478 # server 1 UDP
    - turn:server1.example.com:3478?transport=tcp # server 1 TCP
    - turns:server1.example.com:5478?transport=tcp # server 1 TLS
    - turn:server2.example.com:3478 # server 2 UDP
    - turn:server2.example.com:3478?transport=tcp # server 2 TCP
    - turns:server2.example.com:5478?transport=tcp # server 2 TLS
```

And you want to remove server 1, then change the configuration to read

```yaml
turnStatic:
  v2:
    - turn:server2.example.com:3478 # server 2 UDP
    - turn:server2.example.com:3478?transport=tcp # server 2 TCP
    - turns:server2.example.com:5478?transport=tcp # server 2 TLS
```

(or comment out lines by adding a `#` in front of the respective line)

```yaml
turnStatic:
  v2:
  #- turn:server1.example.com:3478 # server 1 UDP
  #- turn:server1.example.com:3478?transport=tcp # server 1 TCP
  #- turns:server1.example.com:5478?transport=tcp # server 1 TLS
  - turn:server2.example.com:3478 # server 2 UDP
  - turn:server2.example.com:3478?transport=tcp # server 2 TCP
  - turns:server2.example.com:5478?transport=tcp # server 2 TLS
```

Next, apply these changes to configuration with `./bin/prod-setup.sh`

You then need to restart the `brig` pods if your code is older than September 2019 (otherwise brig will restart itself automatically):

```bash
kubectl delete pod -l app=brig
```

2. Wait for traffic to drain. This can take up to 12 hours after the configuration change. Wait until current allocations (people connected to the restund server) return 0. See {ref}`allocations`.
3. It's now safe to `systemctl stop restund`, and take any necessary actions.
4. `systemctl start restund` and then add the restund server back to configuration of advertised nodes (see step 1, put the server back).

## How to renew a certificate for restund

1. Replace the certificate file on the server (under `/etc/restund/restund.pem` usually), either with ansible or manually. Ensure the new certificate file is a concatenation of your whole certificate chain *and* the private key:

```text
-----BEGIN CERTIFICATE-----
...
-----END CERTIFICATE-----
-----BEGIN CERTIFICATE-----
...
-----END CERTIFICATE-----
-----BEGIN PRIVATE KEY-----
...
-----END PRIVATE KEY-----
```

2. Restart restund (see sections above)

## How to check which restund/TURN servers will be used by clients

The list of turn servers contacted by clients *should* match what you added to your `turnStatic` configuration. But if you'd like to double-check, here's how:

Terminal one:

```sh
kubectl port-forward svc/brig 9999:8080
```

Terminal two:

```sh
UUID=$(cat /proc/sys/kernel/random/uuid)
curl -s -H "Z-User:$UUID" -H "Z-Connection:anything" "http://localhost:9999/calls/config/v2" | json_pp
```

May return something like:

```json
{
   "ice_servers" : [
      {
         "credential" : "ASyFLXqbmg8fuK4chJG3S1Qg4L/nnhpkN0/UctdtTFbGW1AcuuAaOqUMDhm9V2w7zKHY6PPMqjhwKZ2neSE78g==",
         "urls" : [
            "turn:turn1.example.com:3478"
         ],
         "username" : "d=1582157904.v=1.k=0.t=s.r=mbzovplogqxbasbf"
      },
      {
         "credential" : "ZsxEtGWbpUZ3QWxPZtbX6g53HXu6PWfhhUfGNqRBJjrsly5w9IPAsuAWLEOP7fsoSXF13mgSPROXxMYAB/fQ6g==",
         "urls" : [
            "turn:turn1.example.com:3478?transport=tcp"
         ],
         "username" : "d=1582157904.v=1.k=0.t=s.r=jsafnwtgqhfqjvco"
      },
      {
         "credential" : "ZsxEtGWbpUZ3QWxPZtbX6g53HXu6PWfhhUfGNqRBJjrsly5w9IPAsuAWLEOP7fsoSXF13mgSPROXxMYAB/fQ6g==",
         "urls" : [
            "turns:turn1.example.com:5349?transport=tcp"
         ],
         "username" : "d=1582157904.v=1.k=0.t=s.r=jsafnwtgqhfqjvco"
      }
   ],
   "ttl" : 3600
}
```

In the above case, there is a single server configured to use UDP on port 3478, plain TCP on port 3478, and TLS over TCP on port 5349. The ordering of the list is random and will change on every request made with curl.
