# Restund

`restund` is used for audio and video calls.

This folder contains extra modules on top of [restund](https://github.com/wireapp/restund), as well as some build instructions.

## License

* The actual [restund](https://github.com/wireapp/restund) is under [BSD](https://github.com/wireapp/restund/blob/master/docs/COPYING)
* This folder's extra modules, like the rest of wire-server, are under AGPL, see [LICENSE](LICENSE)

## Building

The preferred way of building `restund` is via Docker, which
encodes all build- and runtime dependencies. The only prerequisites on the build
machine are thus `docker` and `make`.

```shell
make build
```

It's possible to bypass `docker`, in which case the host must have all
build-time dependencies installed.

```shell
make build DOCKER=false
```

## Creating ACIs for use with rkt

If you'd like to create an [ACI](https://github.com/appc/spec/blob/master/spec/aci.md) for use with [rkt](https://github.com/rkt/rkt),
apart from`docker` and `make`, you'll need [docker2aci](https://github.com/appc/docker2aci) and
[actool](https://github.com/appc/spec/tree/master/actool) installed on the build
machine. This also implies having the Go language toolchain installed, unless
you can get hold of the binaries by some other means. Additionally, `gpg` needs to be installed and set
up in order to create signatures for the ACIs.

```shell
export PATH=$GOPATH/bin:$PATH
go get -u github.com/appc/docker2aci
go get -u github.com/appc/spec/actool
```

> Note: the versions used as of this writing are:
>
> ```shell
> local:~$ docker2aci --version
> docker2aci version 0.16.0+git
> appc version 0.8.10
> local:~$ actool version
> actool version 0.8.10+git
> ```
>
> You may want to upgrade these tools from time to time by re-executing
> the above `go get` commands.

Create an `.aci` and signature:

```shell
make publish
```

Before releasing, you may want to increment the version(s): `restund` is
versioned according to its upstream version, plus a "wire version" set in the
Makefile.

## Running restund on a server

You need

* the aci image built in the section above, or, alternatively, a natively compiled `restund` binary with all the shared libraries available.
* an adapted restund config (see below)
* (optionally) a TLS certificate chain in PEM format, including the private key

Example config file:

```conf
# /etc/restund/restund.conf

# core
daemon                  no
debug                   no
realm                   dummy.io
syncinterval            600
udp_listen              {{ ansible_default_ipv4.address }}:3478
udp_sockbuf_size        524288
tcp_listen              {{ ansible_default_ipv4.address }}:3478
# tls_listen is optional, you can comment that line out. If set, you must provide a valid TLS certificate for the domain name you're advertising.
# tls_listen              {{ ansible_default_ipv4.address }}:5349,/usr/local/etc/restund/restund.pem

# modules
module_path             /usr/local/lib/restund/modules
module                  stat.so
module                  drain.so
module                  binding.so
module                  turn.so
module                  status.so
# The auth and zrest modules are optional. If enabled, ensure the zrest_secret below is set to a value shared with the configuration in brig.
module                  zrest.so
module                  auth.so

# auth
auth_nonce_expiry       3600

# turn
turn_max_allocations    64000
turn_max_lifetime       3600
turn_relay_addr         {{ ansible_default_ipv4.address }}

# You generally don't need to set this (only if your server is on a private network and must be reachable from other restund servers that are on another network):
# turn_public_addr is an IP which must be reachable for UDP traffic from other restund servers (and from this server itself). If unset, defaults to 'turn_relay_addr'
#turn_public_addr        {{ public_ipv4 }}

# syslog
syslog_facility         24

# status
status_udp_addr         127.0.0.1
status_udp_port         33000
status_http_addr        127.0.0.1
status_http_port        8080

# zrest (shared secret shared with brig, optional)
zrest_secret            {{ restund_zrest_secret }}
```

Adjust the above configuration:

* Replace the `{{ variables }}` with real values (without `{{`)):
    * Put your private IP of the server in place of: `{{ ansible_default_ipv4.address }}`.
* You may comment these out in case you don't want to use authentication:
```
module                  zrest.so
module                  auth.so
zrest_secret            {{ restund_zrest_secret }}
```

Next, list out TURN IP and port in `deploy/services-demo/resources/turn/servers.txt`, and `deploy/services-demo/resources/turn/servers-v2.txt`, as given below:
`turn:<private-ip>:3478`
Then run the command restund command and you'll get the live stun log in your terminal.

Running restund with `rkt`:

```
/usr/bin/rkt run \
    --net=host \
    --dns=host \
    --hosts-entry=host \
    --volume volume-usr-local-etc-restund,kind=host,source=/etc/restund,readOnly=true \
    {{ aci_base_url }}/restund/restund-{{ versions.restund }}_linux_amd64.aci \
    --user=restund \
    --group=restund
```
