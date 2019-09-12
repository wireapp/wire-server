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

You need a restund config and pem file (for the following assumed to be under /etc/restund), and can then run the aci image with rkt.

A config file could look like the following (with the ansible-style variables filled in accordingly):

```
# core
daemon                  no
debug                   no
realm                   dummy.io
syncinterval            600
udp_listen              {{ ansible_default_ipv4.address }}:{{ restund_udp_listen_port }}
udp_sockbuf_size        524288
tcp_listen              {{ ansible_default_ipv4.address }}:{{ restund_tcp_listen_port }}
tls_listen              {{ ansible_default_ipv4.address }}:{{ restund_tls_listen_port }},/usr/local/etc/restund/restund.pem

# modules
module_path             /usr/local/lib/restund/modules
module                  stat.so
module                  drain.so
module                  binding.so
module                  auth.so
module                  turn.so
module                  zrest.so
module                  status.so

# auth
auth_nonce_expiry       3600

# turn
turn_max_allocations    64000
turn_max_lifetime       3600
turn_relay_addr         {{ ansible_default_ipv4.address }}
{% if ansible_default_ipv4.address != public_ipv4 %}
turn_public_addr        {{ public_ipv4 }}
{% endif %}

# syslog
syslog_facility         24

# status
status_udp_addr         127.0.0.1
status_udp_port         {{ restund_udp_status_port }}
status_http_addr        127.0.0.1
status_http_port        {{ restund_http_status_port }}

# zrest (shared secret shared with brig)
zrest_secret            {{ restund_zrest_secret }}
```

Example rkt command:

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

In case You have set up restund without docker, you just need to make some of these changes:

Put your private IP of the server in place of: `{{ ansible_default_ipv4.address }}`. And replace restund listen ports with `3478`, for both UDP and TCP.

You may comment these out in case you don't want to use:
```
module                  zrest.so
module                  auth.so
zrest_secret            {{ restund_zrest_secret }}
```
It'll help in running the TURN server without interuption or further configuration for testing purpose. List out TURN IP and port in `deploy/services-demo/resources/turn/servers.txt`, and `deploy/services-demo/resources/turn/servers-v2.txt`, as given below:
`turn:<private-ip>:3478`
Then run the command restund command and You'll get the live stun log in your terminal.