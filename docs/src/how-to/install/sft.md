<a id="install-sft"></a>

# Installing Conference Calling 2.0 (aka SFT)

## Background

Please refer to the following [section to better understand SFT and how it works](../../understand/sft.md#understand-sft).

### As part of the wire-server umbrella chart

The `sftd` is packaged as its own Helm chart.

In your `./values/sftd/values.yaml` file you should set the following settings:

```yaml
host: sftd.example.com # Replace example.com with your domain
allowOrigin: https://webapp.example.com # Should be the address you used for the webapp deployment (Note: you must include the uri scheme "https://")
```

In your `secrets.yaml` you should set the TLS keys for sftd domain:

```yaml
tls:
  crt: |
    <TLS CRT HERE>
  key: |
    <TLS KEY HERE>
```

You should also make sure that you configure brig to know about the SFT server in your `./values/wire-server/values.yaml`  file:

```yaml
brig:
  optSettings:
    setSftStaticUrl: "https://sftd.example.com:443"
```

Now you can deploy as usual:

```shell
helm upgrade wire-server wire/wire-server --values ./values/wire-server/values.yaml
```

By default `sftd` doesn’t need to set that many options, so we define them inline. However, you could of course also set these values in a `values.yaml` file.

SFT will deploy a Kubernetes Ingress on `$SFTD_HOST`.  Make sure that the domain name `$SFTD_HOST` points to your ingress IP as set up in [Installing wire-server (production) components using Helm](helm-prod.md#helm-prod).  The SFT also needs to be made aware of the domain name of the webapp that you set up in [Installing wire-server (production) components using Helm](helm-prod.md#helm-prod) for setting up the appropriate CSP headers.

```shell
export SFTD_HOST=sftd.example.com
export WEBAPP_HOST=webapp.example.com
```

Now you can install the chart:

```shell
helm install sftd sftd  \
  --set host=$SFTD_HOST \
  --set allowOrigin=https://$WEBAPP_HOST \
  --set-file tls.crt=/path/to/tls.crt \
  --set-file tls.key=/path/to/tls.key
```

You should also make sure that you configure brig to know about the SFT server, in the `./values/wire-server/values.yaml` file:

```yaml
brig:
  optSettings:
    setSftStaticUrl: "https://sftd.example.com:443"
```

And then roll-out the change to the `wire-server` chart

```shell
helm upgrade wire-server wire/wire-server --values ./values/wire-server/values.yaml
```

For more advanced setups please refer to the [technical documentation](https://github.com/wireapp/wire-server/blob/develop/charts/sftd/README.md).

<a id="install-sft-firewall-rules"></a>

### Firewall rules

The SFT allocates media addresses in the UDP [default port range](../../understand/notes/port-ranges.md#port-ranges). Ingress and
egress traffic should be allowed for this range. Furthermore the SFT needs to be
able to reach the [Restund server](../../understand/restund.md#understand-restund), as it uses STUN and TURN in cases the client
can not directly connect to the SFT. In practise this means the SFT should
allow ingress and egress traffic on the UDP [default port range](../../understand/notes/port-ranges.md#port-ranges) from and
to both, clients and [Restund servers](../../understand/restund.md#understand-restund).

*For more information on this port range, how to read and change it, and how to configure your firewall, please see* [this note](../../understand/notes/port-ranges.md#port-ranges).

The SFT also has an HTTP interface for initializing (allocation) or joining (signaling) a call. This is exposed through
the ingress controller as an HTTPS service.

SFT does require access to the TURN control port during process startup, but does not use it for any real purpose.
It is a health check, that is going to be removed eventually.

An SFT instance does **not** communicate with other SFT instances, TURN does talk to TURN.

Recapitulation table:

| Name                       | Origin      | Destination   | Direction   | Protocol   | Ports                                                              | Action (Policy)   | Description                                                                                                                                                                                                                                   |
|----------------------------|-------------|---------------|-------------|------------|--------------------------------------------------------------------|-------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Denying all ingress        | Any / none. | Here          | Incoming    | All        | None                                                               | Deny              | Deny anything incoming: by default nothing is allowed to come in, and we will individually allow specific ports below.                                                                                                                        |
| Allowing all egress        | Here        | Any           | Outgoing    | All        | All                                                                | Allow             | Allow everything outgoing, by default everything is allowed (all ports) in the outgoing direction.                                                                                                                                            |
| Allowing HTTP(S) ingress   | Any         | Here          | Incoming    | TCP        | sft_nginx_certbot_port, sft_nginx_sft_port, sft_nginx_metrics_port | Allow             | Allow HTTP(S) access to three different (nginx) confiigured ports. This interface is for initializing (allocation) or joining (signaling) a call.                                                                                             |
| Allowing SSH ingress       | Any         | Here          | Incoming    | TCP        | 22 (SSH)                                                           | Allow             | Allow Secure Shell Protocol access for administration.                                                                                                                                                                                        |
| Allowing SFT media ingress | Any         | Here          | Incoming    | UDP        | 32768-61000                                                        | Allow             | Allow ports in the “Ephemeral range” ([https://en.wikipedia.org/wiki/Ephemeral_port](https://en.wikipedia.org/wiki/Ephemeral_port)), defined by the Linux Kernel ass the range from ports 32768 to 61000, used for UDP transmission of media. |
| Allowing SFT media egress  | Here        | Any           | Outgoing    | UDP        | 32768-61000                                                        | Allow             |                                                                                                                                                                                                                                               |
| Federation traffic in      | Any         | Here          | Incoming    | UDP/DTLS   | 9191                                                               | Allow             | The TURN-servers communicate via this port. Either encrypted or unencrypted.                                                                                                                                                                  |
| Federation traffic out     | Here        | Any           | Outgoing    | UDP/DTLS   | 9191                                                               | Allow             |                                                                                                                                                                                                                                               |
| Coturn control in          | Any         | Here          | Incoming    | TCP        | 3478                                                               | Allow             | (STUN and TURN (TCP), helm setting: coturn:coturnTurnListenPort)                                                                                                                                                                              |
| Coturn control in (TLS)    | Any         | Here          | Incoming    | TCP/TLS    | 3478                                                               | Allow             | (STUN and TURN (TLS via TCP), helm setting: coturn:coturnTurnTlsListenPort)                                                                                                                                                                   |
| Coturn control in (UDP)    | Any         | Here          | Incoming    | UDP        | 3478                                                               | Allow             | (STUN and TURN (UDP), helm setting: coturn:coturnTurnListenPort)                                                                                                                                                                              |

*For more information, please refer to the source code of the Ansible role:* [sft-server](https://github.com/wireapp/ansible-sft/blob/develop/roles/sft-server/tasks/traffic.yml).
