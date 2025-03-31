(install-sft)=

# Installing Conference Calling 2.0 (aka SFT)

## Background

Please refer to the following {ref}`section to better understand SFT and how it works <understand-sft>`.

### As part of the wire-server umbrella chart

`` sftd` `` will be installed as part of the `wire-server` umbrella chart if you set `tags.sftd: true`

In your `./values/wire-server/values.yaml` file you should set the following settings:

```yaml
tags:
  sftd: true

sftd:
  host: sftd.example.com # Replace example.com with your domain
  allowOrigin: webapp.example.com # Should be the address you used for the webapp deployment
```

In your `secrets.yaml` you should set the TLS keys for sftd domain:

```yaml
sftd:
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

### Standalone

The SFT component is also shipped as a separate helm chart. Installation is similar to installing
the charts as in {ref}`helm-prod`.

Some people might want to run SFT separately, because the deployment lifecycle for the SFT is a bit more intricate. For example,
if you want to avoid dropping calls during an upgrade, you'd set the `terminationGracePeriodSeconds` of the SFT to a high number, to wait
for calls to drain before updating to the new version (See  [technical documentation](https://github.com/wireapp/wire-server/blob/develop/charts/sftd/README.md)).  that would cause your otherwise snappy upgrade of the `wire-server` chart to now take a long time, as it waits for all
the SFT servers to drain. If this is a concern for you, we advice installing `sftd` as a separate chart.

It is important that you disable `sftd` in the `wire-server` umbrella chart, by setting this in your `./values/wire-server/values.yaml`  file

```yaml
tags:
  sftd: false
```

By default `sftd` doesn't need to set that many options, so we define them inline. However, you could of course also set these values in a `values.yaml` file.

SFT will deploy a Kubernetes Ingress on `$SFTD_HOST`.  Make sure that the domain name `$SFTD_HOST` points to your ingress IP as set up in {ref}`helm-prod`.  The SFT also needs to be made aware of the domain name of the webapp that you set up in {ref}`helm-prod` for setting up the appropriate CSP headers.

```shell
export SFTD_HOST=sftd.example.com
export WEBAPP_HOST=webapp.example.com
```

Now you can install the chart:

```shell
helm upgrade --install sftd wire/sftd --set
helm install sftd wire/sftd  \
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

(install-sft-firewall-rules)=

### Firewall rules

The SFT allocates media addresses in the UDP {ref}`default port range <port-ranges>`. Ingress and
egress traffic should be allowed for this range. Furthermore the SFT needs to be
able to reach the {ref}`Restund server <understand-restund>`, as it uses STUN and TURN in cases the client
can not directly connect to the SFT. In practise this means the SFT should
allow ingress and egress traffic on the UDP {ref}`default port range <port-ranges>` from and
to both, clients and {ref}`Restund servers <understand-restund>`.

*For more information on this port range, how to read and change it, and how to configure your firewall, please see* {ref}`this note <port-ranges>`.

The SFT also has an HTTP interface for initializing (allocation) or joining (signaling) a call. This is exposed through
the ingress controller as an HTTPS service.

SFT does require access to the TURN control port during process startup, but does not use it for any real purpose.
It is a health check, that is going to be removed eventually.

An SFT instance does **not** communicate with other SFT instances, TURN does talk to TURN.

Recapitulation table:

```{eval-rst}
+----------------------------+-------------+-------------+-----------+----------+-----------------------------------------------------------------------------+--------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Name                       | Origin      | Destination | Direction | Protocol | Ports                                                                       | Action (Policy)                      | Description                                                                                                                                                                                   |
+============================+=============+=============+===========+==========+=============================================================================+======================================+===============================================================================================================================================================================================+
| Denying all ingress        | Any / none. | Here        | Incoming  | All      | None                                                                        | Deny                                 | Deny anything incoming: by default nothing is allowed to come in, and we will individually allow specific ports below.                                                                        |
+----------------------------+-------------+-------------+-----------+----------+-----------------------------------------------------------------------------+--------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Allowing all egress        | Here        | Any         | Outgoing  | All      | All                                                                         | Allow                                | Allow everything outgoing, by default everything is allowed (all ports) in the outgoing direction.                                                                                            |
+----------------------------+-------------+-------------+-----------+----------+-----------------------------------------------------------------------------+--------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Allowing HTTP(S) ingress   | Any         | Here        | Incoming  | TCP      | sft_nginx_certbot_port, sft_nginx_sft_port, sft_nginx_metrics_port          | Allow                                | Allow HTTP(S) access to three different (nginx) confiigured ports. This interface is for initializing (allocation) or joining (signaling) a call.                                             |
+----------------------------+-------------+-------------+-----------+----------+-----------------------------------------------------------------------------+--------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Allowing SSH ingress       | Any         | Here        | Incoming  | TCP      | 22 (SSH)                                                                    | Allow                                | Allow Secure Shell Protocol access for administration.                                                                                                                                        |
+----------------------------+-------------+-------------+-----------+----------+-----------------------------------------------------------------------------+--------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Allowing SFT media ingress | Any         | Here        | Incoming  | UDP      | 32768-61000                                                                 | Allow                                | Allow ports in the "Ephemeral range" (https://en.wikipedia.org/wiki/Ephemeral_port), defined by the Linux Kernel ass the range from ports 32768 to 61000, used for UDP transmission of media. |
+----------------------------+-------------+-------------+-----------+----------+-----------------------------------------------------------------------------+--------------------------------------+                                                                                                                                                                                               |
| Allowing SFT media egress  | Here        | Anny        | Outgoing  | UDP      | 32768-61000                                                                 | Allow                                |                                                                                                                                                                                               |
+----------------------------+-------------+-------------+-----------+----------+-----------------------------------------------------------------------------+--------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
```

*For more information, please refer to the source code of the Ansible role:* [sft-server](https://github.com/wireapp/ansible-sft/blob/develop/roles/sft-server/tasks/traffic.yml).
