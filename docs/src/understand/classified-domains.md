# Classified Domains

As a backend administrator, if you want to control which other backends (identified by their domain) are "classified",

change the following `galley` configuration in the `value.yaml.gotmpl` file of the wire-server chart:

```yaml
galley:
  replicaCount: 1
  config:
  ...
    featureFlags:
    ...
      classifiedDomains:
        status: enabled
        config:
          domains: ["domain-that-is-classified.link"]
          ...
```

This is not only a `backend` configuration, but also a `team` configuration/feature.

This means that different combinations of configurations will have different results.

Here is a table to navigate the possible configurations:

| Backend Config enabled/disabled | Backend Config Domains                         | Team Config enabled/disabled | Team Config Domains     | User's view                      |
| ------------------------------- | ---------------------------------------------- | ---------------------------- | ----------------------- | -------------------------------- |
| Enabled                         | \[domain1.wire.example\]                        | Not configured               | Not configured          | Enabled, \[domain1.wire.example\] |
| Enabled                         | \[domain1.wire.example\]\[domain1.wire.example\] | Enabled                      | Not configured          | Enabled, \[domain1.wire.example\] |
| Enabled                         | \[domain1.wire.example\]                        | Enabled                      | \[domain2.wire.example\] | Enabled, Undefined               |
| Enabled                         | \[domain1.wire.example\]                        | Disabled                     | Anything                | Undefined                        |
| Disabled                        | Anything                                       | Not configured               | Not configured          | Disabled, no domains             |
| Disabled                        | Anything                                       | Enabled                      | \[domain2.wire.example\] | Undefined                        |

The table assumes the following:

- When backend level config says that this feature is enabled, it is illegal to not specify domains at the backend level.
- When backend level config says that this feature is disabled, the list of domains is ignored.
- When team level feature is disabled, the accompanying domains are ignored.
