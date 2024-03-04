# Classified Domains

(classified-domains)=

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
          domains: ["domain-that-is-classified.link", "some-other-classified-domain.link"]
          ...
```

```{note}
Note that when enabling this feature, it is important to provide your own domain too in the list of domains. 

In the example above, "domain-that-is-classified.link" and "some-other-classified-domain.link" are your domains.

This is not only a `backend` configuration, but also a `team` configuration/feature.

This means that different combinations of configurations will have different results.
```


Here is a table to navigate the possible configurations:

| Backend Config enabled/disabled | Backend Config Domains                         | Team Config enabled/disabled | Team Config Domains     | User's view                      |
| ------------------------------- | ---------------------------------------------- | ---------------------------- | ----------------------- | -------------------------------- |
| Enabled                         | \[domain1.example.com\]                        | Not configured               | Not configured          | Enabled, \[domain1.example.com\] |
| Enabled                         | \[domain1.example.com\]\[domain1.example.com\] | Enabled                      | Not configured          | Enabled, \[domain1.example.com\] |
| Enabled                         | \[domain1.example.com\]                        | Enabled                      | \[domain2.example.com\] | Enabled, Undefined               |
| Enabled                         | \[domain1.example.com\]                        | Disabled                     | Anything                | Undefined                        |
| Disabled                        | Anything                                       | Not configured               | Not configured          | Disabled, no domains             |
| Disabled                        | Anything                                       | Enabled                      | \[domain2.example.com\] | Undefined                        |

The table assumes the following:

- When backend level config says that this feature is enabled, it is illegal to not specify domains at the backend level.
- When backend level config says that this feature is disabled, the list of domains is ignored.
- When team level feature is disabled, the accompanying domains are ignored.

To disable, either omit the entry entirely (it is disabled by default), or provide the following:

```yaml
  classifiedDomains:
    status: disabled
    config:
      domains: []
```
