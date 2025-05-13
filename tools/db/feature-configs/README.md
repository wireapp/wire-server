# feature-configs

Allows doing DB operations on feature-configs. It is highly recommended to use
the `--dry-run** flag before doing any updates to prod-like environments.
**There is no rollback***

## Usage

Usage: feature-configs [--cassandra-host-galley HOST]
                       [--cassandra-port-galley PORT]
                       [--cassandra-keyspace-galley STRING] [--granularity INT]
                       (-f|--feature FEATURE) [--selector ARG] [--update ARG]
                       [--dry-run]

  DB Operations on feature-configs

Available options:
  -h,--help                Show this help text
  --cassandra-host-galley HOST
                           Cassandra Host for: galley (default: "localhost")
  --cassandra-port-galley PORT
                           Cassandra Port for: galley (default: 9042)
  --cassandra-keyspace-galley STRING
                           Cassandra Keyspace for: galley
                           (default: "galley_test")
  --granularity INT        Number of analysed teams for status report
                           (default: 10000)
  -f,--feature FEATURE     Name of the feature
  --selector ARG           Select configs based on their values like
                           'status=enabled', 'status=enabled &&
                           lockStatus=locked && config.defaultProtocol=1'
  --update ARG             Update configs, example: 'status=disabled', ''
  --dry-run                Do not apply the update, only print the new config
                           stats

### Selector Syntax

The configs can be selected using `--selector` parameter. It allows selecting by
`status`, `lockStatus` or `config`. `config` can be further selected into like
`config.supportedProtocols.0="proteus"`.
The selectors also allow `&&` and `||` operations. Examples:

`status=enabled`
`status=disaled && lockStatus=locked`
`status=enabled && config.defaultProtocol="proteus"`
`config.cipherSuites.0 > 1 && config.defaultCipherSuite < 2`

### Update Syntax

Updates can be specified using the `--update` paramter. It allows updating
`status`, `lockStatus` and `config`. `config` cannot be updated in bulk, instead
all the values must be specified individually. This is limiting, but should be
extended as necessary.

Examples:

`status=enabled`
`status=disabled, lockStatus=unlocked`
`config.cipherSuites.0=1 && config.cipherSuites.1=2`

#### Special note on Arrays in `config`

An array can be appended to or updated using the `--update` flag by specifying
the expected index of the last element. However, if the index is found to be out
of bounds, no update happens.

Example:
Consider that the selector returned three configs like this:
```json
{}
{"supportedProtocols": []}
{"supportedProtocols": [1,2]}
```

If we wanted to make all of these look like:
```json
{"supportedProtocols": [1,2,3]}
```

We must specify
`--update=config.supportedProtocols.0=1,config.supportedProtocols.1=2,config.supportedProtocols.2=3`.
Note how the key `supportedProtocols` doesn't exist, yet it gets created and so
does the array.

If we only specified `--update=config.supportedProtocols.2=3`, we'd get these configs:

```json
{}
{"supportedProtocols": []}
{"supportedProtocols": [1,2,3]}
```
Note how the first two configs don't change because index 2 doesn't make any sense for it.

