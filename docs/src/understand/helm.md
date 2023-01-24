(understand-helm)=

# Understanding helm

See also the official [helm documentation](https://docs.helm.sh/). This page is meant to explain a few concepts directly relevant when installing wire-server helm charts.

(understand-helm-overrides)=

## Overriding helm configuration settings

### Default values

Default values are under a specific chart's `values.yaml` file, e.g. for the chart named `cassandra-ephemeral`, this file: [charts/cassandra-ephemeral/values.yaml](https://github.com/wireapp/wire-server/blob/develop/charts/cassandra-ephemeral/values.yaml). When you install or upgrade a chart, with e.g.:

```
helm upgrade --install my-cassandra wire/cassandra-ephemeral
```

Then the default values from above are used.

### Overriding

Overriding parts of the yaml configuration can be achieved by passing `-f path/to/override-file.yaml` when installing or upgrading a helm chart, like this:

Create file my-file.yaml:

```yaml
cassandra-ephemeral:
  resources:
    requests:
      cpu: "2"
```

Now you can install that chart with a custom value (using 2 cpu cores):

```
helm upgrade --install my-cassandra wire/cassandra-ephemeral -f my-values.yaml
```

### Sub charts

If a chart uses sub charts, there can be overrides in the parent
chart's `values.yaml` file, if namespaced to the sub chart.
Example: if chart `parent` includes chart `child`, and
`child`'s `values.yaml` has a default value `foo: bar`, and the
`parent` chart's `values.yaml` has a value

```yaml
child:
  foo: baz
```

then the value that will be used for `foo` by default is `baz` when you install the parent chart.

Note that if you `helm install parent` but wish to override values for `child`, you need to pass them as above, indented underneath `child:` as above.

### Multiple overrides

If `-f <filename>` is used multiple times, the last file wins in case keys exist
multiple times (there is no merge performed between multiple files passed to `-f`).
This can lead to unexpected results. If you use multiple files with `-f`, ensure they don't overlap.
