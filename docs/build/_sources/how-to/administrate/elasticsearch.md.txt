# Elasticsearch

```{eval-rst}
.. include:: includes/intro.rst
```

For more information, see the [elasticsearch
documentation](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)

(restart-elasticsearch)=

## How to rolling-restart an elasticsearch cluster

For maintenance you may need to restart the cluster.

On each server one by one:

1. check your cluster is healthy (see above)
2. stop shard allocation:

```sh
ES_IP=<the-ip-of-the-elasticsearch-node-to-stop>
curl -sSf -XPUT http://localhost:9200/_cluster/settings -H 'Content-Type: application/json' -d "{ \"transient\" : {\"cluster.routing.allocation.exclude._ip\": \"$ES_IP\" }}"; echo;
```

You should expect some output like this:

```sh
{"acknowledged":true,"persistent":{},"transient":{"cluster":{"routing":{"allocation":{"exclude":{"_ip":"<SOME-IP-ADDRESS>"}}}}}}
```

3. Stop the elasticsearch daemon process: `systemctl stop elasticsearch`
4. do any operation you need, if any
5. Start the elasticsearch daemon process: `systemctl start elasticsearch`
6. re-enable shard allocation:

```sh
curl -sSf -XPUT http://localhost:9200/_cluster/settings -H 'Content-Type: application/json' -d "{ \"transient\" : {\"cluster.routing.allocation.exclude._ip\": null }}"; echo;
```

You should expect some output like this from the above command:

```sh
{"acknowledged":true,"persistent":{},"transient":{}}
```

6. Wait for your cluster to be healthy again.
7. Do the same on the next server.

## How to manually look into what is stored in elasticsearch

See also the elasticsearch sections in {ref}`investigative-tasks`.

(check-the-health-of-an-elasticsearch-node)=

## Check the health of an elasticsearch node

To check the health of an elasticsearch node, run the following command:

```sh
ssh <ip of elasticsearch node> curl localhost:9200/_cat/health
```

You should see output looking like this:

```
1630250355 15:18:55 elasticsearch-directory green 3 3 17 6 0 0 0 - 100.0%
```

Here, the `green` denotes good node health, and the `3 3` denotes 3 running nodes.

## Check cluster health

This is the command to check the health of the entire cluster:

```sh
ssh <ip of elasticsearch node> curl 'http://localhost:9200/_cluster/health?pretty'
```

## List cluster nodes

This is the command to list the nodes in the cluster:

```sh
ssh <ip of elasticsearch node> curl 'http://localhost:9200/_cat/nodes?v&h=id,ip,name'
```

## Troubleshooting

Description:
**ES nodes ran out of disk space** and error message says: `"blocked by: [FORBIDDEN/12/index read-only / allow delete (api)];"`

Solution:

1. Connect to the node:

```sh
ssh <ip of elasticsearch node>
```

2. Clean up disk (e.g. `apt autoremove` on all nodes), then restart machines and/or the elasticsearch process

```sh
sudo apt autoremove
sudo reboot
```

As always make sure you {ref}`check the health of the process <check-the-health-of-an-elasticsearch-node>`. before and after the reboot.

3. Get the elastichsearch cluster out of *read-only* mode, run:

```sh
curl -X PUT -H 'Content-Type: application/json' http://localhost:9200/_all/_settings -d '{"index.blocks.read_only_allow_delete": null}'
```

4. Trigger reindexing: From a kubernetes machine, in one terminal:

```sh
# The following depends on your namespace where you installed wire-server. By default the namespace is called 'wire'.
kubectl --namespace wire port-forward svc/brig 9999:8080
```

And in a second terminal trigger the reindex:

```sh
curl -v -X POST localhost:9999/i/index/reindex
```
