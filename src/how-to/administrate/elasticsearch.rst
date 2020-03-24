Elasticsearch
------------------------------

.. include:: includes/intro.rst

For more information, see the `elasticsearch
documentation <https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html>`__

See cluster health and cluster nodes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: sh

   curl 'http://localhost:9200/_cluster/health?pretty'
   curl 'http://localhost:9200/_cat/nodes?v&h=id,ip,name'

How to rolling-restart an elasticsearch cluster
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For maintenance you may need to restart the cluster.

On each server one by one:

1. check your cluster is healthy (see above)
2. stop shard allocation:

.. code:: sh

    ES_IP=<the-ip-of-the-elasticsearch-node-to-stop>
    curl -sSf -XPUT http://localhost:9200/_cluster/settings -H 'Content-Type: application/json' -d "{ \"transient\" : {\"cluster.routing.allocation.exclude._ip\": \"$ES_IP\" }}"; echo;

You should expect some output like this:

.. code:: sh

   {"acknowledged":true,"persistent":{},"transient":{"cluster":{"routing":{"allocation":{"exclude":{"_ip":"<SOME-IP-ADDRESS>"}}}}}}

3. Stop the elasticsearch daemon process: ``systemctl stop elasticsearch``
4. do any operation you need, if any
5. Start the elasticsearch daemon process: ``systemctl start elasticsearch``
6. re-enable shard allocation:

.. code:: sh

    curl -sSf -XPUT http://localhost:9200/_cluster/settings -H 'Content-Type: application/json' -d "{ \"transient\" : {\"cluster.routing.allocation.exclude._ip\": null }}"; echo;

You should expect some output like this from the above command:

.. code:: sh

   {"acknowledged":true,"persistent":{},"transient":{}}

6. Wait for your cluster to be healthy again.
7. Do the same on the next server.

How to manually look into what is stored in elasticsearch
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See also the elasticsearch sections in :ref:`investigative_tasks`.

Troubleshooting
~~~~~~~~~~~~~~~

Description:
**ES nodes ran out of disk space** and error message says: ``"blocked by: [FORBIDDEN/12/index read-only / allow delete (api)];"``

Solution:
* clean up disk (e.g. ``apt autoremove`` on all nodes), then restart machines and/or the elasticsearch process
* get the elastichsearch cluster out of *read-only* mode: SSH to one elasticsearch machine, then run ``curl -X PUT -H 'Content-Type: application/json' http://localhost:9200/_all/_settings -d '{"index.blocks.read_only_allow_delete": null}'``
* trigger reindexing: From a kubernetes machine, in one terminal: ``kubectl port-forward svc/brig 9999:8080``, and in a second terminal trigger the reindex: ``curl -v -X POST localhost:9999/i/index/reindex``
