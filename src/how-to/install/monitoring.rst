.. _monitoring:

Monitoring wire-server using Prometheus and Grafana
=======================================================

Introduction
------------

The following instructions detail the installation of a monitoring
system consisting of a Prometheus instance and corresponding Alert
Manager in addition to a Grafana instance for viewing dashboards related
to cluster and wire-services health.

Prerequisites
-------------

You need to have wire-server installed, see either of

* :ref:`helm`
* :ref:`helm_prod`.

How to install Prometheus and Grafana on Kubernetes using Helm
---------------------------------------------------------------

.. note::

    The following makes use of overrides for helm charts. You may wish to read :ref:`understand-helm-overrides` first.

Create an override file:

.. code:: bash

    mkdir -p wire-server-metrics
    curl -sSL https://raw.githubusercontent.com/wireapp/wire-server-deploy/master/values/wire-server-metrics/demo-values.example.yaml > wire-server-metrics/values.yaml

And edit this file by editing/uncommenting as needed with respect to the next sections.

The monitoring system requires disk space if you wish to be resilient to
pod failure. This disk space is given to pods by using a so-called "Storage Class". You have three options:

* (1) If you deploy on a kubernetes cluster hosted on AWS you may install the ``aws-storage`` helm chart which provides configurations of Storage Classes for AWS's elastic block storage (EBS). For this, install the aws storage classes with ``helm upgrade --install aws-storage wire/aws-storage --wait``.
* (2) If you're not using AWS, but you sill want to have persistent metrics, see :ref:`using-custom-storage-classes`.
* (3) If you don't want persistence at all, see :ref:`using-no-storage-classes`.

Once you have a storage class configured (or put the override configuration to not use persistence), next we can install the monitoring suite itself.

There are a few known issues surrounding the ``prometheus-operator``
helm chart.

You will likely have to install the Custom Resource Definitions manually
before installing the ``wire-server-metrics`` chart:

::

   kubectl apply -f https://raw.githubusercontent.com/coreos/prometheus-operator/d34d70de61fe8e23bb21f6948993c510496a0b31/example/prometheus-operator-crd/alertmanager.crd.yaml
   kubectl apply -f https://raw.githubusercontent.com/coreos/prometheus-operator/d34d70de61fe8e23bb21f6948993c510496a0b31/example/prometheus-operator-crd/prometheus.crd.yaml
   kubectl apply -f https://raw.githubusercontent.com/coreos/prometheus-operator/d34d70de61fe8e23bb21f6948993c510496a0b31/example/prometheus-operator-crd/prometheusrule.crd.yaml
   kubectl apply -f https://raw.githubusercontent.com/coreos/prometheus-operator/d34d70de61fe8e23bb21f6948993c510496a0b31/example/prometheus-operator-crd/servicemonitor.crd.yaml

Now we can install the metrics chart, run the following::

   helm upgrade --install wire-server-metrics wire/wire-server-metrics --wait -f wire-server-metrics/values.yaml

See the `Prometheus Operator
README <https://github.com/helm/charts/tree/master/stable/prometheus-operator#work-arounds-for-known-issues>`__
for more information and troubleshooting help.

Adding Dashboards
-----------------

Grafana dashboard configurations are included as JSON inside the
``charts/wire-server-metrics/dashboards`` directory. You may import
these via Grafana's web UI. See `Accessing
grafana <#accessing-grafana>`__.

Monitoring in a separate namespace
----------------------------------

It is advisable to separate your monitoring services from your
application services. To accomplish this you may deploy
``wire-server-metrics`` into a separate namespace from ``wire-server``.
Simply provide a different namespace to the ``helm upgrade --install``
calls with ``--namespace your-desired-namespace``.

The wire-server-metrics chart will monitor all wire services across *all* namespaces.

Accessing grafana
-----------------

Forward a port from your localhost to the grafana service running in
your cluster:

::

   kubectl port-forward service/<release-name>-grafana 3000:80 -n <namespace>

Now you can access grafana at ``http://localhost:3000``

The username and password are stored in the ``grafana`` secret of your
namespace

By default this is:

-  username: ``admin``
-  password: ``admin``

Accessing prometheus
--------------------

Forward a port from your localhost to the prometheus service running in
your cluster:

::

   kubectl port-forward service/<release-name>-prometheus 9090:9090 -n <namespace>

Now you can access prometheus at ``http://localhost:9090``


Customization
---------------

.. _using-no-storage-classes:

Monitoring without persistent disk
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you wish to deploy monitoring without any persistent disk (not
recommended) you may add the following overrides to your ``values.yaml``
file.

.. code:: yaml

   # This configuration switches to use memory instead of disk for metrics services
   # NOTE: If the pods are killed you WILL lose all your metrics history
   prometheus-operator:
     grafana:
       persistence:
         enabled: false
     prometheusSpec:
       storageSpec: null
     alertmanager:
       alertmanagerSpec:
           storage: null

.. _using-custom-storage-classes:

Using Custom Storage Classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you're using a provider other than AWS please reference the
`Kubernetes documentation on storage
classes <https://kubernetes.io/docs/concepts/storage/storage-classes/>`__
for configuring a storage class for your kubernetes cluster.

If you wish to use a different storage class (for instance if you don't
run on AWS) you may add the following overrides to your ``values.yaml``
file.

.. code:: yaml

   prometheus-operator:
     grafana:
       persistence:
         storageClassName: "<my-storage-class>"
     prometheusSpec:
       storageSpec:
         volumeClaimTemplate:
           spec:
             storageClassName: "<my-storage-class>"
     alertmanager:
       alertmanagerSpec:
         storage:
           volumeClaimTemplate:
             spec:
               storageClassName: "<my-storage-class>"


Troubleshooting
---------------

"validation failed"
^^^^^^^^^^^^^^^^^^^^^

If you receive the following error:

::

   Error: validation failed: [unable to recognize "": no matches for kind "Alertmanager" in version
   "monitoring.coreos.com/v1", unable to recognize "": no matches for kind "Prometheus" in version
   "monitoring.coreos.com/v1", unable to recognize "": no matches for kind "PrometheusRule" in version

Please run the script to install Custom Resource Definitions which is
detailed in the installation instructions above.

"object is being deleted"
^^^^^^^^^^^^^^^^^^^^^^^^^^

When upgrading you may see the following error:

::

   Error: object is being deleted: customresourcedefinitions.apiextensions.k8s.io "prometheusrules.monitoring.coreos.com" already exists

Helm sometimes has trouble cleaning up or defining Custom Resource
Definitions. Try manually deleting the resource definitions and trying
your helm install again:

::

   kubectl delete customresourcedefinitions \
     alertmanagers.monitoring.coreos.com \
     prometheuses.monitoring.coreos.com \
     servicemonitors.monitoring.coreos.com \
     prometheusrules.monitoring.coreos.com
