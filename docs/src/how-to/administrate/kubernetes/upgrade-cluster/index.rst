Upgrading a Kubernetes cluster
==============================

Before upgrading Kubernetes, a couple of aspects should be taken into account:

* downtime is (not) permitted
* stateful backing services that run outside or on top of Kubernetes

As a result the following questions arise:

1. Is an in-place upgrade required (reuse existing machines) or is it possible to
   deploy a second cluster right next to the first one and install Wire on top?
2. How was the Kubernetes cluster deployed?

Depending on the deployment method, the upgrade procedure may vary. It may be reasonable to test
the upgrade in a non-production environment first.
Regardless of the deployment method, it is recommended to :ref:`back up the cluster state
<etcd_backup-and-restore>` before starting to upgrade the cluster. Additional background knowledge
can be found in the section about :ref:`restarting a machine in an kubernetes cluster <restarting-a-machine-in-a-kubernetes-cluster>`.


.. warning::

    For an in-place upgrade, it is *NOT* recommended to go straight to the latest Kubernetes
    version. Instead, one should upgrade step by step between each minor version.


Manually
~~~~~~~~

Doing an upgrade by hand is cumbersome and error-prone, which is why there are tools and
automation for this procedure. The high-level steps would be:

1. upgrade the control plane (also see a more detailed `list <https://kubernetes.io/docs/tasks/administer-cluster/cluster-upgrade/#manual-deployments>`__)
    a) all *etcd* instances
    b) api-server on each control-plane host
    c) controllers, scheduler,
2. upgrade the nodes (order may vary, depending on whether the kube-components run in containers)
    * kubelet
    * kube-proxy
    * container runtime
3. then upgrade the clients (``kubectl``, e.g. on workstations or in pipelines)

*For more details, please refer to the official documentation:*
`Upgrade A Cluster <https://kubernetes.io/docs/tasks/administer-cluster/cluster-upgrade/>`__


Kubespray (Ansible)
~~~~~~~~~~~~~~~~~~~

Kubespray comes with a dedicated playbook that should be used to perform the upgrade:
``upgrade-cluster.yml``. Before running the playbook, make sure that the right Kubespray version
is being used. Each Kubespray version supports only a small and sliding window of Kubernetes
versions (check ``kube_version`` & ``kube_version_min_required`` in ``roles/kubespray-defaults/defaults/main.yaml``
for a given `release version tag <https://github.com/kubernetes-sigs/kubespray/releases>`__).

The commands may look similar to this example (assuming Kubernetes v1.18 version installed
with Kubespray 2.14):

.. code:: bash

    git clone https://github.com/kubernetes-sigs/kubespray
    cd kubespray
    git checkout release-2.15
    ${EDITOR} roles/kubespray-defaults/defaults/main.yaml

    ansible-playbook -i ./../path/my/inventory-dir -e kube_version=v1.19.7 ./upgrade-cluster.yml

.. TODO: adjust the example showing how to run this with wire-server-deploy a/o the offline toolchain container image
.. TODO: add ref to the part of this documentation that talks about the air-gapped installation

Kubespray takes care of bringing the new binaries into position on each machine, restarting
the components, and draining/uncordon nodes.

*For more details please refer to the official documentation:*
`Upgrading Kubernetes in Kubespray <https://kubespray.io/#/docs/upgrades>`__


Kubeadm
~~~~~~~

Please refer to the *official documentation:* `Upgrading kubeadm clusters <https://kubernetes.io/docs/tasks/administer-cluster/kubeadm/kubeadm-upgrade/>`__
