(restarting-a-machine-in-a-kubernetes-cluster)=

# Restarting a machine in a Kubernetes cluster

```{note}
1. Know which kind of machine is going to be restarted

   > 1. control plane (api-server, controllers, etc.)
   > 2. node (runs actual workload, e.g. *Brig* or *Webapp*)
   > 3. *a* and *b* combined

2. The kind of machine in question must be deployed redundantly

3. Take out machines in a rolling fashion (sequentially, one at a time)
```

## Control plane

Depending on whether *etcd* is hosted on the same machine alongside the control plane (common practise), you need
to take its implications into account (see {ref}`How to rolling-restart an etcd cluster <how-to-rolling-restart-an-etcd-cluster>`)
when restarting a machine.

Regardless of where *etcd* is located, before turning off any machine that is part of the control plane, one should
{ref}`back up the cluster state <etcd-backup-and-restore>`.

If a part of the control plane does not run sufficiently redundant, it is advised to prevent any mutating interaction
during the procedure, until the cluster is healthy again.

```bash
kubectl get nodes
```

## Node

```{rubric} High-level steps:
```

1. Drain the node so that all workload is rescheduled on other nodes
2. Restart / Update / Decommission
3. Mark the node as being schedulable again (if not decommissioned)

*For more details please refer to the official documentation:* [Safely Drain a Node](https://kubernetes.io/docs/tasks/administer-cluster/safely-drain-node/)
