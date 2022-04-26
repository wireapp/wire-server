
+-------------------------+--------+--------------+--------------+-------------------+
| Name                    | Amount | CPU Cores    | Memory (GB)  | Disk Space (GB)   |
+=========================+========+==============+==============+===================+
| Cassandra               | 3      | 2            | 4            | 80                |
+-------------------------+--------+--------------+--------------+-------------------+
| MinIO                   | 3      | 1            | 2            | 400               |
+-------------------------+--------+--------------+--------------+-------------------+
| ElasticSearch           | 3      | 1            | 2            | 60                |
+-------------------------+--------+--------------+--------------+-------------------+
| Kubernetes³             | 3      | 6¹           | 8            | 40                |
+-------------------------+--------+--------------+--------------+-------------------+
| Restund⁴                | 2      | 1            | 2            | 10                |
+-------------------------+--------+--------------+--------------+-------------------+
| **Per-Server Totals**   | ---    | 11 CPU Cores | 18 GB Memory | 590 GB Disk Space |
+-------------------------+--------+--------------+--------------+-------------------+
| Admin Host²             | 1      | 1            | 4            | 40                |
+-------------------------+--------+--------------+--------------+-------------------+
| Asset Host²             | 1      | 1            | 4            | 100               |
+-------------------------+--------+--------------+--------------+-------------------+
| **Per-Server Totals with| ---    | 13 CPU Cores | 26 GB Memory | 730 GB Disk Space |
| Admin and Asset Hosts** |        |              |              |                   |
+-------------------------+--------+--------------+--------------+-------------------+

- ¹ Kubernetes hosts may need more ressources to support SFT (Conference Calling). See "Conference Calling Hardware Requirements" below.
- ² Admin and Asset Hosts can run on any one of the 3 servers, but that server must not allocate additional resources as indicated in the table above.
- ³ Etcd is run inside of Kubernetes, hence no specific resource allocation
- ⁴ Restund may be hosted on only 2 of the 3 servers, or all 3. Two nodes are enough to ensure high availability of Restund services

General Hardware Requirements

- Minimum 3 physical servers required
- Wire has a minimum requirement for a total of 16 Ubuntu 18.04 virtual machines across the 3 servers (in accordance with the table above)

Conference Calling Hardware Requirements

- Kubernetes Hosts may need additional resources for SFT services. For concurrent SFT users (SFT = Selective Forwarding Turn-server, ie. Conference calling), we recommend an extra 3% of CPU allocation, evenly distributed across the nodes (i.e. 1% more CPU per kubernetes server). So for every 100 users plan on adding one CPU core on each Kubernetes node. The SFT component runs inside of Kubernetes, and does not require a separate virtual machine for operation.

