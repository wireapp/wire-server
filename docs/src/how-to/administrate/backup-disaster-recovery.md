# Backup and disaster recovery

## Introduction

While you might never use them, your backup plan (and the corresponding disaster recovery steps) are possibly your most important procedure.

This page explains in detail how to properly backup an on-premise Wire installation, and how to recover from your backup if you ever need to.

## Backing up

By nature, a significant part of a Wire installation is ephemeral, and not meant to be stored long-term or recovered: in case of trouble, those parts can just be started fresh with minimal impact on user experience. 

The exceptions to this rule, are what you want to back up. In particular:

* Your "wire-server" installation folder (used during the installation procedure)
* Your Cassandra database
* Your Minio data

If you save these, you can then whenever needed create a fresh install of Wire, re-import/re-install them on top of it, and get back to a working state.

Here is how to back up each:

### Wire-server

To backup the wire-server folder, we simply use ssh to read it from the server in which it is installed:

    ssh user@my-wire-server.mydomain.com 'cd /path/to/my/folder/for/wire-server/ && tar -cf - wire-server | gzip -9' > wire-server-backup.tar.gz

Where :

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `my-wire-server.mydomain.com` is the domain name or IP address for the server with your Wire install
* `/path/to/my/folder/for/wire-server/` is the (absolute) path, on the server, where your wire-server folder is located
* `wire-server` is the name of the folder for your wire-server install, typically (as per instructions) simply `wire-server`
* `wire-server-backup.tar.gz` is the name of the file in which your wire-server folder will be stored

Once the command is done executing, make sure the file exists and is not empty with:

    file wire-server-backup.tar.gz       # Should say the file type is a TAR archive
    ls -lh wire-server-backup.tar.gz     # Should show a file size other than 0
    tar tvf wire-server-backup.tar.gz    # Should list the files inside your wire-server folder correctly

Now simply save this file in multiple locations as per your normal company backup procedures, and repeat this procedure on a regular basis as is appropriate.

### Cassandra

Cassandra stores things such as user profiles/accounts, conversations, etc. It is the most critical data to backup/store/recover.

To backup your Cassandra database, do as follows:

You can read general information about connecting to your Cassandra node on [this page](/how-to/administrate/cassandra.html)

In particular, SSH into the Cassandra Virtual Machine with:

    ssh user@cassandra-vm.your-domain.com

Where:

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `cassandra-vm.mydomain.com` is the domain name or IP address for the server with your Cassandra node

Make sure (while connected via ssh) your Cassandra installation is doing well with:

    nodetool status

You should see something like:

You should see a list of nodes like this:

    Datacenter: datacenter1
    =======================
    Status=Up/Down
    |/ State=Normal/Leaving/Joining/Moving
    --  Address         Load       Tokens          Owns (effective)   Host ID                                Rack
    UN  192.168.220.13  9.51MiB    256             100.0%             3dba71c8-eea7-4e35-8f35-4386e7944894   rack1
    UN  192.168.220.23  9.53MiB    256             100.0%             3af56f1f-7685-4b5b-b73f-efdaa371e96e   rack1
    UN  192.168.220.33  9.55MiB    256             100.0%             RANDOMLY-MADE-UUID-GOES-INTHISPLACE!   rack1

As per the [Cassandra documentation](https://cassandra.apache.org/doc/latest/cassandra/operating/backups.html) to backup your database, you will use the `nodetool snapshot` command.

First we need to edit the `cassandra.yaml` file (which you can if needed find with `find -name cassandra.yaml`) has `auto_snapshot` set to `false`:

    auto_snapshot: false

Same with: `snapshot_before_compaction`

    snapshot_before_compaction: false

After editing the file, make sure you restart cassandra with:

    sudo service cassandra restart

You can find a list of all keyspaces by doing: building

    ls /mnt/cassandra/data/

Now (while connected via ssh, as per above), use nodetool to actually generate a snapshot of all tables:

    nodetool snapshot --tag catalog-ks catalogkeyspace

This should succesfully save the snapshots to the disk.

You should now be able to find your snapshots in the snapshots list with:

    nodetool listsnapshots

To actually save the files, you'll need to locate them with:

    find -name snapshots

Which should give you a list of paths to snapshots, such as:

    /mnt/cassandra/data/data/catalogkeyspace/journal-296a2d30c22a11e9b1350d927649052c/snapshots
    /mnt/cassandra/data/data/catalogkeyspace/magazine-446eae30c22a11e9b1350d927649052c/snapshots

Now to create a (local) backup of these snapshots, we use `ssh` the same way we did above for `wire-server`:

    ssh user@cassandra-vm.your-domain.com 'cd /mnt/cassandra/data/data/catalogkeyspace/journal-296a2d30c22a11e9b1350d927649052c/ && tar -cf - snapshots | gzip -9' > cassandra-journal-backup.tar.gz

Where :

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `cassandra-vm.your-domain.com` is the domain name or IP address for the server with your Wire install
* `/mnt/cassandra/` is the (absolute) path, on the server, where your cassandra folder is located, to which you add the location of the specific snapshot, found with `find` above

Repeat this for each of the snapshots.

Now simply save these files in multiple locations as per your normal company backup procedures, and repeat this procedure on a regular basis as is appropriate.

### MinIO

MinIO emulates an Amazon-S3-compatible file-storage setup, and is used by Wire to store things such as file attachements, images etc.

If your specific installation is using the actual Amazon file storage (and not a local emulated alternative), you should skip this section (but still actually backup whatever you are using).

Similarly to Cassandra, to create a backup you need to SSH into the Virtual Machine running MinIO in your installation:

You can read general information about your MinIO node on [this page](/how-to/administrate/minio.html)

SSH into the MinIO Virtual Machine with:

    ssh user@minio-vm.your-domain.com

Where:

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `minio-vm.mydomain.com` is the domain name or IP address for the server with your MinIO node

To backup the MinIO data, we need to backup two servers over SSH, the same way we did for Cassandra and wire-server:

    ssh user@my-minio-server.mydomain.com 'cd /var/lib/ && tar -cf - minio-server1 | gzip -9' > minio-server1-backup.tar.gz
    ssh user@my-minio-server.mydomain.com 'cd /var/lib/ && tar -cf - minio-server2 | gzip -9' > minio-server2-backup.tar.gz

Where:

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `my-minio-server.mydomain.com` is the domain name or IP address for the MinIO Virtual Machine
* `minio-server[number]-backup.tar.gz` is the name of the file in which each minio data folder will be stored

## Recovery procedure