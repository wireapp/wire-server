# Backup and disaster recovery

## Introduction

While you might never use them, your backup plan (and the corresponding disaster recovery steps) are possibly your most important procedure.

You should:

1. Write it up fully
2. Test it from beginning to end, simulating an actual disaster
3. Run backups on a regular basis, ideally automatically

This page explains in detail how to properly backup an on-premise Wire installation, and how to recover from your backup if you ever need to.

Note that you should not trust this page (or your execution of it) to allow for a proper backup and restore, therefore, you should immediately (before it becomes critical to do so) back up your Wire installation **and** attempt (as a test) to restore it. This will ensure that you can **in fact** backup and restore Wire, and will catch any problem in the process before any problem becomes damaging to you.

## Backing up

By nature, a significant part of a Wire installation is ephemeral, and not meant to be stored long-term or recovered: in case of trouble, those parts can just be started fresh with minimal impact on user experience. 

The exceptions to this rule, are what you want to back up. In particular:

* Your "wire-server" installation folder (used during the installation procedure)
* Your Cassandra database
* Your Minio data

If you save these, you can then whenever needed, create a fresh installation of Wire, re-import/re-install them on top of it, and get back to a working state.

Here is how to back up each:

### Backing up Wire-server

To backup the wire-server folder, we simply use ssh to read it from the server in which it is installed:

    ssh user@my-wire-server.your-domain.com 'cd /path/to/my/folder/for/wire-server/ && tar -cf - wire-server | gzip -9' > wire-server-backup.tar.gz

Where :

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `my-wire-server.your-domain.com` is the domain name or IP address for the server with your Wire install
* `/path/to/my/folder/for/wire-server/` is the (absolute) path, on the server, where your wire-server folder is located
* `wire-server` is the name of the folder for your wire-server install, typically (as per instructions) simply `wire-server`
* `wire-server-backup.tar.gz` is the name of the file in which your wire-server folder will be stored

Once the command is done executing, make sure the file exists and is not empty with:

    file wire-server-backup.tar.gz       # Should say the file type is a TAR archive
    ls -lh wire-server-backup.tar.gz     # Should show a file size other than 0
    tar tvf wire-server-backup.tar.gz    # Should list the files inside your wire-server folder correctly

Now simply save this file in multiple locations as per your normal company backup procedures, and repeat this procedure on a regular basis as is appropriate.

### Backing up Cassandra

Cassandra stores things such as user profiles/accounts, conversations, etc. It is the most critical data to backup/store/recover.

To backup your Cassandra database, do as follows:

You can read general information about connecting to your Cassandra node on [this page](/how-to/administrate/cassandra)

In particular, SSH into the Cassandra Virtual Machine with:

    ssh user@cassandra-vm.your-domain.com

Where:

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `cassandra-vm.your-domain.com` is the domain name or IP address for the server with your Cassandra node

Make sure (while connected via ssh) your Cassandra installation is doing well with:

    nodetool status

or (in newer versions)

    nodetool -h ::FFFF:127.0.0.1 status

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

You can find a list of all keyspaces by doing:

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

    ssh user@cassandra-vm.your-domain.com 'cd /mnt/cassandra/data/data/catalogkeyspace/journal-296a2d30c22a11e9b1350d927649052c/ && tar -cf - snapshots | gzip -9' > cassandra-catalogkeyspace-journal-backup.tar.gz

Where :

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `cassandra-vm.your-domain.com` is the domain name or IP address for the server with your Wire install
* `/mnt/cassandra/` is the (absolute) path, on the server, where your cassandra folder is located, to which you add the location of the specific snapshot, found with `find` above

Repeat this for each of the snapshots.

Now simply save these files in multiple locations as per your normal company backup procedures, and repeat this procedure on a regular basis as is appropriate.

### Batch Cassandra backup

The backup procedure above presumes manually backing up each Cassandra snapshot serially one by one.

If you want to back all files at once, you can use the `find` command to find all snapshots and pack them into a single archive:

    ssh user@cassandra-vm.your-domain.com 'find /mnt/cassandra/ -name "snapshots" -print0 | tar -cvf - --null -T - | gzip -9 ' > cassandra-batch.tar.gz

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `cassandra-vm.your-domain.com` is the domain name or IP address for the server with your Wire install
* `/mnt/cassandra/` is the (absolute) path, on the server, where your cassandra folder is located, to which you add the location of the specific snapshot, found with `find` above
* `cassandra-batch.tar.gz` is the local file everything will be written to

This will (over ssh) find all `snapshot` folders in the `cassandra` folder, pack them into a tar file, gzip it to save space, and output it to a local file.

### Backing up MinIO

MinIO emulates an Amazon-S3-compatible file-storage setup, and is used by Wire to store things such as file attachments, images etc.

If your specific installation is using the actual Amazon file storage (and not a local emulated alternative), you should skip this section (but still actually backup whatever you are using).

Similarly to Cassandra, to create a backup you need to SSH into the Virtual Machine running MinIO in your installation:

You can read general information about your MinIO node on [this page](/how-to/administrate/minio).

SSH into the MinIO Virtual Machine with:

    ssh user@minio-vm.your-domain.com

Where:

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `minio-vm.your-domain.com` is the domain name or IP address for the server with your MinIO node

To backup the MinIO data, we need to backup two servers over SSH, the same way we did for Cassandra and wire-server:

    ssh user@my-minio-server.your-domain.com 'cd /var/lib/ && tar -cf - minio-server1 | gzip -9' > minio-server1-backup.tar.gz
    ssh user@my-minio-server.your-domain.com 'cd /var/lib/ && tar -cf - minio-server2 | gzip -9' > minio-server2-backup.tar.gz

Where:

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `my-minio-server.your-domain.com` is the domain name or IP address for the MinIO Virtual Machine
* `minio-server[number]-backup.tar.gz` is the name of the file in which each minio data folder will be stored

### Automated regular backups

It is important to back up as often as possible. You can use `cron` to automatically run your backup commands on a regular basis.

For example, you can create the following shell script, and write it to `/home/myuser/backup/wire-backup.sh`:

    #!/bin/sh
    # Make the folder if it does not exist yet
    mkdir -p /home/myuser/backup/data/

    # Back up wire-server folder
    ssh user@my-wire-server.your-domain.com 'cd /path/to/my/folder/for/wire-server/ && tar -cf - wire-server | gzip -9' > /home/myuser/backup/data/wire-server-backup.tar.gz

    # Cause Cassandra to generate new snapshots
    ssh user@cassandra-vm.your-domain.com 'nodetool snapshot --tag catalog-ks catalogkeyspace`

    # Backup Cassandra snapshots to a file
    ssh user@cassandra-vm.your-domain.com 'find /mnt/cassandra/ -name "snapshots" -print0 | tar -cvf - --null -T - | gzip -9 ' > /home/myuser/backup/data/cassandra-batch.tar.gz

    # Backup MinIO files
    ssh user@my-minio-server.your-domain.com 'cd /var/lib/ && tar -cf - minio-server1 | gzip -9' > /home/myuser/backup/data/minio-server1-backup.tar.gz
    ssh user@my-minio-server.your-domain.com 'cd /var/lib/ && tar -cf - minio-server2 | gzip -9' > /home/myuser/backup/data/minio-server2-backup.tar.gz

    # Tar all backup files into a single unified archive
    tar -cf /home/myuser/backup/full-backup.tar /home/myuser/backup/data/*

    # Make remote copies of this single file to remote hosts for redundancy
    scp /home/myuser/backup/full-backup.tar user@remote-redundancy-host-one.your-domain.com:/path/to/backup/folder/
    scp /home/myuser/backup/full-backup.tar user@remote-redundancy-host-two.your-domain.com:/path/to/backup/folder/

Make the file executable with:

    chmod +x /home/myuser/backup/wire-backup.sh

You can then manually run the file with:

    ./home/myuser/backup/wire-backup.sh

As a test, run the script manually to make sure it actually properly does its job without any errors.

Then, you can add this to your cron file (by running `crontab -e`) to make sure this commands gets executed on a regular basis (here we will use an hourly backup):
                                                                                                                                                                                               
    # Edit this file to introduce tasks to be run by cron.
    # 
    # m h  dom mon dow   command
    @hourly /home/myuser/backup/wire-backup.sh

Your backup should now happen every hour automatically, ensuring you always have a backup at least an hour fresh in case of an emergency.

There are ways to have incremental backups and to do more complex/refined backup procedure, but those are beyond the scope of this document. You should check out [Borg](https://borgbackup.readthedocs.io/en/stable/).

You should also set up your monitoring software (for example [Nagios](https://www.nagios.org/)) to check whether your backup file is [older than an hour](https://support.nagios.com/kb/article/file-and-folder-checks-783.html#file_modified), and if it is (meaning something went wrong), warn you immediately.

## Recovery procedure

If the worse has happened, and you need to recover/restore your Wire installation, you will need to do the following:

1. Create a new Wire installation from scratch (following this website or other customer-specific on-premise instructions you used the first time around).
2. While creating this new wire installation, instead of using a fresh/empty `wire-server` folder, you use the `wire-server` folder you backed up above (as it contains your `values` and `secret` files among other important files).
3. Restore your Cassandra backup files over your new Cassandra installation.
4. Restore your MinIO backup files over your new MinIO installation.
5. Restart all services.
6. Ensure all services are functioning correctly

### Restoring Wire-Server from a backup

If you correctly backup up your `wire-server` installation, you should have access to a file named `wire-server-backup.tar.gz` (see above).

You can extract this file to the remote machine with the following command:

    cat wire-server-backup.tar.gz | ssh user@my-wire-server.your-domain.com "cd /path/to/my/folder/for/wire-server/ && tar zxvf -"

Where :

* `wire-server-backup.tar.gz` is the name of the file in which your wire-server folder has been stored
* `my-wire-server.your-domain.com` is the domain name or IP address for the server with your Wire install
* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `/path/to/my/folder/for/wire-server/` is the (absolute) path, on the server, where your wire-server folder is located

Now all files should be in their proper place, with the proper values, and you should be able to run the required ansible/helm commands that will result in a full installation of Wire.

### Restoring Cassandra from a backup

If you correctly backed up your Cassandra database, you should have a series of `snapshot` files, which you now need to restore over your "fresh" (ie. empty) Cassandra installation.

[This page from the Cassandra documentation](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/operations/opsBackupSnapshotRestore.html) goes over how to restore from snapshots.

You should have lots of snapshots from the backup process. This example will go over how to restore one of the snapshots, but you should do this process for **each and every** snapshot you backup up.

First, transfer and extract the snapshot to the Cassandra Virtual Machine:

    cat cassandra-catalogkeyspace-journal-backup.tar.gz | ssh user@cassandra-vm.your-domain.com "cd /mnt/cassandra/data/data/catalogkeyspace/journal-296a2d30c22a11e9b1350d927649052c/ && tar zxvf -"

Now the folder `/mnt/cassandra/data/data/catalogkeyspace/journal-296a2d30c22a11e9b1350d927649052c/snapshots/` should contain your snapshot.

Next, ssh into the Cassandra Virtual Machine and cd to the snapshot folder:

    ssh user@cassandra-vm.your-domain.com
    cd /mnt/cassandra/data/data/catalogkeyspace/journal-296a2d30c22a11e9b1350d927649052c/

Next run 

    ls -lh

This will show you a list of the snapshots available:

    drwxr-xr-x 2 cassandra cassandra 4.0K May 26 14:45 1653752714460
    drwxr-xr-x 2 cassandra cassandra 4.0K May 28 17:46 1653752759667

Select the most recent one (here it is `1653752759667`).

Finally, use the `sstableloader` file to load the snapshot to Cassandra:

    sstableloader -d localhost /mnt/cassandra/data/data/catalogkeyspace/journal-296a2d30c22a11e9b1350d927649052c/snapshots/1653752759667/

This should load the snapshot.

Repeat this for all snapshots you saved, and you should have a fully restored Cassandra database.

Finally, restart Cassandra:

    sudo service cassandra restart

And ensure that it is working properly.

### Restoring MinIO from a backup

Restoring MinIO from a backup is as simple as extracting the files (`minio-server1-backup.tar.gz` and `minio-server2-backup.tar.gz` which you have backup up in the backup procedure and should have access to) to the correct remote host (MinIO Virtual Machine), and restarting MinIO:

First run:

    cat minio-server1-backup.tar.gz | ssh user@my-minio-server.your-domain.com "cd /var/lib/  && tar zxvf -"
    cat minio-server2-backup.tar.gz | ssh user@my-minio-server.your-domain.com "cd /var/lib/  && tar zxvf -"

Where:

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `my-minio-server.your-domain.com` is the domain name or IP address for the MinIO Virtual Machine
* `minio-server[number]-backup.tar.gz` is the name of the file in which each minio data folder was be stored and backup up

Finally SSH into the server and restart MinIO:

    ssh user@minio-vm.your-domain.com
    sudo service minio restart

Where:

* `user` is the user you used to install Wire on this server, typically `wire` or `root`
* `minio-vm.your-domain.com` is the domain name or IP address for the server with your MinIO node

