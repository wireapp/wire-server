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




## Recovery procedure