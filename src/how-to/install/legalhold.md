# Installing and setting up Legal Hold

## Introduction

Legal Hold is a service allowing for all communications of specific users of a Wire installation to be recorded in a secure vault, typically for legal reasons.

The Legal Hold service is composed of three different sub-services:

* Collector, which collects the conversations from individual users
* Exporter, which exports the conversations for storage
* Hold, which holds the conversation after Collector collects them, and before Exporter exports them

A typical installation of Legal Hold looks like this:

1. The Legal Hold service/container is installed and run on a server
2. The Teams Settings for the team that will use Legal Hold is configured to use this service
3. Specific users in that team are selected to have Legal Hold activated
4. These users are asked to confirm that they are aware of the information collection. Once they do, Legal Hold is active for their account.
5. Information starts being collected by the Collector service and stored by the Hold service

## Installing Legal Hold

To install and run Legal Hold, you need a server dedicated to the service. 

Here we will use Ubuntu 18.04 as it is the system wire-server is installed on, but other systems should be possible.

First, we need to install postgreSQL:

```bash
    sudo apt update
    sudo apt install postgresql
```

You then need to change the password to the database. First enter the PostgreSQL console:

```bash
   sudo -u postgres psql
```

Then use this command to change the password:

```sql
   ALTER USER postgres PASSWORD '<your-postgresql-password>';
```

Now that we have a working PostgreSQL database, we need to install the actual legalhold services.

First install Git and Docker:

```bash
    sudo apt update
    sudo apt install git docker.io
```

Then clone the legalhold repository: 

```bash
    git clone https://github.com/wireapp/legalhold.git
    cd legalhold/
```

Generate a random secret to use as your service token. 

Here we will use `secr3t` as an example, however you can for example use this command to generate one:

```bash
   openssl rand -hex 20
```

Finally, run the actual Docker container for the LegalHold service:

```bash
    docker run \
    -e DB_URL='jdbc:postgresql://localhost/legalhold' \
    -e DB_USER='postgres' \
    -e DB_PASSWORD='your-postgresql-password' \
    -e SERVICE_TOKEN='secr3t' \
    -p 80:8080 \
    --name secure-hold --rm quay.io/wire/legalhold:1.0.4
```

Now that the legalhold service is running, you need to configure the DNS for your domain so that `legal.<yourdomain>` redirects to this service.

## Configuring Team Settings to use Legal Hold

The next step is to configure the Wire Team, in the Team Settings interface, to interface with the newly installed Legal Hold service.

To do this, first go to `teams.wire.com` or `teams.<your-domain>` depending on your setup, and log in.

Once in the interface, select `Customization` in the menu on the left:

![Customization](img/legalhold-step01-click-customization.png)


On the customization page, go to `Legal Hold`:

![Legal Hold](img/legalhold-step02-goto-legalhold.png)


Under the Legal Hold section, click the small down-pointing blue arrow:

![Arrow](img/legalhold-step03-click-arrow.png)


In the extended Legal Hold section, click on `Manage Configuration`:

![Manage Configuration](img/legalhold-step04-click-manage-configuration.png)

From here, you can fill in the required configuration info:

![Fill in configuration](img/legalhold-step05-fill-info.png)

