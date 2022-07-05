.. _investigative_tasks:

Investigative tasks (e.g. searching for users as server admin)
---------------------------------------------------------------

This page requires that you have root access to the machines where kubernetes runs on, or have kubernetes permissions allowing you to port-forward arbitrary pods and services.

If you have the `backoffice` pod installed, see also the `backoffice README <https://github.com/wireapp/wire-server/tree/develop/charts/backoffice>`__.

If you don't have `backoffice`, see below for some options:

Manually searching for users in cassandra
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Terminal one:

.. code:: sh

   kubectl port-forward svc/brig 9999:8080

Terminal two: Search for your user by email:

.. code:: sh

   EMAIL=user@example.com
   curl -v -G localhost:9999/i/users --data-urlencode email=$EMAIL; echo
   # or, for nicer formatting
   curl -v -G localhost:9999/i/users --data-urlencode email=$EMAIL | json_pp

You can also search by ``handle`` (unique username) or by phone:

.. code:: sh

   HANDLE=user123
   curl -v -G localhost:9999/i/users --data-urlencode handles=$HANDLE; echo

   PHONE=+490000000000000 # phone numbers must have the +country prefix and no spaces
   curl -v -G localhost:9999/i/users --data-urlencode phone=$PHONE; echo


Which should give you output like:

.. code:: json

   [
      {
         "managed_by" : "wire",
         "assets" : [
            {
               "key" : "3-2-a749af8d-a17b-4445-b360-46c93fc41bc6",
               "size" : "preview",
               "type" : "image"
            },
            {
               "size" : "complete",
               "type" : "image",
               "key" : "3-2-6cac6b57-9972-4aba-acbb-f078bc538b54"
            }
         ],
         "picture" : [],
         "accent_id" : 0,
         "status" : "active",
         "name" : "somename",
         "email" : "user@example.com",
         "id" : "9122e5de-b4fb-40fa-99ad-1b5d7d07bae5",
         "locale" : "en",
         "handle" : "user123"
      }
   ]

The interesting part is the ``id`` (in the example case ``9122e5de-b4fb-40fa-99ad-1b5d7d07bae5``):

.. _user-deletion:

Deleting a user which is not a team user
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following will completely delete a user, its conversations, assets, etc. The only thing remaining will be an entry in cassandra indicating that this user existed in the past (only the UUID remains, all other attributes like name etc are purged)

You can now delete that user by double-checking that the user you wish to delete is really the correct user:

.. code:: sh

   # replace the id with the id of the user you want to delete
   curl -v localhost:9999/i/users/9122e5de-b4fb-40fa-99ad-1b5d7d07bae5 -XDELETE

Afterwards, the previous command (to search for a user in cassandra) should return an empty list (``[]``).

When done, on terminal 1, ctrl+c to cancel the port-forwarding.

Manual search on elasticsearch (via brig, recommended)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This should only be necessary in the case of some (suspected) data inconsistency between cassandra and elasticsearch.

Terminal one:

.. code:: sh

   kubectl port-forward svc/brig 9999:8080

Terminal two: Search for your user by name or handle or a prefix of that handle or name:

.. code:: sh

   NAMEORPREFIX=test7
   UUID=$(cat /proc/sys/kernel/random/uuid)
   curl -H "Z-User:$UUID" "http://localhost:9999/search/contacts?q=$NAMEORPREFIX"; echo
   # or, for pretty output:
   curl -H "Z-User:$UUID" "http://localhost:9999/search/contacts?q=$NAMEORPREFIX" | json_pp

If no match is found, expect a query like this:

.. code:: json

   {"took":91,"found":0,"documents":[],"returned":0}

If matches are found, the result should look like this:

.. code:: json

   {
      "found" : 2,
      "documents" : [
         {
            "id" : "dbdbf370-48b3-4e1e-b377-76d7d4cbb4f2",
            "name" : "Test",
            "handle" : "test7",
            "accent_id" : 7
         },
         {
            "name" : "Test",
            "accent_id" : 0,
            "handle" : "test7476",
            "id" : "a93240b0-ba89-441e-b8ee-ff4403808f93"
         }
      ],
      "returned" : 2,
      "took" : 4
   }

How to manually search for a user on elasticsearh directly (not recommended)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, ssh to an elasticsearch instance.

.. code:: sh

  ssh <ip of elasticsearch instance>

Then run the following:

.. code:: sh

   PREFIX=...
   curl -s "http://localhost:9200/directory/_search?q=$PREFIX" | json_pp

The `id` (UUID) returned can be used when deleting (see below).

How to manually delete a user from elasticsearch only
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. warning::

   This is NOT RECOMMENDED. Be sure you know what you're doing. This only deletes the user from elasticsearch, but not from cassandra. Any change of e.g. the username or displayname of that user means this user will re-appear in the elasticsearch database. Instead, either fully delete a user: :ref:`user-deletion` or make use of the internal GET/PUT ``/i/searchable`` endpoint on brig to make this user prefix-unsearchable.

If, despite the warning, you wish to continue?

First, ssh to an elasticsearch instance:

.. code:: sh

  ssh <ip of elasticsearch instance>

Next, check that the user exists:

.. code:: sh

   UUID=...
   curl -s "http://localhost:9200/directory/user/$UUID" | json_pp

That should return a ``"found": true``, like this:

.. code:: json

   {
      "_type" : "user",
      "_version" : 1575998428262000,
      "_id" : "b3e9e445-fb02-47f3-bac0-63f5f680d258",
      "found" : true,
      "_index" : "directory",
      "_source" : {
         "normalized" : "Mr Test",
         "handle" : "test12345",
         "id" : "b3e9e445-fb02-47f3-bac0-63f5f680d258",
         "name" : "Mr Test",
         "accent_id" : 1
      }
   }


Then delete it:

.. code:: sh

   UUID=...
   curl -s -XDELETE "http://localhost:9200/directory/user/$UUID" | json_pp

Mass-invite users to a team
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you need to invite members to a specific given team, you can use the ``create_team_members.sh`` Bash script, located `here <https://github.com/wireapp/wire-server/blob/develop/deploy/services-demo/create_team_members.sh>`__.

This script does not create users or causes them to join a team by itself, instead, it sends invites to potential users via email, and when users accept the invitation, they create their account, set their password, and are added to the team as team members.

Input is a `CSV file <https://en.wikipedia.org/wiki/Comma-separated_values>`__, in comma-separated format, in the form ``'Email,Suggested User Name'``.

You also need to specify the inviting admin user, the team, the URI for the Brig (`API <https://docs.wire.com/understand/federation/api.html?highlight=brig>`__) service (Host), and finally the input (CSV) file containing the users to invite.

The exact format for the parameters passed to the script is `as follows <https://github.com/wireapp/wire-server/blob/develop/deploy/services-demo/create_team_members.sh#L17>`__:

* ``-a <admin uuid>``: `User ID <https://docs.wire.com/understand/federation/api.html?highlight=user%20id#qualified-identifiers-and-names>`__ in `UUID format <https://en.wikipedia.org/wiki/Universally_unique_identifier>`__ of the inviting admin. For example ``9122e5de-b4fb-40fa-99ad-1b5d7d07bae5``.
* ``-t <team uuid>``: ID of the inviting team, same format.
* ``-h <host>``: Base URI of brig's internal endpoint.
* ``-c <input file>``: file containing info on the invitees in format 'Email,UserName'.

For example, one such execution of the script could look like:

.. code:: sh

   sh create_team_members.sh -a 9122e5de-b4fb-40fa-99ad-1b5d7d07bae5 -t 123e4567-e89b-12d3-a456-426614174000 -h http://localhost:9999 -c users_to_invite.csv

Note: the 'http://localhost:9999' implies you are running the 'kubectl port-forward' given at the top of this document
.
Once the script is run, invitations will be sent to each user in the file every second until all invitations have been sent.

If you have a lot of invitations to send and this is too slow, you can speed things up by commenting `this line <https://github.com/wireapp/wire-server/blob/develop/deploy/services-demo/create_team_members.sh#L91>`__.


How to obtain logs from an Android client to investigate issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Wire clients communicate with Wire servers (backend).

Sometimes to investigate server issues, you (or the Wire team) will need client information, in the form of client logs.

In order to obtain client logs on the Android Wire client, follow this procedure:

* Open the Wire app (client) on your Android device
* Click on the round user icon in the top left of the screen, leading to your user Profile.
* Click on "Settings" at the bottom of the screen
* Click on "Advanced" in the menu
* Check/activate "Collect usage data"
* Now go back to using your client normally, so usage data is generated. If you have been asked to follow a specific testing regime, or log a specific problem, this is the time to do so.
* Once enough usage data is generated, go back to the "Advanced" screen (User profile > Settings > Advanced)
* Click on "Create debug report"
* A menu will open allowing you to share the debug report, you can now save it or send it via email/any other means to the Wire team.


How to obtain logs from an iOS client to investigate issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Wire clients communicate with Wire servers (backend).

Sometimes to investigate server issues, you (or the Wire team) will need client information, in the form of client logs.

In order to obtain client logs on the iOS Wire client, follow this procedure:

* Open the Wire app (client) on your iOS device
* Click on the round user icon in the top left of the screen, leading to your user Profile.
* Click on "Settings" at the bottom of the screen
* Click on "Advanced" in the menu
* Check/activate "Collect usage data"
* Now go back to using your client normally, so usage data is generated. If you have been asked to follow a specific testing regime, or log a specific problem, this is the time to do so.
* Once enough usage data is generated, go back to the "Advanced" screen (User profile > Settings > Advanced)
* Click on "Send report to wire"
* A menu will open to share the debug report via email, allowing you to send it to the Wire team.

How to retrieve metric values manually
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Metric values are sets of data points about services, such as status and other measures, that can be retrieved at specific endpoints, typically by a monitoring system (such as Prometheus) for monitoring, diagnosis and graphing.

Sometimes, you will want to manually obtain this data that is normally automatically grabbed by Prometheus.

Some of the pods allow you to grab metrics by accessing their ``/i/metrics`` endpoint, in particular:

* ``brig``: User management API
* ``cannon``: WebSockets API
* ``cargohold``: Assets storage API
* ``galley``: Conversations and Teams API
* ``gundeck``: Push Notifications API
* ``spar``: Single-Sign-ON and SCIM

For more details on the various services/pods, you can check out `this link <../../understand/overview.html?highlight=gundeck#focus-on-pods>`.

Before you can grab metrics from a pod, you need to find its IP address. You do this by running the following command:

.. code:: sh

   d kubectl get pods -owide

(this presumes you are already in your normal Wire environment, which you obtain by running ``source ./bin/offline-env.sh``)

Which will give you an output that looks something like this:

.. code::

   demo@Ubuntu-1804-bionic-64-minimal:~/Wire-Server$ d kubectl get pods -owide
   NAME                               READY   STATUS      RESTARTS   AGE     IP              NODE        NOMINATED NODE   READINESS GATES
   account-pages-784f9b547c-cp444     1/1     Running     0          6d23h   10.233.113.5    kubenode3   <none>           <none>
   brig-746ddc55fd-6pltz              1/1     Running     0          6d23h   10.233.110.11   kubenode2   <none>           <none>
   brig-746ddc55fd-d59dw              1/1     Running     0          6d4h    10.233.110.23   kubenode2   <none>           <none>
   brig-746ddc55fd-zp7jl              1/1     Running     0          6d23h   10.233.113.10   kubenode3   <none>           <none>
   brig-index-migrate-data-45rm7      0/1     Completed   0          6d23h   10.233.110.9    kubenode2   <none>           <none>
   cannon-0                           1/1     Running     0          3h1m    10.233.119.41   kubenode1   <none>           <none>
   cannon-1                           1/1     Running     0          3h1m    10.233.113.47   kubenode3   <none>           <none>
   cannon-2                           1/1     Running     0          3h1m    10.233.110.51   kubenode2   <none>           <none>
   cargohold-65bff97fc6-8b9ls         1/1     Running     0          6d4h    10.233.113.20   kubenode3   <none>           <none>
   cargohold-65bff97fc6-bkx6x         1/1     Running     0          6d23h   10.233.113.4    kubenode3   <none>           <none>
   cargohold-65bff97fc6-tz8fh         1/1     Running     0          6d23h   10.233.110.5    kubenode2   <none>           <none>
   cassandra-migrations-bjsdz         0/1     Completed   0          6d23h   10.233.110.3    kubenode2   <none>           <none>
   demo-smtp-784ddf6989-vmj7t         1/1     Running     0          6d23h   10.233.113.2    kubenode3   <none>           <none>
   elasticsearch-index-create-7r8g4   0/1     Completed   0          6d23h   10.233.110.4    kubenode2   <none>           <none>
   fake-aws-sns-6c7c4b7479-wfp82      2/2     Running     0          6d4h    10.233.110.27   kubenode2   <none>           <none>
   fake-aws-sqs-59fbfbcbd4-n4c5z      2/2     Running     0          6d23h   10.233.110.2    kubenode2   <none>           <none>
   galley-7c89c44f7b-nm2rr            1/1     Running     0          6d23h   10.233.110.8    kubenode2   <none>           <none>
   galley-7c89c44f7b-tdxz4            1/1     Running     0          6d23h   10.233.113.6    kubenode3   <none>           <none>
   galley-7c89c44f7b-tr8pm            1/1     Running     0          6d4h    10.233.110.29   kubenode2   <none>           <none>
   galley-migrate-data-g66rz          0/1     Completed   0          6d23h   10.233.110.13   kubenode2   <none>           <none>
   gundeck-7fd75c7c5f-jb8xq           1/1     Running     0          6d23h   10.233.110.6    kubenode2   <none>           <none>
   gundeck-7fd75c7c5f-lbth9           1/1     Running     0          6d23h   10.233.113.8    kubenode3   <none>           <none>
   gundeck-7fd75c7c5f-wvcw6           1/1     Running     0          6d4h    10.233.113.23   kubenode3   <none>           <none>
   nginz-5cdd8b588b-dbn86             2/2     Running     16         6d23h   10.233.113.11   kubenode3   <none>           <none>
   nginz-5cdd8b588b-gk6rw             2/2     Running     14         6d23h   10.233.110.12   kubenode2   <none>           <none>
   nginz-5cdd8b588b-jvznt             2/2     Running     11         6d4h    10.233.113.21   kubenode3   <none>           <none>
   reaper-6957694667-s5vz5            1/1     Running     0          6d4h    10.233.110.26   kubenode2   <none>           <none>
   redis-ephemeral-master-0           1/1     Running     0          6d23h   10.233.113.3    kubenode3   <none>           <none>
   spar-56d77f85f6-bw55q              1/1     Running     0          6d23h   10.233.113.9    kubenode3   <none>           <none>
   spar-56d77f85f6-mczzd              1/1     Running     0          6d4h    10.233.110.28   kubenode2   <none>           <none>
   spar-56d77f85f6-vvvfq              1/1     Running     0          6d23h   10.233.110.7    kubenode2   <none>           <none>
   spar-migrate-data-ts4sx            0/1     Completed   0          6d23h   10.233.110.14   kubenode2   <none>           <none>
   team-settings-fbbb899c-qxx7m       1/1     Running     0          6d4h    10.233.110.24   kubenode2   <none>           <none>
   webapp-d97869795-grnft             1/1     Running     0          6d4h    10.233.110.25   kubenode2   <none>           <none>

Here presuming we need to get metrics from ``gundeck``, we can see the IP of one of the gundeck pods is ``10.233.110.6``.

We can therefore connect to node ``kubenode2`` on which this pod runs with ``ssh kubenode2.your-domain.com``, and run the following:

.. code:: sh

   curl 10.233.110.6:8080/i/metrics

Alternatively, if you don't want to, or can't for some reason, connect to kubenode2, you can use port redirect instead:

.. code:: sh

   # Allow Gundeck to be reached via the port 7777
   kubectl --kubeconfig kubeconfig.dec -n wire port-forward service/gundeck 7777:8080
   # Reach Gundeck directly at port 7777 using curl, output resulting data to stdout/terminal
   curl -v http://127.0.0.1:7777/i/metrics

Output will look something like this (truncated):

.. code:: sh

   # HELP gc_seconds_wall Wall clock time spent on last GC
   # TYPE gc_seconds_wall gauge
   gc_seconds_wall 5481304.0
   # HELP gc_seconds_cpu CPU time spent on last GC
   # TYPE gc_seconds_cpu gauge
   gc_seconds_cpu 5479828.0
   # HELP gc_bytes_used_current Number of bytes in active use as of the last GC
   # TYPE gc_bytes_used_current gauge
   gc_bytes_used_current 1535232.0
   # HELP gc_bytes_used_max Maximum amount of memory living on the heap after the last major GC
   # TYPE gc_bytes_used_max gauge
   gc_bytes_used_max 2685312.0
   # HELP gc_bytes_allocated_total Bytes allocated since the start of the server
   # TYPE gc_bytes_allocated_total gauge
   gc_bytes_allocated_total 4.949156056e9

This example is for Gundeck, but you can also get metrics for other services. All k8s services are listed at `this link <../../understand/overview.html?highlight=gundeck#focus-on-pods>`__.

This is an example adapted for Cannon:

.. code:: sh

   kubectl --kubeconfig kubeconfig.dec -n wire port-forward service/cannon 7777:8080
   curl -v http://127.0.0.1:7777/i/metrics

In the output of this command, ``net_websocket_clients`` is roughly the number of connected clients.

.. _reset session cookies:

Reset session cookies
~~~~~~~~~~~~~~~~~~~~~

Remove session cookies on your system to force users to login again within the next 15 minutes (or whenever they come back online):

.. warning::
   This will cause interruptions to ongoing calls and should be timed properly.

Reset cookies of all users
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: sh

   ssh <name or IP of brig-cassandra>
   # from the ssh session
   cqlsh
   # from the cqlsh shell
   truncate brig.user_cookies;

Reset cookies for a defined list of users
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: sh

   ssh <name or IP of brig-cassandra>
   # within the ssh session
   cqlsh
   # within the cqlsh shell: delete all users by userId
   delete from brig.user_cookies where user in (c0d64244-8ab4-11ec-8fda-37788be3a4e2, ...);

(Keep reading if you want to find out which users on your system are using SSO.)

.. _identify sso users:

Identify all users using SSO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Collect all teams configured with an IdP:

.. code:: sh

   ssh <name or IP of spar-cassandra>
   # within the ssh session start cqlsh
   cqlsh
   # within the cqlsh shell export all teams with idp
   copy spar.idp (team) TO 'teams_with_idp.csv' with header=false;

Close the session and proceed locally:

.. code:: sh

   # download csv file
   scp <name or IP of spar-cassandra>:teams_with_idp.csv .
   # convert to a single line, comma separated list
   tr '\n' ',' < teams_with_idp.csv; echo

And use this list to get all team members in these teams:

.. code:: sh

   ssh <name or IP of galley-cassandra>
   # within the ssh session start cqlsh
   cqlsh
   # within the cqlsh shell select all members of previous identified teams
   # <output of tr> should look like this: f2207d98-8ab3-11ec-b689-07fc1fd409c9, ...
   select user from galley.team_member where team in (<output of tr>);
   # alternatively, export the list of all users (for filterling locally in eg. excel)
   copy galley.team_member (user, team, sso_id) TO 'users_with_idp.csv' with header=true;

Close the session and proceed locally to generate the list of all users from teams with IdP:

.. code:: sh

   # download csv file
   scp <name or IP of brig-cassandra>:users_with_idp.csv .
   # convert to a single line, comma separated list
   tr '\n' ',' < users_with_idp.csv; echo


.. note::
   Don't forget to dellete the created csv files after you have downloaded/processed them.
