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

This command allows you to obtain the data, for example for gundeck:

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
