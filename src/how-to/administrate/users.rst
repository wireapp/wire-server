Investigative tasks (e.g. searching for users as server admin)
---------------------------------------------------------------

This page requires that you have root access to the machines where kubernetes runs on, or have kubernetes permissions allowing you to port-forward arbitrary pods and services.

If you have the `backoffice` pod installed, see also the `backoffice README <https://github.com/wireapp/wire-server-deploy/tree/develop/charts/backoffice>`__.

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

Deleting a user which is not a team user
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can now delete that user by double-checking that the user you wish to delete is really the correct user:

.. code:: sh

   # replace the id with the id of the user you want to delete
   curl -v localhost:9999/i/users/9122e5de-b4fb-40fa-99ad-1b5d7d07bae5 -XDELETE

Afterwards, the previous command (to search for a user in cassandra) should return an empty list (``[]``).

When done, on terminal 1, ctrl+c to cancel the port-forwarding.

Manual search on elasticsearch
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
