Using a Deep Link to connect an App to a Custom Backend
=======================================================

Introduction
------------

Once you have your own wire-server set up and configured, you may want to use a client other than the web interface (webapp). There are a few ways to accomplish this:

- **Using a Deep Link** (which this page is all about)
- Registering your backend instance with the hosted SaaS backend for re-direction. For which you might need to talk to the folks @ Wire (the company).

Assumptions:

- You have wire-server installed and working
- You have a familiarity with JSON files
- You can place a JSON file on an HTTPS supporting web server somewhere your users can reach.

Supported client apps:

- iOS
- Android

Connecting to a custom backend utilizing a Deep Link
----------------------------------------------------

A deep link is a special link a user can click on after installing wire, but before setting it up. This link instructs their wire client to connect to your wire-server, rather than wire.com.

From a user's perspective:
--------------------------

1. First, a user installs the app from the store
2. The user clicks on a deep link, which is formatted similar to: ``wire://access/?config=https://eu-north2.mycustomdomain.de/configs/backend1.json`` (notice the protocol prefix: ``wire://``)
3. The app will ask the user to confirm that they want to connect to a custom backend. If the user cancels, the app exits.
4. Assuming the user did not cancel, the app will download the file ``eu-north2.mycustomdomain.de/configs/backend1.json`` via HTTPS. If it can't download the file or the file doesn't match the expected structure, the wire client will display an error message (*'sInvalid link'*).
5. The app will memorize the various hosts (REST, websocket, team settings, website, support) specified in the JSON and use those when talking to your backend.
6. In the welcome page of the app, a "pill" (header) is shown at the top, to remind the user that they are now on a custom backend. A button "Show more" shows the URL of where the configuration was fetched from.

From the administrator's (your) perspective:
--------------------------------------------

If you're using minio installed using the ansible code from `wire-server-deploy <https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/>`__, then the `minio ansible playbook <https://github.com/wireapp/wire-server-deploy/blob/develop/ansible/minio.yml#L75-L88>`__ (make sure to override these variables) creates a json and a html file in the right format, and makes it accessible at ``https://assets.<domain>/public/deeplink.json`` and at ``https://assets.<domain>/public/deeplink.html``

Otherwise you need to create a ``.json`` file, and host it somewhere users can get to. This ``.json`` file needs to specify the URLs of your backend. For the production wire server that we host, the JSON would look like:

.. code:: json

   {
      "endpoints" : {
         "backendURL" : "https://prod-nginz-https.wire.com",
         "backendWSURL" : "https://prod-nginz-ssl.wire.com",
         "blackListURL" : "https://clientblacklist.wire.com/prod",
         "teamsURL" : "https://teams.wire.com",
         "accountsURL" : "https://accounts.wire.com",
         "websiteURL" : "https://wire.com"
      },
      "title" : "Production"
   }

There is no requirement for these hosts to be consistent, e.g. the REST endpoint could be `wireapp.pineapple.com` and the team setting `teams.banana.com`. If you have been following this documentation closely, these hosts will likely be consistent in naming, regardless.

You now need to get a link referring to that ``..json`` file to your users, prepended with ``wire://access/?config=``. For example, you can save the above ``.json`` file as ``https://example.com/wire.json``, and save the following HTML content as ``https://example.com/wire.html``:

.. code:: html

   <html>
     <head></head>
     <body>
       <a href="wire://access/?config=https://example.com/wire.json">link</a>
     </body>
   </html>

Now, you can email a link to ``https://example.com/wire.html`` to your users, and they can follow the above procedure, by clicking on ``link``.
