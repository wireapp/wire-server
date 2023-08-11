# Redirecting email domains from cloud (or any other backend) to a custom backend

This feature is mostly for the operators of `app.wire.com`, but in
principle it is applicable to any custom backend operator who has
running backoffice/stern deployed and wants to redirect to other
backends for login based on the user's email domain.

If you are hosting your own custom backend, there is a way to allow
users to login by getting redirected from a more visible backend
(usually `app.wire.com`) based on the email address associated with
their account.  As the redirect target operator, you need to know your
domain, a URL under which clients can retrieve a json file to set
themselves up to talk to your backend, and a welcome URL that is the
redirect target.  Then you need to ask the operators of the
redirecting backend to set up their instance accordingly.

The data required is stored in cassandra table
`galley.custom_backend`, and consists of the domain to be re-routed, a
URL under which the client can retrieve a config URL, and a welcome
URL.  You could just use `cqlsh` to maintain this table, but there is
a more convenient way.

[The wire backoffice aka
stern](https://github.com/wireapp/wire-server/tree/develop/tools/stern/README.md)
allows customer support to perform some of the tasks otherwise done by
the site operators, like blocking and unblocking users or teams,
extract billing information from the system, or investigating customer
support tickets.

Stern supports maintaining that table with a simple CRUD interface
under the path `"sso-domain-redirect"`.

## Where do I find my config URL and welcome URL as a redirect target?

TBD.
