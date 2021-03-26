Error Codes
=========================

This page describes the errors that can occur during federation.

.. _authentication-errors:

Authentication Errors
---------------------

TODO for now, we only describe the errors here. Later, we should add exact error codes.

TODO we might want to merge one or more of these errors

* _`authentication error`: occurs when a backend queries another backend and
  provides either no client certificate, or a client certificate that the
  receiving backend cannot authenticate
* _`authorization error`: occurs when a sending backend authenticates successfully,
  but is not on the allow list of the receiving backend
* _`discovery error`: occurs when a sending backend authenticates
  successfully, but the `SRV` record published for the claimed domain of the
  sending backend doesn't match the SAN of the sending backend's client
  certificate
