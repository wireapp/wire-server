## General

**NOTE**: only a few endpoints are visible here at the moment, more will come as we migrate them to Swagger 2.0. In the meantime please also look at the old swagger docs link for the not-yet-migrated endpoints. See https://docs.wire.com/understand/api-client-perspective/swagger.html for the old endpoints.

## SSO Endpoints

### Overview

`/sso/metadata` will be requested by the IdPs to learn how to talk to wire.

`/sso/initiate-login`, `/sso/finalize-login` are for the SAML authentication handshake performed by a user in order to log into wire.  They are not exactly standard in their details: they may return HTML or XML; redirect to error URLs instead of throwing errors, etc.

`/identity-providers` end-points are for use in the team settings page when IdPs are registered.  They talk json.


### Configuring IdPs

IdPs usually allow you to copy the metadata into your clipboard.  That should contain all the details you need to post the idp in your team under `/identity-providers`.  (Team id is derived from the authorization credentials of the request.)

#### okta.com

Okta will ask you to provide two URLs when you set it up for talking to wireapp:

1. The `Single sign on URL`.  This is the end-point that accepts the user's credentials after successful authentication against the IdP.  Choose `/sso/finalize-login` with schema and hostname of the wire server you are configuring.

2. The `Audience URI`.  You can find this in the metadata returned by the `/sso/metadata` end-point.  It is the contents of the `md:OrganizationURL` element.

#### centrify.com

Centrify allows you to upload the metadata xml document that you get from the `/sso/metadata` end-point.  You can also enter the metadata url and have centrify retrieve the xml, but to guarantee integrity of the setup, the metadata should be copied from the team settings page and pasted into the centrify setup page without any URL indirections.

## Federation errors

Endpoints involving federated calls to other domains can return some extra failure responses, common to all endpoints. Instead of listing them as possible responses for each endpoint, we document them here.

For errors that are more likely to be transient, we suggest clients to retry whatever request resulted in the error. Transient errors are indicated explicitly below.

**Note**: when a failure occurs as a result of making a federated RPC to another backend, the error response contains the following extra fields:

 - `domain`: the target backend of the RPC that failed;
 - `path`: the path of the RPC that failed.

### Domain errors

Errors in this category result from trying to communicate with a backend that is considered non-existent or invalid. They can result from invalid user input or client issues, but they can also be a symptom of misconfiguration in one or multiple backends.

 - **Remote backend not found** (status: 422, label: `srv-record-not-found`): This backend attempted to contact a backend which does not exist or is not properly configured. For the most part, clients can consider this error equivalent to a domain not existing, although it should be noted that certain mistakes in the DNS configuration on a remote backend can lead to the backend not being recognized, and hence to this error. It is therefore not advisable to take any destructive action upon encountering this error, such as deleting remote users from conversations.
 - **Federation denied locally** (status: 400, label: `federation-not-allowed`): This backend attempted an RPC to a non-whitelisted backend. Similar considerations as for the previous error apply.

### Local federation errors

An error in this category likely indicates an issue with configuration of federation on the local backend. Possibly transient errors are indicated explicitly below.

 - **Federation not enabled** (status: 400, label: `federation-not-enabled`): Federation has not been configured for this backend. This will happen if a federation-aware client tries to talk to a backend for which federation is disabled, or if federation was disabled on the backend after reaching a federation-specific state (e.g. conversations with remote users). There is no way to cleanly recover from these errors at this point.
 - **Federation unavailable** (status: 500, label: `federation-not-available`): Federation is configured for this backend, but the local federator cannot be reached. This can be transient, so clients should retry the request.
 - **Federation not implemented** (status: 403, label: `federation-not-implemented`): Federated behaviour for a certain endpoint is not yet implemented.
 - **Federator discovery failed** (status: 500, label: `srv-lookup-dns-error`): A DNS error occurred during discovery of a remote backend. This can be transient, so clients should retry the request.
 - **Too much concurrency** (status: 533, label: `too-much-concurrency`): Too many concurrent requests from this backend. This can be transient, so clients should retry the request.

### Remote federation errors

Errors in this category are returned in case of communication issues between the local backend and a remote one, or if the remote side encountered an error while processing an RPC. Some errors in this category might be caused by incorrect client behaviour or wrong user input. All of these errors can be transient, so clients should retry the request that caused them.

 - **gRPC error** (status: 533, label: `grpc-error`): The current federator encountered an error when making an RPC to a remote one. Check the error message for more details.
 - **Client RPC error** (status: 500, label: `client-rpc-error`): There was a non-specified error when making a request to another backend. Check the error message for more details.
 - **Connection refused** (status: 521, label: `cannot-connect-to-remote-federator`): The local federator could not connect to a remote one.
 - **Unknown remote error** (status: 500, label: `unknown-federation-error`): An RPC failed but no specific error was returned by the remote side. Check the error message for more details.

### Backend compatibility errors

An error in this category will be returned when this backend makes an invalid or unsupported RPC to another backend. This can indicate some incompatibility between backends or a backend bug. These errors are unlikely to be transient, so retrying requests is *not* advised.

 - **Version mismatch** (status: 531): A remote backend is running an unsupported version of the federator.
 - **Invalid method** / **Streaming not supported** (status: 500, label: `federation-invalid-call`): There was an error in the communication between a service on this backend and the local federator.
 - **Invalid request** (status: 500, label: `invalid-request-to-federator`): The local federator made an invalid request to a remote one. Check the error message for more details.
 - **Invalid content type** (status: 503, label: `federation-invalid-content-type-header`): An RPC to another backend returned an invalid content type.
 - **Unsupported content type** (status: 503, label: `federation-unsupported-content-type`): An RPC to another backend returned an unsupported content type.
 - **Invalid origin domain** (status: 533, label: `invalid-origin-domain`): The current backend attempted an RPC with an invalid origin domain field.
 - **Forbidden endpoint** (status: 533, label: `forbidden-endpoint`): The current backend attempted an RPC to a forbidden or inaccessible remote endpoint.
 - **Unknown federation error** (status: 503, label: `unknown-federation-error`): The target of an RPC returned an unexpected reponse. Check the error message for more details.

### Authentication errors

The errors in this category relate to authentication or authorization issues between backends. These errors are unlikely to be transient, so retrying requests is *not* advised.

 - **TLS failure**: (status: 525): An error occurred during the TLS handshake between the local federator and a remote one. This is most likely due to an issue with the certificate on the remote end.
 - **Federation denied remotely** (status: 532): The current backend made an unauthorized request to a remote one.
