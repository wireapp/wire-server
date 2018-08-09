# Architecture

The wire backend, as described in the [diagram](https://github.com/wireapp/wire-server/blob/develop/doc/arch/wire-arch-2.png), is composed by a few different components that can be divided in:

  * frontend service
    * [nginz](#nginz)
  * backend services
    * [brig](#brig)
    * [galley](#galley)
    * [gundeck](#gundeck)
    * [proxy](#proxy)
    * [cargohold](#cargohold)
    * [cannon](#cannon)
    * [spar](#spar)
  * media server
    * [restund](#restund) 

## API

Wire implements a RESTful API, using (mostly) JSON over HTTPS to communicate between clients and the server. A more accurate description of the different endpoints can be found by looking at [our swagger](https://github.com/wireapp/wire-server/blob/develop/deploy/services-demo/README.md#is-there-a-way-to-look-at-some-api-endpoints) documentation. A handful of endpoints also support protocol buffers for performance reasons.

Besides the HTTPS channel, every client should establish a websocket connection with the backend: this is then used to push notifications back to clients (e.g., when client A wants to send a text message to client B, client A makes a POST request to the `/conversations/:id/otr/messages` and assuming B is online, B would then receive that message as a notification over the websocket channel).
  
## Authentication

There are 2 types of endpoints that our API supports, namely:

 * requests that do not require authentication
   * user registration, log in, password reset, account activation
 * requests that require authentication
   * all other requests require authentication (posting a message, establishing a websocket connection, etc.)

The authentication protocol used by the API is loosely inspired by the OAuth2 protocol. As such, API requests are authorised through so-called bearer tokens. For as long as a bearer token is valid, it grants access to the API under the identity of the user whose credentials have been used for the login. The validity of access tokens can be set on `brig` and is advertised to the API users when an access token is obtained (typically 15 minutes).

In order to obtain new access tokens without having to ask the user for his credentials again, so-called "user tokens" are issued which are issued in the form of a `zuid` HTTP cookie. These cookies have a long lifetime (if persistent, typically at least a few months) and their use is strictly limited to the `/access` endpoint used for token refresh. Persistent access cookies are regularly refreshed as part of an access token refresh.

An access cookie is obtained either directly after registration or through a subsequent login. A successful login provides both an access cookie and and access token. Both access token and cookie must be stored safely and kept confidential. User passwords should not be stored.

We are currently working on authorising third-party applications to perform operations on the API on behalf of a user. Such functionality (SSO) is currently being worked on using SAML as a protocol and `spar` is the service responsible for it.

To authorise an API request, the access token must be provided via the HTTP Authorization header with the Bearer scheme as follows:

```
Authorization: Bearer fmmLpDSjArpksFv57r5rDrzZZlj...
```

## Authorization

Once requests are authenticated (valid access token), the backend servers perform a number of authorization checks for every request. These will vary on the type of request that is being made but some examples are:

  * user A wants to send a message in conversation B. When `galley` receives this request, it will check that user A is in fact part of the conversation B: if that is not the case, then this request will result in an error.
  * user A wants to invite a new member to team B. When `brig` receives this request, it will check that A is a member of the team B _and_ has sufficient permissions to invite new members.
  
## nginz
  
https://github.com/wireapp/wire-server/blob/develop/services/nginz

## brig
  
https://github.com/wireapp/wire-server/blob/develop/services/brig

## galley
  
https://github.com/wireapp/wire-server/blob/develop/services/galley

## gundeck
  
https://github.com/wireapp/wire-server/blob/develop/services/gundeck

## proxy
  
https://github.com/wireapp/wire-server/blob/develop/services/proxy

## cannon
  
https://github.com/wireapp/wire-server/blob/develop/services/cannon

## spar
  
https://github.com/wireapp/wire-server/blob/develop/services/spar

## restund
  
https://github.com/wireapp/wire-server/blob/develop/services/restund

## Data storages used

## cassandra

Main data storage, we use separate keyspaces to store different types of information, namely:

 * cassandra brig
   * user profiles and connections
   * team invitations
   * providers and services
 * cassandra galley
   * conversation metadata and membership
   * team membership
 * cassandra gundeck
   * user notifications

## elasticsearch

We use elasticsearch to provide user directory search

## redis

Websockets presences so that notifications can be routed to the correct cannon that the user is connected to