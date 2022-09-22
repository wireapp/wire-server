# End-to-end client identity certificate enrollment (OAuth & OIDC)

## Roles

### The Third-Party Application: "Client" or "App"

> The client is the application that is attempting to get access to the user's account. It needs to get permission from the user before it can do so.

In our use case this is the *Wire client* or *device*.

### The API: "Resource Server"

> The resource server is the API server used to access the user's information.

The resource server is the *ACME server* and the resources the client requires access to is the ACME server API.

### The Authorization Server

> This is the server that presents the interface where the user approves or denies the request. In smaller implementations, this may be the same server as the API server, but larger scale deployments will often build this as a separate component.

The *Wire backend*.

### The User: Resource Owner

> The resource owner is the person who is giving access to some portion of their account.

A user with a Wire account.

### Scope

The ACME server API.

## Sequence Diagram - OAuth only (Device Authorization Grant & OIDC)

```mermaid
sequenceDiagram
    autonumber
    actor User (Resource Owner)
    User (Resource Owner)->>Client (App): Access Wire client
    note over Client (App): create random code verifier(v)<br/>$=sha256(v)
    Client (App)->>Wire backend (Authorization Server): login with credentials and $
    note over Wire backend (Authorization Server): store $
    note over Wire backend (Authorization Server): authenticate user
    Wire backend (Authorization Server)-->>Client (App): issue authorization code α
    Client (App)->>Wire backend (Authorization Server): request access token
    note over Client (App),Wire backend (Authorization Server): request includes:<br/>client_id=abc123<br/>authorization_code=α<br/>code_verifier=v<br/>scope=acme<br/>state=foobar
    Wire backend (Authorization Server)->>Wire backend (Authorization Server): verify request
    note over Wire backend (Authorization Server): validate:<br/>client_id<br/>sha265(v)=$<br/>α
    Wire backend (Authorization Server)-->>Client (App): issue authorization token
    note over Client (App),Wire backend (Authorization Server): {"access_token":"Phai6Eesheirae3r",<br/>"expires_in":3920,"token_type":"Bearer"}
    note over Client (App),ACME (Resource Server): The client can now access the required<br/>resources (the ACME server API)<br/>with the provided access token.<br/>This includes all the steps necessary<br/>for the ACME certificate enrollment process.<br/>Specifically create ACME account.
    Client (App)->>ACME (Resource Server): resource request
    note right of Client (App): Authorization: Bearer Phai6Eesheirae3r
    note over Client (App),ACME (Resource Server): and so on...
    note over Client (App),Wire backend: Client fetches DPoP access token
    Client (App)->>Wire backend: HEAD /clients/:cid/nonce
    Wire backend-->>Client (App): backend nonce
    Client (App)->>Wire backend: POST /clients/:cid/access-token
    Wire backend-->>Client (App): DPoP access token
    note over Client (App),ACME (Resource Server): Complete certificate enrollment process
``` 

### Questions

- 2,3: Do we really need to get the authorization request from the ACME server? Maybe the client create the request itself?
- What type of authorization grant are we going to use?
  - Maybe *client credential* (back channel only, recommended for machine to machine/service communication)
- How do we set up client credentials
