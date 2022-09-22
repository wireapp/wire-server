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

The Wire user.

## Sequence Diagram - OAuth only

```mermaid
sequenceDiagram
    autonumber
    actor User (Resource Owner)
    User (Resource Owner)->>Client (App): Access
    Client (App)->>ACME (Resource Server): get authorization request
    ACME (Resource Server)-->>Client (App): authorization request
    Client (App)->>Wire backend (Authorization Server): authorization request
    note over User (Resource Owner),Wire backend (Authorization Server): Wire backend is requesting permission to access ACME resources.<br/>This may be implicit because user is logged in already?
    Wire backend (Authorization Server)->>User (Resource Owner): getting user consent
    User (Resource Owner)-->>Wire backend (Authorization Server): approve
    Wire backend (Authorization Server)-->>Client (App): issue access token
    note over Client (App),ACME (Resource Server): The client can now access the required<br/>resources (the ACME server API)<br/>with the provided access token.<br/>This includes all the steps necessary<br/>for the ACME certificate enrollment process.
    Client (App)->>ACME (Resource Server): resource request
    ACME (Resource Server)->>ACME (Resource Server): verify token
    ACME (Resource Server)-->>Client (App): resource response
    note over Client (App),Wire backend: Client fetches DPoP access token
    Client (App)->>Wire backend: HEAD /clients/:cid/nonce
    Wire backend-->>Client (App): backend nonce
    Client (App)->>Wire backend: POST /clients/:cid/access-token
    Wire backend-->>Client (App): DPoP access token
    note over Client (App),ACME (Resource Server): Complete certificate enrollment process
    Client (App)->>ACME (Resource Server): resource request
    ACME (Resource Server)->>ACME (Resource Server): verify token
    ACME (Resource Server)-->>Client (App): resource response    
``` 

    