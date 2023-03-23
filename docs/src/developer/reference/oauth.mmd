sequenceDiagram
    autonumber
    actor U as User
    participant C as Outlook Calendar Extension
    participant A as Authorization Server (wire-server)
    participant R as Resource Server (wire-server)

    U->>C: Click login
    C->>A: Authorization code request + code challenge /authorize
    A->>U: Redirect to login/authorization prompt
    U->>A: Authenticate and consent
    A->>C: Authorization code
    C->>A: Authorization code + code verifier
    A->>A: Validate authorization code + code verifier
    A->>C: Access token
    C->>R: Request a resource with access token (e.g. POST /conversations)
    R->>R: Validate access token with public key from auth server
    R->>C: Response
