## Adding new endpoints in wire-server

When adding new endpoints in the Haskell code in wire-server, correct routing needs to be applied on the nginz level.

NB: The nginz paths are interpreted as *prefixes*.  If you add a new end-point that is identical to an existing one except for the path of the latter being a proper prefix of the former, and if the nginz configuration of the two paths should be the same, nothing needs to be done.  Exception: if you see a path like `/self$`, you know it doesn't match `/self/sub/what`.

Let's assume you add (verbs like GET/PUT are not relevant on nginz level atm) the following:

```
GET /new/endpoint
POST /turtles
PUT /turtles/<turtleId>/name
```

to `galley`. The following needs to be done, ***as part of your PR***.

* (1) Update nginz config under `wire-server/charts/nginz/values.yaml`
    * For the above example, the following needs adding under the `galley` section:
        ```
        - path: /new/endpoint
        - path: ^/turtles(.*)
        ```
    * For internal endpoints for QA access on staging environments, copy a block with `/i/` containing
        ```
        zauth: false
        basic_auth: true
        whitelisted_envs: ['staging']
        ```
    * For customer support access to an internal endpoint, instead update code in [stern](https://github.com/wireapp/wire-server/tree/develop/tools/stern) as part of your PR (no need to add that endpoint to nginz)
* (2) Update nginz config in the demo in wire-server (as part of the haskell change PR ideally)
  * [ ] Update [this file](https://github.com/wireapp/wire-server/blob/develop/deploy/services-demo/conf/nginz/nginx.conf)
    * Use `common_response_no_zauth.conf;` for public endpoints without authentication and `common_response_with_zauth.conf;` for the normal (authenticated) endpoints. Browse the file to see examples.
