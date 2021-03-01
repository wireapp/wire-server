# Developer how-to's

The following assume you have a working developer environment with all the dependencies listed in [./dependencies.md](./dependencies.md) available to you.

## How to look at the swagger docs / UI locally

Terminal 1:
* Set up backing services: `./deploy/dockerephemeral/run.sh`

Terminal 2:
* Compile all services: `make services`
* Run services including nginz: `export INTEGRATION_USE_NGINZ=1; ./services/start-services-only.sh`

Open your browser at:

- http://localhost:8080/api/swagger-ui for the swagger 2.0 endpoints (in development as of Feb 2021 - more endpoints will be added here as time goes on)
- http://localhost:8080/swagger-ui/ for the old swagger 1.2 API (old swagger, endpoints will disappear from here (and become available in the previous link) as time progresses)

Swagger json (for swagger 2.0 endpoints) is available under http://localhost:8080/api/swagger.json
