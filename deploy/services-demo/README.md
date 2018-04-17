```
# From the top level folder, run
make services
```

Once that finishes, use 2 different terminals to:

```
# Start the dependencies
cd deploy/docker-ephemeral && docker-compose up
```

```
# Start the services
cd services-demo && ./demo.sh
```

### This is fantastic, all services up & running... what now, can I run some kind of smoketests?

Short answer: yes and no. At the moment, you need _one_ AWS service in order to test your cluster with our automated smoketester tool. The `sesEndpoint` in `brig`'s [example configuration](https://github.com/wireapp/wire-server/blob/develop/services/brig/brig.integration.yaml) needs to point to a real AWS SES endpoint.

In your environment, you can configure `AWS_REGION`, `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` for your correct AWS account. Note that there are other ways to specify these credentials (to be detailed later).

Then, have a look at what the configuration for the [api-smoketest](https://github.com/wireapp/wire-server/blob/develop/tools/api-simulations/README.md) should be. Once you have the correct `mailboxes.json`, this should just work from the top level directory (note the `sender-email` must match brig's [sender-email](https://github.com/wireapp/wire-server/blob/develop/services/brig/brig.integration.yaml#L35))

```
# 
../../api-smoketest --api-host=127.0.0.1 --api-port=8080 --api-websocket-host=127.0.0.1 --api-websocket-port=8081 --mailbox-config=<path_to_mailboxes_file> --sender-email=backend-integration@wire.com --enable-asserts
```

### Common troubles

I see "Schema Version too old! Expecting at least: <...>, but got: <...>". If there are schema changes and you don't force pull the docker migrations, you may run out of sync. We recommend that you run the following:

> docker-compose pull && docker-compose up

Which will ensure that your DB schema is up to date.
