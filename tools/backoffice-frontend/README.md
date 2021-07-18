# frontend for the backoffice (aka customer support, aka stern)

A docker image containing a Web UI (frontend) for stern.  It can be
run as a composite containing both a stern image and the frontend
(demo mode).  But you can also run the frontend separately and connect
it to a stern that you work on on your local machine (develop mode).

This image has not been thoroughly tested and may or may not be
suitable for production use.


## usage

building the frontend locally:

```sh
make
```

If you skip that part and `export DOCKER_TAG=latest`, docker will try
to pull the image from quay.io.

Run the frontend in demo mode:

```sh
make run-demo
```

Run it in evelop mode:

```sh
make run-develop
```

Read the messages on stdout.


## Not supported: k8s and weird custom setups

k8s is not supported yet.  If you see something like this, please open
an issue:

```
fatal: [bknode0]: FAILED! => {
"assertion": "ip in ansible_all_ipv4_addresses",
"changed": false,
"evaluated_to": false,
"msg": "Assertion failed"
}
```

If you want to run both stern and the frontend on a machine other than
localhost, you are more or less on your own.  It should work, but it
probably requires some tweaking of things in `/conf/`.
