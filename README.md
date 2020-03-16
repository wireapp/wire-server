# System for Cross-domain Identity Management (SCIM)

This implements part of the [SCIM standard](http://www.simplecloud.info)
for identity management. The parts that are currently supported are:

 * User schema version 2.0

## Building

This project uses stack. You can install the sample executable with

```sh
stack install
```

## Developing and testing

This library only implements the schemas and endpoints defined by the
SCIM standard. You will need to implement the actual storage by giving
an instance for the `Persistence` class.

There's a simple in-memory implementation of this class, which is used
for tests. You can run the tests with the standard stack interface:

```sh
stack test
```

# Contributing

Before submitting a PR, make sure to install [ormolu](https://github.com/tweag/ormolu)
by doing `stack install ormolu` (we pin the version in our `stack.yaml` file)
and run `make format`.
