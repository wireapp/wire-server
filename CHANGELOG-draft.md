THIS FILE ACCUMULATES THE RELEASE NOTES FOR THE UPCOMING RELEASE.

# [2021-xx-xx]

## Release Notes

## API Changes

* Remove the long-deprecated `message` field in `POST /connections` (#1726)
* Add `PUT /conversations/:domain/:cnv/name` (#1737)
* Deprecate `PUT /conversations/:cnv/name` (#1737)
* Add `GET & PUT /conversations/:domain/:cnv/self` (#1740)
* Deprecate `GET & PUT /conversations/:cnv/self` (#1740)

## Features

## Bug fixes and other updates

## Documentation

* Added documentation of federation errors (#1674)
* Better swagger schema for the Range type (#1748)
* Add better example for Domain in swagger (#1748)

## Internal changes

* Rewrite the `POST /connections` endpoint to Servant (#1726)

## Federation changes

* Ensure clients only receive messages meant for them in remote convs (#1739)
* Federator CA store and client credentials are now automatically reloaded (#1730)
* Avoid remote calls to get conversation when it is not found locally (#1713)
