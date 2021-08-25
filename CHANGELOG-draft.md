THIS FILE ACCUMULATES THE RELEASE NOTES FOR THE UPCOMING RELEASE.

<!--

# [2021-xx-xx]

[please put all changes that only affect federation into this section to unclutter the rest of the release notes.]
[if something is both an API change and a feature, please mention it twice (you can abbreviate the second mention and add "see above").]

## Release Notes

## API Changes

## Features

## Bug fixes and other updates

## Documentation

## Internal changes

-->


# [unreleased]

[please put all changes that only affect federation into the "Federation changes" section to unclutter the rest of the release notes.]
[if something is both an API change and a feature, please mention it twice (you can abbreviate the second mention and add "see above").]

## Release Notes

## API Changes

* Deprecate `DELETE /conversations/:cnv/members/:usr` (#1697)
* Add `DELETE /conversations/:cnv/members/:domain/:usr` (#1697)

## Features

## Bug fixes and other updates

* Fix case sensitivity in schema parser in hscim library (#1714)
* [helm charts] resolve a rate-limiting issue when using certificate-manager alongside wire-server and nginx-ingress-services helm charts (#1715)

## Documentation

* Improve Swagger for `DELETE /conversations/:cnv/members/:usr` (#1697)

## Internal changes

* Integration test script now displays output interactively (#1700)
* Fixed a few issues with error response documentation in Swagger (#1707)
* Make mapping between (team) permissions and roles more lenient (#1711)
* The `DELETE /conversations/:cnv/members/:usr` endpoint rewritten to Servant (#1697)

## Federation changes

* Added client certificate support for server to server authentication (#1682)
* Implemented full server-to-server authentication (#1687)
* Add an endpoint for removing a qualified user from a local conversation (#1697)
