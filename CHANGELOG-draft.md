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

* Add `POST /conversations/list/v2`
* Deprecate `POST /list-conversations`

## Features

## Bug fixes and other updates

## Documentation

## Internal changes

* Integration test script now displays output interactively (#1700)

## Federation changes

* Added client certificate support for server to server authentication (#1682)
* Implemented full server-to-server authentication (#1687)
* Added new endpoint to allow fetching conversation metadata by qualified ids (#1703)
* Avoid remote calls to get conversation when it is not found locally (#1713)
