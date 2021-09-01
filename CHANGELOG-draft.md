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

# [2021-xx-xx]

## Release Notes

## API Changes

* Add `POST /conversations/list/v2` (#1703)
* Deprecate `POST /list-conversations` (#1703)

## Features

## Bug fixes and other updates

* Remove support for managed conversations in member removal (#1718)

## Documentation

* Document backend internals for user connections (#1717)

## Internal changes

* The update conversation membership federation endpoint takes OriginDomainHeader (#1719)
* Added new endpoint to allow fetching conversation metadata by qualified ids (#1703)
