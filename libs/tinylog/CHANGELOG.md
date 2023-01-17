0.15
-----------------------------------------------------------------------------
- Allow full control over the output format (merge requests #2, #3). To do
so, a type `Renderer` has been added which can be configured via
`setRenderer`.  The setting `netstring` has been removed, instead the
complete renderer can be requested via `renderer`.
- A setting `setReadEnvironment` has been added which allows one to ignore
process environment variables (see merge request #7).

0.14.1
-----------------------------------------------------------------------------
- Maintenance release to enable compatibility with GHC 8.4.

0.14.0
-----------------------------------------------------------------------------
- `setFormat` now expects a `Maybe DateFormat` to allow for not having a date
  format at all.

0.13.0
-----------------------------------------------------------------------------
- Change `DateFormat` representation to `UnixTime -> ByteString`. This
  is mainly to allow clients complete control over date and time formatting.

0.12.1
-----------------------------------------------------------------------------
- Relax upper-bound of `fast-logger` dependency.

0.12.0
-----------------------------------------------------------------------------
- Add support for logger-specific log-levels.
- Changed logger `name` in settings to `Maybe Text`.

0.10
-----------------------------------------------------------------------------
- Introduce `Settings` module.

0.9
-----------------------------------------------------------------------------
- Add support for netstrings encoding.

0.8
-----------------------------------------------------------------------------
- Initial release.
