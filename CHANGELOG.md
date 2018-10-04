# 2018-10-04

See the [pull request][#477] for the exact changeset.

## Highlights

  * [#469][] We now store the `otr_muted_status` field per conversation,
    suitable for supporting more notifications options than just "muted/not
    muted". The exact meaning of this field is client-dependent.

  * [#467][] Our schema migration tools (which you are probably using if
    you're doing self-hosting) are more resilient now. They have longer
    timeouts and they wait for schema consistency across peers before
    reporting success.

## Other changes

  * [#474][] Building from scratch on macOS is now a tiny bit easier.

  * Various Spar fixes, breaking changes, refactorings, and what-not. Please
    refer to the commit log, in particular commits [c173f42b][] and
    [80d06c9a][].

  * Spar now only accepts a [subset][TLS ciphersuite] of available TLS
    ciphers. See [af8299d4][].

[#467]: https://github.com/wireapp/wire-server/pull/467
[#469]: https://github.com/wireapp/wire-server/pull/469
[#474]: https://github.com/wireapp/wire-server/pull/474
[#477]: https://github.com/wireapp/wire-server/pull/477

[80d06c9a]: https://github.com/wireapp/wire-server/commit/80d06c9aba9f8f6a36bb19f0963636504d403761
[c173f42b]: https://github.com/wireapp/wire-server/commit/c173f42b570fd458553ab354099926d1bd841e4d
[af8299d4]: https://github.com/wireapp/wire-server/pull/466/commits/af8299d4aada7485ecc15cf6e8fb200b46d83d74

[TLS ciphersuite]: https://hackage.haskell.org/package/tls-1.4.1/docs/src/Network-TLS-Extra-Cipher.html#ciphersuite_default
