# 2018-10-04  #477

## Highlights

  * We now store the `otr_muted_status` field per conversation,
    suitable for supporting more notifications options than just "muted/not
    muted". The exact meaning of this field is client-dependent.  #469

  * Our schema migration tools (which you are probably using if
    you're doing self-hosting) are more resilient now. They have longer
    timeouts and they wait for schema consistency across peers before
    reporting success.  #467

## Other changes

  * Building from scratch on macOS is now a tiny bit easier.  #474

  * Various Spar fixes, breaking changes, refactorings, and what-not. Please
    refer to the commit log, in particular commits c173f42b and
    80d06c9a.

  * Spar now only accepts a [subset][TLS ciphersuite] of available TLS
    ciphers. See af8299d4.

[TLS ciphersuite]: https://hackage.haskell.org/package/tls-1.4.1/docs/src/Network-TLS-Extra-Cipher.html#ciphersuite_default
