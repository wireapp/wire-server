# Token format

```
token              ::= <signature> "." <version> "." <key-index> "." <timestamp> "." <type> "." <tag> "." <type-specific-data>
signature          ::= Ed25519 signature
version            ::= "v=" Integer
key-index          ::= "k=" Integer (> 0)
timestamp          ::= "d=" Integer (POSIX timestamp, expiration time)
type               ::= "t=" ("a" | "u" | "b" | "p") ; access, user, bot, provider
tag                ::= "l=" ("s" | ""  (session or nothing))
type-specific-data ::= <access-data> | <user-data> | <bot-data> | <provider-data>
access-data        ::= "u=" <UUID> "." "c=" <Word64>
user-data          ::= "u=" <UUID> "." "r=" <Word32>
bot-data           ::= "p=" <UUID> "." "b=" <UUID> "." "c=" <UUID>
provider-data      ::= "p=" <UUID>
```

## Examples

### User-Token

`7B2fdkjqBm0BZEpvF_1itY-W22LM2RWLDIQgu2k7d-BJojlMfyNpVfXYPEQiWpcCztmwZO_yphgKhhtKetiuCw==.v=1.k=1.d=1409335821.t=u.l=.u=c5eda68f-93f3-4413-93fe-d45e81f8a9f9.r=bb3d1d9f`

### User-Token (Session)

`7CPhoJv6TOYr7epokS6S2pj0nLoV-mJ_o5iRUII3JM5jBItZzluXNNGb-u476EYQM0fpr1qUGK2eRuKCZuELBA==.v=1.k=1.d=1429832092.t=u.l=s.u=161e7fe7-9a71-4ffd-9a79-de9ee2fa178c.r=3f6a49c4`

### Access-Token

`5Bdn6CnDO2yIng7_MblYFhMNEo27ESsHsZmD40fNpcTdEybk15dw7zUVOcJDeFyf6QbEsZF4ruNKRu1ICmbzCg==.v=1.k=1.d=1419834921.t=a.l=.u=c5eda68f-93f3-4413-93fe-d45e81f8a9f9.c=8875802285613998639`

# Token creation

Given:

- `d` = token duration (in seconds)
- `k` = the secret key of ed25519
- `i` = the key index of `k`
- `v` = the token version
- `x` = the token-type specific data

Then:

- let `n` = current POSIX time + `d`
- let `t` = the concatenated token data as specified above (sans `<signature> "."`)
- let `s` = the ed25519 token signature of `t` using `k`
- return as token URL-safe Base64 encoding of `s "." t`

# Token verification

Given:

- `t` = a token in bytestring format

Then:

- let `n` = current POSIX time
- let `x` = base64 decoded `t` parsed according to format spec.
- if the timestamp of `x` is < `n` then fail
- let `i` = the key index part of `x`
- let `k` = the public key at index `i`
- let `y` = `x` without `<signature> "."`
- verify the ed25519 signature of `y` using `k`
