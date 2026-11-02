# TOTP 20 — review the per-user rate-limit bucket model

```
assignedTo: Matthias Fischmann
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4 "Rate limiting".

Nothing blocks this ticket. It is independent of the whole TOTP chain and can be picked up at any point. `TOTP 09` is what surfaces the problem, but does not depend on the fix.

Milestone 1.3 (debt).

## Background

Brig has exactly one rate-limit environment. `Brig/App.hs:301` builds it with

```haskell
rateLimitEnv <- newRateLimitEnv opts.settings.passwordHashingRateLimit
```

and `Brig/CanonicalInterpreter.hs` installs it once via `interpretRateLimit e.rateLimitEnv`. The interpreter (`libs/wire-subsystems/src/Wire/RateLimit/Interpreter.hs`) keeps an LRU of `RateLimitKey → TokenBucket`, where

```haskell
data RateLimitKey = RateLimitIp IpAddr | RateLimitUser UserId | RateLimitProvider ProviderId | RateLimitInternal
```

So **every rate-limited operation for a given user shares a single token bucket**, sized by a config option whose name says it is about password hashing. The integration defaults (`brig.integration.yaml`) are `userLimit: { burst: 5, inverseRate: 60s }` — five tokens, refilling one per minute.

That was fine while `RateLimit` was used only from `Wire.HashPassword` (`hashPassword8`, `verifyPassword`, `verifyPasswordWithStatus`). It stops being obviously fine as soon as a second, structurally different kind of operation starts drawing from the same bucket.

## The concrete trigger

TOTP verification ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4) needs rate limiting for brute-force resistance, and deliberately reuses the existing per-user bucket rather than introducing a parallel one: the guessing math works out comfortably (3 valid codes in 10⁶ per attempt at the default `skew = 3`, 5 at `skew = 5` — the numbers in [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4 — capped at ~60 attempts/hour ⇒ years to a 1% success chance), and a TOTP check is two to three orders of magnitude cheaper than the Argon2id verification in the same request.

But it exposes the coupling. A login that verifies a password **and** a TOTP code draws two tokens from a burst-5 bucket; three mistyped codes and the user gets a 429 on an otherwise correct password. `TOTP 09` works around this by charging the bucket only on *failure*. That workaround is correct, and it is still one caller reasoning about a shared resource it does not own.

Recovery codes (`TOTP 14`) sharpen it further: those verifications *are* argon2id-expensive, so the cost profile within a single bucket now spans three orders of magnitude.

## What this ticket should produce

An analysis and a proposal; implement if the proposal is cheap.

1. **Inventory.** Every current `doRateLimited` / `RateLimitKey` call site, and for each: what it protects, what an attacker gains by hammering it, what a legitimate user's retry pattern looks like, and the cost of one attempt (CPU, memory, DB roundtrips).

2. **Decide whether one shared per-user bucket is the right model.** Candidates, in increasing order of effort:
   - Keep one bucket, but make the charging policy explicit per call site (charge-on-failure vs. charge-always) and document it.
   - Partition by purpose: `RateLimitKey` gains a scope component (`RateLimitUser uid PasswordHashing`, `RateLimitUser uid SecondFactor`, …) so unrelated operations stop interfering. Costs a config change and more LRU entries per user.
   - Separate `RateLimitEnv`s per concern, configured independently.

3. **Rename the config option.** `settings.passwordHashingRateLimit` is a misnomer the moment anything else uses it. Needs a deprecation path: accept both names for a release, changelog entry, chart update.

4. **Interaction with the other failure counters.** Brig has at least one other throttle on the same user action — `decrRetryLimit` in `Brig/User/Auth.hs` on login-code failures. Unify, or document why the overlap is deliberate.

5. **Check the IP-bucket side.** `ipv4CidrBlock: 32` / `ipv6CidrBlock: 64` and `maxRateLimitedKeys: 100000`: is LRU eviction a usable attack — flood with distinct keys to evict a victim's bucket and reset their limit? Worth a paragraph either way.

## Non-goals

- Tuning the rate-limit *values*. This is about the model.
- Rate limiting at the nginz/ingress layer.

## Notes

- `checkRateLimitImpl` uses `atomicModifyIORef` over an LRU; any key-shape change must preserve that.
- `RateLimitInternal` is the "no limit" escape hatch for internal endpoints; any redesign must keep an equivalent.
- Galley and other services may have their own `RateLimitEnv`; check before assuming brig is the whole story.
