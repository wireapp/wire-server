# TOTP 26 — `EnsureReauthenticated` returns evidence

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 18
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6.

Milestone 1.3 (debt). Small compared to `TOTP 21`, and worth doing before it: it gives that refactor a type to converge on.

## The problem

```haskell
EnsureReauthenticated :: Local UserId -> SensitiveOperation -> ReauthBody -> AuthenticationSubsystem m ()
```

returns `()`. Nothing connects the check to the operation it is supposed to guard. Forgetting the call compiles, passes every test that does not specifically look for it, and ships an unprotected endpoint. The existing second-factor code (`TOTP 21`'s inventory) is full of exactly this failure mode, including four call sites that swallow a failed re-authentication as success.

## The change

Make the check produce a value that the guarded operation demands:

```haskell
-- Wire.AuthenticationSubsystem.Reauth (constructor NOT exported)
data Reauthenticated (op :: SensitiveOperation) = UnsafeReauthenticated (Local UserId)

reauthenticatedUser :: Reauthenticated op -> Local UserId

EnsureReauthenticated
  :: Local UserId
  -> Sing op              -- or a proxy/`SSensitiveOperation`, whatever the codebase prefers
  -> ReauthBody
  -> AuthenticationSubsystem m (Reauthenticated op)
```

and then every sensitive operation takes it:

```haskell
deleteSelf :: Reauthenticated 'OpDeleteSelf -> ... -> Sem r ()
```

Three properties, each of which is the reason for one design detail:

1. **Unforgeable.** The constructor is not exported, so the only way to obtain a `Reauthenticated` is to have called the check. Put it in a module whose name says so, and if an escape hatch is needed for tests, name it `unsafeReauthenticatedForTests` and let that be loud in review.
2. **Operation-specific.** The phantom `op` (DataKinds over the existing `SensitiveOperation`) stops evidence collected for a cheap operation being passed to an expensive one — the bug where a handler that reauthenticates for one purpose reuses the result for another, several refactors later.
3. **Bound to a user.** It carries the `Local UserId` it was issued for, and the operation uses *that* rather than a separately-threaded id. Evidence for user A authorising an action on user B is the failure mode worth the most here, and it is purely a matter of which value the operation reads.

### What it is not

Not a token, not serialisable, not storable, no expiry, never crosses a process boundary. It is an in-process proof that a check ran earlier in *this* request. If it ever grows a `ToJSON` instance, it has become a capability token with an unexamined lifetime, and that is a different (worse) design. Say so in the module header.

It also does not prove *recency* beyond "earlier in this request". If some operation needs "within the last five minutes", that is a timestamp in the evidence and a check at the use site, and it should be added when something actually needs it.

## Scope

- The type, the effect signature change, and the mechanical update of every `EnsureReauthenticated` call site from `TOTP 11` and `TOTP 18`.
- Threading the value into the functions that perform the operations — this is the bulk of the work and the whole value of the ticket. A `Reauthenticated` that is produced and discarded two lines later proves nothing; the argument has to reach the function that does the irreversible thing.
- Where that turns out to be impractical for a given operation, write down why rather than quietly dropping the evidence on the floor.

## Acceptance criteria

- `cabal build` clean across brig, galley and spar.
- A negative compilation test (or a documented, reviewed example) showing that calling a guarded operation without evidence does not compile, and that evidence for the wrong `op` does not either. Without this, the invariant quietly rots the first time someone adds a handler.
- No behaviour change: the full existing re-authentication test matrix passes unchanged. This is a types-only refactor and any behavioural diff is a bug in it.
- The constructor is exported from exactly one module and that module's export list is reviewed.
- `grep` for remaining operations that perform a sensitive action without taking evidence; the leftovers become the input to `TOTP 21`.

## Does this ticket make sense?

Yes — with the caveat that its value is proportional to how far the evidence is threaded. As a one-line type change it is decoration. As "every irreversible operation takes proof in its signature" it converts a class of silent security bugs into compile errors, and `TOTP 21`'s inventory shows that class is not hypothetical in this codebase.

Two things to watch:

- Do not let it balloon into `TOTP 21`. This ticket changes types; that one changes semantics. If a call site turns out to have *wrong* behaviour rather than merely unproven behaviour, note it and leave it for 21.
- Polysemy effect signatures returning a phantom-indexed type can get awkward around `makeSem` and higher-order interpreters. Spike that shape first, on one operation, before converting fifteen.
