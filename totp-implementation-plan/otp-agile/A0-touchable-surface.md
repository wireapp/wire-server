# A0 — touchable surface

> **Disclaimer in force:** every endpoint returns randomly generated data and persists nothing. `GET /self/totp` does not show what you just enrolled. Activation succeeds every time, for any code.

**Demo:** `curl` an enrolment and get back a well-formed `TOTPEnrolment`; point a client at it and render a QR code from the `uri` field.

## This step is already written

It is `../otp-tickets/plan-mock-api.md`, in full. Do not duplicate it here — implement that file. It is handover-ready: it carries its own reading list, repo mechanics, traps, file:line anchors, the error table, and acceptance criteria.

Summary of what it delivers, so this directory reads end to end:

- the real types (`Wire.API.User.Auth.TOTP`), fully schema'd, with `Arbitrary` instances in the library so brig can use them;
- the real route tree (`Wire.API.Routes.Public.Brig.TOTP`), five endpoints, all `From 'V19`, all documented to the `plan-otp-07` checklist;
- the ten TOTP `BrigError` constructors, so routes carry real `CanThrow`s even though nothing throws them yet;
- handlers in `Brig/API/TOTP/Mock.hs` that answer with `QC.generate arbitrary`, with three fixups so the `otpauth://` URI is self-consistent with the `secret` in the same response;
- nginz routes in `charts/nginz/values.yaml` **and** in the integration-test nginx config, so this is reachable from a laptop and so a chart typo fails CI.

## The one decision A1 reverses

`plan-mock-api.md` argues at length against giving the mock state ("resist adding an `IORef (Map UserId [...])` since it is five lines"). That argument was correct *for A0*, where the alternative on offer was an in-memory map that would have invented semantics we had not agreed on.

**A1 overrides it** — with real storage, not an `IORef`, and with the semantics taken from `plan-otp.md` §3/§4 rather than improvised. If you are implementing A0 and are tempted to add state, do not; you are two days from being allowed to do it properly.

## Adjustment to `plan-mock-api.md` for this sequence

Its "Notes" section says the mock handler module is deleted by `TOTP 07`. In this re-cut that is **A2**. The file name stays `Brig/API/TOTP/Mock.hs` through A1 — it is still mostly lies at that point — and A2 is what replaces it with the real handler module.

## Done when

All of `plan-mock-api.md`'s acceptance criteria, unchanged. The two that matter most for the sequence:

- the V18 → 404 version-guard test, because it is the only gate and everything downstream rests on it;
- the through-nginz test, because "touchable" is the entire deliverable and a chart typo is invisible otherwise.
