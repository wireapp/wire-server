# TOTP, re-cut for delivery

`otp-tickets/` is the engineering plan: 27 tickets, layered bottom-up — types, store, crypto, subsystem, config, endpoints, then feature after feature. It is a good decomposition of the *problem*. It is a bad decomposition of the *work*, because nothing is visible to anyone outside the backend team until about ticket 7, and nothing a user would recognise as "two-factor authentication" exists until about ticket 11.

This directory re-cuts the same work outside-in. Each step ends in something a person can touch, demo, and give feedback on. The backend lies to begin with, and each step replaces one lie with the truth.

`otp-tickets/` stays as-is and stays authoritative on *how* each piece works. These files say *in what order*, *how far to fake it*, and *when to stop faking*. Where they disagree on sequencing, this directory wins; where they disagree on design, `otp-tickets/plan-otp.md` wins.

---

## The thing that makes this possible

All new endpoints are `From 'V19`, and `V19` is a development version (`isDevelopmentVersion V19 = True`). `charts/wire-server/values.yaml` ships `setDisabledAPIVersions: [development]`, so a default production deployment answers `404 unsupported-version` on every one of them. Staging drops `development` from that list and gets the lot.

That single switch is what buys the right to ship half-built security. We are not shipping a weak second factor to users; we are shipping a *toy* to a backend where nobody can reach it except people who were told it is a toy.

### The invariant

> **The only way to obtain an activated TOTP credential is through a `From 'V19` endpoint.**

This is what makes the gate hold *transitively*, and it is not obvious, so it is written here rather than left to be rediscovered. `POST /login` is **not** version-gated — it cannot be, it is how everyone logs in. From step **A3** onward it will reject a login that lacks a second factor when the user has one. On a production backend that branch is unreachable, because the user could never have enrolled in the first place.

Break the invariant — add an internal endpoint, a migration, a support tool, a test fixture that creates an activated credential outside V19 — and production users start getting challenged for a factor they never enrolled and cannot produce. Nothing else in this plan is as load-bearing as this one sentence.

### What we tell client developers, every step

Three things, repeated until step A5 lands:

1. **Staging only, `/v19` only.** If you get a 404, the backend has development versions disabled.
2. **Your secrets are not safe.** Until A5, TOTP secrets are stored unencrypted. Enrol a throwaway authenticator entry, not the one guarding anything you care about.
3. **The current step's disclaimer**, which is stated at the top of each file below and changes as we go.

---

## The sequence

| | step | the demo at the end | fakes remaining |
|---|---|---|---|
| **A0** | [Touchable surface](A0-touchable-surface.md) | `curl` returns a parseable enrolment; a client renders a QR from it | everything |
| **A1** | [It remembers](A1-it-remembers.md) | enrol on one device, see it listed on another; rename and delete stick | codes not checked; secrets in plaintext |
| **A2** | [Real codes](A2-real-codes.md) | scan the QR with Google Authenticator; the real code is accepted, a wrong one is not | no replay protection, no skew handling, plaintext secrets |
| **A3** | [Login challenge](A3-login-challenge.md) | log out, log in, get challenged, type the code from your phone, get in | as A2, plus no recovery path |
| **A4** | [Recovery codes](A4-recovery-codes.md) | lose your phone, log in with a recovery code | as A2 |
| **A5** | [Harden](A5-harden.md) | **nothing visible** — a pen-test checklist going green | none; this is the production gate |
| **A6** | [Reauth everywhere](A6-reauth-everywhere.md) | change your password, get challenged | — |
| **A7** | [Team enforcement](A7-team-enforcement.md) | a team admin flips a switch; members are told to enrol | — |

A0–A4 are strictly sequential: each one needs the previous one's state to be demonstrable at all. **A5 is a parallel bucket**, not a sequence — its five items are independent, invisible to clients, and can land in any order by any number of people. A6 and A7 are independent of each other and both depend only on A3.

### The two gates

"General availability" here is not an abstraction. It is the moment `development` comes out of `setDisabledAPIVersions` on production backends, so `/v19` — and with it every endpoint in this plan — becomes reachable by real users. Two separate things have to be true before that, and it is worth keeping them apart because it is tempting to treat the first as the finish line.

- **A5 is the security gate.** After it the feature is defensible in front of a reviewer: secrets encrypted at rest, replay protection, clock skew handled, rate limiting. Nothing before A5 may be reachable in production under any circumstances.
- **A7 group 1 is the general-availability gate.** Recovery-code regeneration and status, admin reset, the ops runbook. None of it is a security property — A5 covered those — but without it every user who loses a phone or burns their recovery codes becomes a manual support escalation. After A5 and before A7 group 1, the feature is *safe* and *unshippable*.

A6 and A7 groups 2–3 are neither gate. They can land after general availability.

### Where the old tickets go

| agile step | absorbs |
|---|---|
| A0 | `plan-mock-api.md` (which already absorbed `plan-otp-01` and the servant half of `07`, and the nginz half of `08`) |
| A1 | `plan-otp-02` (store), `plan-otp-05` (subsystem, partial), `plan-otp-06` (config, partial), `plan-otp-12` (delete) |
| A2 | `plan-otp-04` (test helper), `plan-otp-07` (real handlers), `plan-otp-09` (verification, partial) |
| A3 | `plan-otp-11` (reauth + login) |
| A4 | `plan-otp-13` (recovery codes), `plan-otp-14` (recovery login) |
| A5 | `plan-otp-03` (encryption), `plan-otp-09` (replay), `plan-otp-10` (clock skew), `plan-otp-16` (sweep), `plan-otp-20` (rate limits) |
| A6 | `plan-otp-18` (remaining operations), password reset (`plan-otp.md` §6) |
| A7 | `plan-otp-19`, `22`, `23`, `24`, `25`; `plan-otp-15` (admin reset), `plan-otp-17` (recovery management), the `docs/` half of `plan-otp-08` |
| — | `plan-otp-21` (second-factor cleanup) is a refactor of pre-existing mess with no user-visible effect. Not in this sequence; schedule it whenever, or never. |

`plan-otp-26` (reauth evidence) is **not** on that list, and the temptation to put it there is the mistake. It is written as a later refactor of fifteen call sites, and `plan-otp-11` explicitly ships `EnsureReauthenticated` returning `()` with a note that 26 will fix it. That is backwards: A3 is where the type is *born*, with three call sites, and building it right there costs a fraction of converting it afterwards. It is folded into A3.

## What `otp-tickets/` is still for

This directory is a **sequencing layer**, not a replacement. It says in what order, how far to fake it, and when to stop faking. It does not restate the engineering detail, and it should not — every one of the 26 tickets is referenced from these files and remains authoritative on *how* its piece works: acceptance criteria, test lists, design rationale, and the arguments for decisions that look arbitrary until you read them.

So: read the A-file for scope and sequencing, then read the tickets it names for the detail. `plan-otp.md` is the spec behind both.

One file there is actively misleading now. **`otp-tickets/BLOCKERS.md` encodes the old bottom-up dependency graph** — `05` blocked by `03`, `07` blocked by `06`, and so on. Those were real dependencies for the old ordering and they are *not* the constraints here; this re-cut deliberately violates several of them (A1 ships storage without encryption, which `BLOCKERS.md` has `05` waiting on `03` to avoid). Treat it as a historical artefact. The dependency statement that applies is the one above: A0–A4 sequential, A5 a parallel bucket, A6 and A7 independent and depending only on A3.

`otp-tickets/TICKET_IDS.csv` has no rows for `plan-mock-api.md` or for any file in this directory. Someone will need to cut tickets for A0–A7 and add them.

---

## Rules for the faking

The point of a lie is that it is cheap and that it is removed. Three rules keep it that way.

**Fake behaviour, never shape.** A response may contain nonsense, but it must be the right *type*, with the right field names, at the right status code. A client that integrates against a faked step must need **zero** changes when the step becomes real. Every time we have faked a shape instead of a behaviour, we have paid for it twice.

**Every lie has a deletion address.** Each step below lists what it knowingly ships broken, and which later step removes it. A lie with no removal date is a bug that has been given a nicer name.

**The disclaimer is in the OpenAPI document, not just in Slack.** Each route's `Description` carries a line stating what it currently does not do. That line is edited by the step that fixes it. A client developer reading the generated docs must not have to know which Slack thread was current.

## Keeping it honest

One integration test, added in A1 and updated by every step, asserts the *current* fake state explicitly — e.g. in A1, "activation accepts `000000`". When a later step makes that false, the test fails, and whoever fixes it is forced to notice they have changed the security posture. A test that asserts a weakness is an odd thing to write; it is the cheapest alarm we have against a lie outliving its deletion date.
