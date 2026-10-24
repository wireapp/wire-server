# A6 — reauth everywhere

**Demo:** with an authenticator enrolled, change your password. You are asked for a code. Same for deleting your account, removing a device, and changing your email.

Independent of A7. Depends only on A3 (`EnsureReauthenticated` must exist). Can run in parallel with A5, though landing it after A5 is tidier — these endpoints are not where the interesting attacks are, and A5 is what makes the mechanism they call trustworthy.

## Scope

A3 defined the full `SensitiveOperation` enumeration and wired three of its constructors. This step wires the rest. `plan-otp-18`, `plan-otp.md` §6.

Each one takes the `Reauthenticated op` evidence A3 introduced, threaded as far as the function that performs the irreversible action — not produced and discarded in the handler prologue. Because A3 built the type rather than deferring it to `plan-otp-26`, this is seven ordinary additions rather than a cross-service refactor, and a handler that forgets the check does not compile.

| endpoint | operation |
|---|---|
| `DELETE /self` | `OpDeleteSelf` |
| `DELETE /clients/:id` | `OpDeleteClient` |
| `PUT /self/password` | `OpChangePassword` |
| email change | `OpChangeEmail` |
| OAuth access revocation | `OpRevokeOAuthAccess` |
| `GET /i/users/:uid/reauthenticate` | `OpCreateScimToken`, `OpDeleteTeam` — galley and spar call this |
| `POST /password-reset/complete` | see below |

Two of these have no natural request body today. Add one. **Never a query parameter** — a code in the URL is written to access logs at every proxy hop, and OTPs must not be logged (OWASP). **Never a custom header** — nginz rewrites `Z-*` headers, and a non-`Z-` header is easy to drop in a proxy chain. §6.

## Password reset

`plan-otp.md` §6 "Password reset" and D16. The analysis matters more than the code, so read it rather than working from this summary.

The load-bearing property is already true and currently unwritten: `POST /password-reset/complete` returns 200 with an empty body and issues **no cookie**, so email possession alone yields a changed password and never a session. The user must still log in, and that login is challenged normally. **Add the regression test for this before adding anything else** — it is the property that actually prevents the bypass, and it is one refactor away from quietly breaking.

On top of it, `CompletePasswordReset` gains an optional `totp_code`. Four details that are easy to get wrong:

- **Gate per-user, not per-site.** The condition is "this user has an activated credential", not the site-level `totp.enabled`. Gating on the site flag demands a code from users who never enrolled.
- **Recovery codes count**, because the field is `SecondFactorCode`. "Lost my phone and forgot my password" must stay recoverable.
- **Ordering:** validate the emailed reset code first. A wrong one never consumes a TOTP step or burns a recovery code.
- **No enumeration concern here**, unlike at login: the caller has already proved mailbox possession with a valid reset code, so `totp-required` at this point discloses nothing new. Do not copy A3's oracle-avoidance reasoning into this endpoint and conclude the wrong thing.

## Not in this step

`plan-otp-21`, the second-factor cleanup that unifies TOTP with the existing emailed `verificationCode` mechanism. It is a large refactor with no user-visible effect and it must not be smuggled in here. The two mechanisms coexist; A3 documented the precedence rule and this step must not quietly change it.

Also leave `Brig/User/Auth.hs:408-427` (`ssoLogin`) alone. It swallows several `reauthenticateEither` error cases as success. Do not extend the pattern and do not fix it here.

## Done when

- Each endpoint in the table above challenges a user with an activated credential and is unchanged for a user without one. The second half is the regression test that matters.
- The password-reset regression test described above, plus tests for the four details.
- SSO accounts behave per D15 at every one of these endpoints — they have no password and no credential, so the exclusive rule must hold here too and not just at enrolment.
- `changelog.d/` entry. Operators and client teams both need to know that a set of previously password-only operations now take an optional second factor.
