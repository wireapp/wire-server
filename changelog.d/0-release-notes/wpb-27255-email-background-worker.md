Outbound email delivery has moved from **brig** to the **background-worker**,
and the queue now carries the *composing payload* instead of a finished mail:
brig no longer renders templates or builds MIME mail. It enqueues every
outbound email (verification, activation, password-reset, invitation,
new-client, account-deletion, SAML IdP-change, provider and enterprise-audit
mail) as a `send_email` job on the `emails` Arbiter queue (a PostgreSQL table
in the default Arbiter schema), carrying only the email type, locale and
structured inputs (recipient, keys/codes, team names, certificate summaries,
...). The background-worker composes the email — locale template selection,
placeholder rendering, MIME building — from the localized templates bundled in
its image (`/usr/share/wire/templates`) right before performing the actual
SMTP/SES send. The queue is not routed through RabbitMQ.

Operators must configure two blocks on the background-worker:

- `background-worker.config.email` — the transport (SES **or** SMTP, the same
  shape as brig's former `emailSMS.email`); for SES also the worker's AWS
  region and credentials (`AWS_REGION` and
  `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`).
- `background-worker.config.emailTemplates` — the template directory, default
  locale, sender address, branding and the user/team/provider URL templates.
  These were previously brig's `emailSMS` template/URL/branding settings
  (`emailSMS.general.templateDir`, `emailSMS.general.templateBranding`,
  `emailSMS.user.{activation,passwordReset,deletion}Url` and
  `emailSMS.provider`); those brig keys are gone, and the worker values must
  match what brig used to configure so emails render with the same URLs and
  branding as before. The templates directory now ships in the
  background-worker image.

Failed sends are retried by Arbiter with bounded exponential backoff and
eventually land in the queue's dead-letter table, so transient
background-worker downtime does not lose mail: jobs stay in the `emails` table
until a worker picks them up.

When rolling out, deploy the updated background-worker before (or alongside)
the updated brig so that the new `send_email` jobs are consumed as soon as they
appear; both services run the Arbiter migrations that create the `emails`
table at startup. This ordering assumes no intermediate build that queued email
on RabbitMQ is still running: `send-email` messages on the `background-jobs`
queue are requeued forever by an updated worker (which no longer understands
them). If such a build ran anywhere, drain or delete residual `send-email`
messages from the `background-jobs` queue before upgrading the worker.

Note: the `emails` queue and its dead-letter table live in the shared
PostgreSQL database and contain the queued request data (including one-time
codes, recipient addresses and reset URLs, for jobs that were never delivered).
Access to the database should therefore be least-privileged, and DLQ growth
should be monitored.
