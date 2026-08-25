Outbound email delivery has moved from **brig** to the **background-worker**.
brig no longer sends email directly: it inserts every outbound message
(verification, activation, password-reset, invitation, new-client,
account-deletion, SAML IdP-change, provider and enterprise-audit mail) as a
`send_email` job on the `emails` Arbiter queue (a PostgreSQL table in the
default Arbiter schema), and the background-worker performs the actual
SMTP/SES send. The queue is not routed through RabbitMQ. Operators must
configure the new `background-worker.config.email` block (SES **or** SMTP, the
same shape as brig's `emailSMS.email`) and, for SES, the worker's AWS region
and credentials (`AWS_REGION` and `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`).
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
PostgreSQL database and contain full email content (including one-time codes
and reset links, for jobs that were never delivered). Access to the database
should therefore be least-privileged, and DLQ growth should be monitored.
