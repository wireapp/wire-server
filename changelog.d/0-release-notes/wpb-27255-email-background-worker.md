Outbound email delivery has moved from **brig** to the **background-worker**.
brig no longer sends email directly: it enqueues every outbound
message (verification, activation, password-reset, invitation, new-client,
account-deletion, SAML IdP-change, provider and enterprise-audit mail) on the
existing `background-jobs` RabbitMQ queue, and the background-worker performs
the actual SMTP/SES send. Operators must configure the new
`background-worker.config.email` block (SES **or** SMTP, the same shape as
brig's `emailSMS.email`) and, for SES, the worker's AWS region and
credentials (`AWS_REGION` and `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`).
The `background-jobs` queue is a durable quorum queue, so transient
background-worker downtime does not lose mail: undelivered jobs are
requeued until a worker picks them up. When rolling out, deploy the updated
background-worker before (or alongside) the updated brig so that the new
`send-email` jobs are consumed as soon as they appear.
