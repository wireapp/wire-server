* Galley has a new optional `settings.meetings.email` configuration block
  (WPB-27175) for sending meeting-invitation emails to invited external
  addresses. It takes a required `from` sender, an optional `replyTo` address,
  and a `transport` that selects AWS SES or SMTP (the same shape Brig uses).
  When the block is unset, meeting invitation emails are disabled. For SMTP,
  set `galley.secrets.smtpPassword` (mounted at
  `/etc/wire/galley/secrets/smtp-password.txt`). This change adds the
  configuration plumbing only; email sending itself lands in a follow-up.
