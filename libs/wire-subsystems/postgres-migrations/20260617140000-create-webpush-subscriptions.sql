-- Web Push subscriptions (W3C Push API / RFC 8030 application-server side).
--
-- One row per (user, client, endpoint) tuple: a single client may hold
-- distinct subscriptions for different browser push services, and a single
-- user may hold subscriptions across multiple clients. Re-registering the
-- same endpoint upserts (refreshes keys / expiry) rather than duplicating.
--
-- The leading PRIMARY KEY column (user_id) doubles as the index for the
-- per-user dispatch lookup hot path (LookupSubscriptions).
CREATE TABLE webpush_subscriptions (
    user_id    uuid     NOT NULL,
    client_id  text     NOT NULL,    -- ClientId is a Word64 rendered as hex text
    endpoint   text     NOT NULL,    -- browser push-service URL (RFC 8030)
    p256dh     text     NOT NULL,    -- base64url ECDH P-256 pub key (65B uncompressed, RFC 8291)
    auth       text     NOT NULL,    -- base64url auth secret (16B, RFC 8291)
    expiration bigint,               -- ms since epoch; NULL = no expiry
    conn_id    bytea    NOT NULL,
    PRIMARY KEY (user_id, client_id, endpoint)
);
