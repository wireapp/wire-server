<!--  Referenced from /libs/wire-api-federation/src/Wire/API/Federation/API/Galley.hs -->
<!--  Referenced from /libs/wire-api-federation/src/Wire/API/Federation/API/Brig.hs -->
# Federation API Conventions

- All endpoints must start with `/federation/`
- All path segments must be in kebab-case. The name the field in the record must
  be the same name in camelCase.
- There can be either one or two path segments after `/federation/`, so
  `/federation/foo` is valid, `/fedeartion/foo/bar` is valid, but
  `/federation/foo/bar/baz` is not.
- All endpoints must be `POST`.
- No query query params or captured path params, all information that needs to
  go must go in body.
- All responses must be `200 OK`, domain specific failures (e.g. the
  conversation doesn't exist) must be indicated as a Sum type. Unhandled
  failures can be 5xx, an endpoint not being implemented will of course
  return 404. But we shouldn't pile onto these. This keeps the federator simple.
- Accept only json, respond with only json. Maybe we can think of changing
  this in future. But as of now, the federator hardcodes application/json as
  the content type of the body.
- Ensure that paths don't collide between brig and galley federation API, this
  will be very helpful when we merge brig and galley.
- Name of the first path segment after `/federation/` must be either
  `<imperative-verb>-<object>` or `on-<subject>-<past-tense-verb>`, e.g.
  `get-conversations` or `on-conversation-created`.

  How to decide which one to use:
  - If the request is supposed to ask for information/change from another
    backend which has authority over that information, like send a message or
    leave a conversation, then use the first format like `send-message` or
    `leave-conversation`.
  - If the request is supposed to notify a backend about something the caller of
    this request has authority on, like a conversation got created, or a message
    is sent, then use the second format like `on-conversation-created` or
    `on-message-sent`
- Path segment number 3 (so `/federation/not-this/but-this-one`), must only be
  used in exceptional circumstances, like when there needs to be the same path
  in brig and galley, e.g. `on-user-deleted`. In this case use the third segment
  to express the difference. For `on-user-deleted` we came up with
  `on-user-deleted/connections`for brig and `on-user-deleted/conversations` for
  galley.
