module API.Gundeck where

import Testlib.Prelude

data GetNotifications = GetNotifications
  { since :: Maybe String,
    size :: Maybe Int
  }

instance Default GetNotifications where
  def = GetNotifications {since = Nothing, size = Nothing}

postPushV2 ::
  ( HasCallStack,
    MakesValue user,
    MakesValue a
  ) =>
  user ->
  [a] ->
  App Response
postPushV2 user payloads = do
  req <- baseRequest user Gundeck Unversioned "/i/push/v2"
  body <- traverse make payloads
  submit "POST" $ req & addJSON body

getNotifications ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  user ->
  client ->
  GetNotifications ->
  App Response
getNotifications user client r = do
  c <- client & asString
  req <- baseRequest user Gundeck Versioned "/notifications"
  let req' =
        req
          & addQueryParams
            ( [("since", since) | since <- toList r.since]
                <> [("client", c)]
                <> [("size", show size) | size <- toList r.size]
            )
  submit "GET" req'

getNotification ::
  (HasCallStack, MakesValue user, MakesValue client, MakesValue nid) =>
  user ->
  client ->
  nid ->
  App Response
getNotification user client nid = do
  c <- client & asString
  n <- nid & asString
  req <-
    baseRequest user Gundeck Versioned $
      joinHttpPath ["notifications", n]
  submit "GET" $ req & addQueryParams [("client", c)]

getLastNotification ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  user ->
  client ->
  App Response
getLastNotification user client = do
  c <- client & asString
  req <-
    baseRequest user Gundeck Versioned "/notifications/last"
  submit "GET" $ req & addQueryParams [("client", c)]
