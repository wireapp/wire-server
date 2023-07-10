module API.Gundeck where

import API.Common
import Testlib.Prelude

data GetNotifications = GetNotifications
  { since :: Maybe String,
    size :: Maybe Int
  }

instance Default GetNotifications where
  def = GetNotifications {since = Nothing, size = Nothing}

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

data PostPushToken = PostPushToken
  { transport :: String,
    app :: String,
    token :: Maybe String,
    tokenSize :: Int
  }

instance Default PostPushToken where
  def =
    PostPushToken
      { transport = "GCM",
        app = "test",
        token = Nothing,
        tokenSize = 16
      }

postPushToken ::
  (HasCallStack, MakesValue u, MakesValue c) =>
  u ->
  c ->
  PostPushToken ->
  App Response
postPushToken u client args = do
  req <- baseRequest u Gundeck Versioned "/push/tokens"
  token <- maybe (randomHex (args.tokenSize * 2)) pure args.token
  c <- make client
  let t =
        object
          [ "transport" .= args.transport,
            "app" .= args.app,
            "token" .= token,
            "client" .= c
          ]
  submit "POST" $ req & addJSON t
