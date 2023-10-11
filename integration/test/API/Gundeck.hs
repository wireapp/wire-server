module API.Gundeck where

import API.Common
import Testlib.Prelude

data GetNotifications = GetNotifications
  { since :: Maybe String,
    size :: Maybe Int,
    client :: Maybe String
  }

instance Default GetNotifications where
  def = GetNotifications {since = Nothing, size = Nothing, client = Nothing}

getNotifications ::
  (HasCallStack, MakesValue user) =>
  user ->
  GetNotifications ->
  App Response
getNotifications user r = do
  req <- baseRequest user Gundeck Versioned "/notifications"
  let req' =
        req
          & addQueryParams
            ( [("since", since) | since <- toList r.since]
                <> [("client", c) | c <- toList r.client]
                <> [("size", show size) | size <- toList r.size]
            )
  submit "GET" req'

data GetNotification = GetNotification
  { client :: Maybe String
  }

instance Default GetNotification where
  def = GetNotification Nothing

getNotification ::
  (HasCallStack, MakesValue user, MakesValue nid) =>
  user ->
  GetNotification ->
  nid ->
  App Response
getNotification user opts nid = do
  n <- nid & asString
  req <-
    baseRequest user Gundeck Versioned $
      joinHttpPath ["notifications", n]
  submit "GET" $ req & addQueryParams [("client", c) | c <- toList opts.client]

getLastNotification ::
  (HasCallStack, MakesValue user) =>
  user ->
  GetNotification ->
  App Response
getLastNotification user opts = do
  req <-
    baseRequest user Gundeck Versioned "/notifications/last"
  submit "GET" $ req & addQueryParams [("client", c) | c <- toList opts.client]

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
