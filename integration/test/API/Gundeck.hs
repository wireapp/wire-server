module API.Gundeck where

import API.Common
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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

data GeneratePushToken = GeneratePushToken
  { transport :: String,
    app :: String,
    tokenSize :: Int
  }

instance Default GeneratePushToken where
  def =
    GeneratePushToken
      { transport = "GCM",
        app = "test",
        tokenSize = 16
      }

generateAndPostPushToken ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  user ->
  client ->
  GeneratePushToken ->
  App Response
generateAndPostPushToken user client args = do
  token <- generateToken args.tokenSize
  clientJson <- make client
  postPushToken user $
    object
      [ "transport" .= args.transport,
        "app" .= args.app,
        "token" .= token,
        "client" .= clientJson
      ]

generateToken :: Int -> App String
generateToken =
  fmap (Text.unpack . Text.decodeUtf8 . Base16.encode) . randomBytes

postPushToken ::
  (HasCallStack, MakesValue user) =>
  user ->
  Value ->
  App Response
postPushToken user token = do
  req <- baseRequest user Gundeck Versioned "/push/tokens"
  submit "POST" $ req & addJSON token
