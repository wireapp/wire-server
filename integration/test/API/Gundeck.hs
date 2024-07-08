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
    baseRequest user Gundeck Versioned
      $ joinHttpPath ["notifications", n]
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
  clientId <- make client & asString
  postPushToken user $ PushToken args.transport args.app token clientId

data PushToken = PushToken
  { transport :: String,
    app :: String,
    token :: String,
    client :: String
  }
  deriving (Show, Eq)

instance ToJSON PushToken where
  toJSON pt =
    object
      [ "transport" .= pt.transport,
        "app" .= pt.app,
        "token" .= pt.token,
        "client" .= pt.client
      ]

instance MakesValue PushToken where
  make = pure . toJSON

generateToken :: Int -> App String
generateToken =
  fmap (Text.unpack . Text.decodeUtf8 . Base16.encode) . randomBytes

postPushToken ::
  ( HasCallStack,
    MakesValue token,
    MakesValue user
  ) =>
  user ->
  token ->
  App Response
postPushToken user token = do
  req <- baseRequest user Gundeck Versioned "/push/tokens"
  tokenJson <- make token
  submit "POST" $ req & addJSON tokenJson

listPushTokens :: (MakesValue user) => user -> App Response
listPushTokens user = do
  req <-
    baseRequest user Gundeck Versioned
      $ joinHttpPath ["/push/tokens"]
  submit "GET" req

unregisterClient ::
  (MakesValue user, MakesValue client) =>
  user ->
  client ->
  App Response
unregisterClient user client = do
  cid <- asString client
  req <-
    baseRequest user Gundeck Unversioned
      $ joinHttpPath ["/i/clients", cid]
  submit "DELETE" req
