-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Native where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Id (ClientId (..), ConnId (..), UserId, randomId)
import qualified Data.List1 as List1
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Imports
import Network.AWS (Region (Ireland))
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Native"
    [ testProperty "serialise/ok" $
        -- this may fail sporadically, but that's not a production issue.
        -- see <https://github.com/wireapp/wire-server/issues/341>.
        forAll genTransport serialiseOkProp
    ]

serialiseOkProp :: Transport -> Property
serialiseOkProp t = ioProperty $ do
  a <- mkAddress t
  n <- randNotif (0, 1280)
  m <- randMessage n
  r <- serialise m (a ^. addrUser) (a ^. addrTransport)
  let sn = either (const Nothing) Just r >>= decode' . LT.encodeUtf8
  let equalTransport = fmap snsNotifTransport sn == Just t
  equalNotif <- case snsNotifBundle <$> sn of
    Nothing -> return False
    Just (NoticeBundle n') -> return $ ntfId n == n'
  let debugInfo = (t, a, n, r, sn, equalTransport, equalNotif)
  return . counterexample (show debugInfo) $ equalTransport && equalNotif

-----------------------------------------------------------------------------
-- Types

data SnsNotification = SnsNotification
  { snsNotifTransport :: !Transport,
    snsNotifData :: !SnsData
  }
  deriving (Eq, Show)

instance FromJSON SnsNotification where
  parseJSON = withObject "SnsNotification" $ \o ->
    case HashMap.toList o of
      [("GCM", String n)] -> parseGcm n
      [("APNS", String n)] -> parseApns APNS n
      [("APNS_SANDBOX", String n)] -> parseApns APNSSandbox n
      [("APNS_VOIP", String n)] -> parseApns APNSVoIP n
      [("APNS_VOIP_SANDBOX", String n)] -> parseApns APNSVoIPSandbox n
      _ -> mempty
    where
      parseApns t n =
        let apn = decodeStrict' (T.encodeUtf8 n)
         in maybe mempty (pure . SnsNotification t . SnsApnsData) apn
      parseGcm n =
        let gcm = decodeStrict' (T.encodeUtf8 n)
         in maybe mempty (pure . SnsNotification GCM . SnsGcmData) gcm

data SnsData
  = SnsGcmData !GcmData
  | SnsApnsData !ApnsData
  deriving (Eq, Show)

snsNotifBundle :: SnsNotification -> Bundle
snsNotifBundle n = case snsNotifData n of
  SnsGcmData d -> gcmBundle d
  SnsApnsData d -> apnsBundle d

data GcmData = GcmData
  { gcmPriority :: !Text,
    gcmBundle :: !Bundle
  }
  deriving (Eq, Show)

instance FromJSON GcmData where
  parseJSON = withObject "GcmData" $ \o ->
    GcmData <$> o .: "priority"
      <*> o .: "data"

data ApnsData = ApnsData
  { apnsMeta :: !Object,
    apnsBundle :: !Bundle
  }
  deriving (Eq, Show)

instance FromJSON ApnsData where
  parseJSON = withObject "ApnsData" $ \o ->
    ApnsData <$> o .: "aps"
      <*> o .: "data"

newtype Bundle = NoticeBundle NotificationId
  deriving (Eq, Show)

instance FromJSON Bundle where
  parseJSON = withObject "Bundle" $ \o ->
    case HashMap.lookup "type" o of
      Just (String "notice") -> case HashMap.lookup "data" o of
        Just (Object o') -> NoticeBundle <$> o' .: "id"
        _ -> mempty
      _ -> mempty

data PlainData = PlainData
  { plainNotif :: !Notification,
    plainUser :: !(Maybe UserId)
  }
  deriving (Eq, Show)

instance FromJSON PlainData where
  parseJSON = withObject "PlainData" $ \o ->
    PlainData <$> o .: "data" <*> o .:? "user"

-----------------------------------------------------------------------------
-- Randomness

genTransport :: Gen Transport
genTransport = elements [minBound ..]

randNotif :: (Int, Int) -> IO Notification
randNotif size = do
  i <- randomId
  generate $ do
    l <- choose size
    v <- T.pack <$> vectorOf l (elements ['a' .. 'z'])
    let pload = List1.singleton (HashMap.fromList ["data" .= v])
    Notification i <$> arbitrary <*> pure pload

randMessage :: Notification -> IO NativePush
randMessage n = pure $ NativePush (ntfId n) HighPriority Nothing

-----------------------------------------------------------------------------
-- Utilities

mkAddress :: Transport -> IO Address
mkAddress t =
  Address
    <$> randomId
    <*> pure (mkEndpoint t (AppName "test"))
    <*> pure (ConnId "conn")
    <*> pure (pushToken t (AppName "test") (Token "test") (ClientId "client"))

mkEndpoint :: Transport -> AppName -> EndpointArn
mkEndpoint t a = mkSnsArn Ireland (Account "test") topic
  where
    topic = mkEndpointTopic (ArnEnv "test") t a (EndpointId "test")
