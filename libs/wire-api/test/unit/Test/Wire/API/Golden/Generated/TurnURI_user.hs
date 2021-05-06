{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TurnURI_user where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_TurnURI_user_1 :: TurnURI
testObject_TurnURI_user_1 = (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "6") (Just TransportTCP))
testObject_TurnURI_user_2 :: TurnURI
testObject_TurnURI_user_2 = (turnURI (SchemeTurns) (TurnHostName "007.com") (read "8") (Just TransportTCP))
testObject_TurnURI_user_3 :: TurnURI
testObject_TurnURI_user_3 = (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing))
testObject_TurnURI_user_4 :: TurnURI
testObject_TurnURI_user_4 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "119.60.78.65"))) (read "2") (Nothing))
testObject_TurnURI_user_5 :: TurnURI
testObject_TurnURI_user_5 = (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "7") (Just TransportTCP))
testObject_TurnURI_user_6 :: TurnURI
testObject_TurnURI_user_6 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "34.51.173.116"))) (read "8") (Just TransportUDP))
testObject_TurnURI_user_7 :: TurnURI
testObject_TurnURI_user_7 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "40.2.91.92"))) (read "5") (Nothing))
testObject_TurnURI_user_8 :: TurnURI
testObject_TurnURI_user_8 = (turnURI (SchemeTurns) (TurnHostName "007.com") (read "3") (Just TransportUDP))
testObject_TurnURI_user_9 :: TurnURI
testObject_TurnURI_user_9 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "167.114.219.170"))) (read "9") (Just TransportUDP))
testObject_TurnURI_user_10 :: TurnURI
testObject_TurnURI_user_10 = (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP))
testObject_TurnURI_user_11 :: TurnURI
testObject_TurnURI_user_11 = (turnURI (SchemeTurn) (TurnHostName "007.com") (read "5") (Nothing))
testObject_TurnURI_user_12 :: TurnURI
testObject_TurnURI_user_12 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "250.102.177.220"))) (read "2") (Nothing))
testObject_TurnURI_user_13 :: TurnURI
testObject_TurnURI_user_13 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "138.30.103.64"))) (read "8") (Just TransportTCP))
testObject_TurnURI_user_14 :: TurnURI
testObject_TurnURI_user_14 = (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportTCP))
testObject_TurnURI_user_15 :: TurnURI
testObject_TurnURI_user_15 = (turnURI (SchemeTurns) (TurnHostName "host.name") (read "2") (Just TransportTCP))
testObject_TurnURI_user_16 :: TurnURI
testObject_TurnURI_user_16 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "83.60.6.222"))) (read "8") (Nothing))
testObject_TurnURI_user_17 :: TurnURI
testObject_TurnURI_user_17 = (turnURI (SchemeTurn) (TurnHostName "123") (read "7") (Just TransportUDP))
testObject_TurnURI_user_18 :: TurnURI
testObject_TurnURI_user_18 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "38.49.43.110"))) (read "1") (Just TransportUDP))
testObject_TurnURI_user_19 :: TurnURI
testObject_TurnURI_user_19 = (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "64.81.18.159"))) (read "3") (Just TransportUDP))
testObject_TurnURI_user_20 :: TurnURI
testObject_TurnURI_user_20 = (turnURI (SchemeTurns) (TurnHostName "a-c") (read "4") (Nothing))
