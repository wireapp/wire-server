{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Connect_user where

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
testObject_Connect_user_1 :: Connect
testObject_Connect_user_1 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000200000006"))), cMessage = Just "pg`F", cName = Nothing, cEmail = Nothing}
testObject_Connect_user_2 :: Connect
testObject_Connect_user_2 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000400000003"))), cMessage = Just "\n~", cName = Just "\DEL1\NAKH\50917", cEmail = Nothing}
testObject_Connect_user_3 :: Connect
testObject_Connect_user_3 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000300000008"))), cMessage = Nothing, cName = Just "K\US\NAKq", cEmail = Just "\1098914\&4"}
testObject_Connect_user_4 :: Connect
testObject_Connect_user_4 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000400000005"))), cMessage = Just "[U", cName = Just "'1", cEmail = Just "\179162\DC4g\SYN\1035660"}
testObject_Connect_user_5 :: Connect
testObject_Connect_user_5 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000400000003"))), cMessage = Just "\ESC\994715\59656", cName = Just "Lg\CANW\DC1\US", cEmail = Nothing}
testObject_Connect_user_6 :: Connect
testObject_Connect_user_6 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000800000001"))), cMessage = Just "i7T\1069601\163824", cName = Just "", cEmail = Just "#}\124992 "}
testObject_Connect_user_7 :: Connect
testObject_Connect_user_7 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000400000000"))), cMessage = Just "\EOTz4\38620F\t|", cName = Nothing, cEmail = Just ""}
testObject_Connect_user_8 :: Connect
testObject_Connect_user_8 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000100000005"))), cMessage = Just "?\163703 \1021445o\ACK", cName = Just "'", cEmail = Just "'\DC2\US"}
testObject_Connect_user_9 :: Connect
testObject_Connect_user_9 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000600000007"))), cMessage = Just "", cName = Nothing, cEmail = Nothing}
testObject_Connect_user_10 :: Connect
testObject_Connect_user_10 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000600000000"))), cMessage = Just "ns\r\4640Y\1097507", cName = Just "m", cEmail = Nothing}
testObject_Connect_user_11 :: Connect
testObject_Connect_user_11 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000200000005"))), cMessage = Just "|", cName = Just "(\EOT'M\65819\24864\985130", cEmail = Just "\EM\US"}
testObject_Connect_user_12 :: Connect
testObject_Connect_user_12 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000300000002"))), cMessage = Just "\US4;\EM", cName = Just "\146991\n\1037604\49033\1075168\1042849!", cEmail = Nothing}
testObject_Connect_user_13 :: Connect
testObject_Connect_user_13 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000800000000"))), cMessage = Nothing, cName = Just "_\1065512V=", cEmail = Nothing}
testObject_Connect_user_14 :: Connect
testObject_Connect_user_14 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000700000000"))), cMessage = Just "!i\DELw!\164559", cName = Nothing, cEmail = Just "/\DC3&"}
testObject_Connect_user_15 :: Connect
testObject_Connect_user_15 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000006"))), cMessage = Just "", cName = Nothing, cEmail = Nothing}
testObject_Connect_user_16 :: Connect
testObject_Connect_user_16 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000800000004"))), cMessage = Nothing, cName = Just "\b\1043785\1111351\1109088\990262", cEmail = Just "\175355]p\177548\&2^)"}
testObject_Connect_user_17 :: Connect
testObject_Connect_user_17 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000600000001"))), cMessage = Just "\111135\EOTA", cName = Just "", cEmail = Just ";A<\60778"}
testObject_Connect_user_18 :: Connect
testObject_Connect_user_18 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000700000004"))), cMessage = Just "\159703f", cName = Nothing, cEmail = Just "\35565$\1076672\STX\DC3\1030641\&6"}
testObject_Connect_user_19 :: Connect
testObject_Connect_user_19 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000500000006"))), cMessage = Just "gV", cName = Just "\1091425V\SUB\1044186f", cEmail = Just "\1010296\2440!"}
testObject_Connect_user_20 :: Connect
testObject_Connect_user_20 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000400000004"))), cMessage = Just "|\ESC31;\t\r", cName = Just "*\4771nZ\10254s3", cEmail = Just ""}
