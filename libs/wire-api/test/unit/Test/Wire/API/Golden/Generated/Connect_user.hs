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
testObject_Connect_user_1 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-000100000003"))), cMessage = Just "%99", cName = Just "#", cEmail = Nothing}
testObject_Connect_user_2 :: Connect
testObject_Connect_user_2 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000400000001"))), cMessage = Just "A", cName = Just "\127828\&6:_", cEmail = Just "\SI"}
testObject_Connect_user_3 :: Connect
testObject_Connect_user_3 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000500000007"))), cMessage = Just "\11955d\NUL$\1038305\&0", cName = Just ":2?\36739M", cEmail = Nothing}
testObject_Connect_user_4 :: Connect
testObject_Connect_user_4 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000800000004"))), cMessage = Just "\110846\40600", cName = Just "", cEmail = Nothing}
testObject_Connect_user_5 :: Connect
testObject_Connect_user_5 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000005"))), cMessage = Just "1\ETB\EMj", cName = Nothing, cEmail = Just "5/p\1007239b|\NUL"}
testObject_Connect_user_6 :: Connect
testObject_Connect_user_6 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000000000004"))), cMessage = Just "\\u\987669g\57661\1024270", cName = Just "KT))Y,]", cEmail = Just "+"}
testObject_Connect_user_7 :: Connect
testObject_Connect_user_7 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000004"))), cMessage = Nothing, cName = Nothing, cEmail = Just "//"}
testObject_Connect_user_8 :: Connect
testObject_Connect_user_8 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000600000005"))), cMessage = Just "Ih!'^", cName = Just "g?\11533L_i", cEmail = Nothing}
testObject_Connect_user_9 :: Connect
testObject_Connect_user_9 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000002"))), cMessage = Nothing, cName = Just "p\1056309\&2\SUB\154882", cEmail = Just "\GS"}
testObject_Connect_user_10 :: Connect
testObject_Connect_user_10 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000600000000"))), cMessage = Just "\611-<c", cName = Just "\110697\ESCK'\1111586\21291\CAN", cEmail = Just "\1089014s):\ACK\NAK"}
testObject_Connect_user_11 :: Connect
testObject_Connect_user_11 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000000000001"))), cMessage = Just "", cName = Just "I{\SOHh\n", cEmail = Just "|P"}
testObject_Connect_user_12 :: Connect
testObject_Connect_user_12 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000800000002"))), cMessage = Just "\ETB4\DC3", cName = Nothing, cEmail = Just "\18409*0]\EOT8x"}
testObject_Connect_user_13 :: Connect
testObject_Connect_user_13 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000000000003"))), cMessage = Just "\1112928A4e\42620", cName = Nothing, cEmail = Just "A"}
testObject_Connect_user_14 :: Connect
testObject_Connect_user_14 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000600000003"))), cMessage = Nothing, cName = Just "\1103647\158697(-", cEmail = Just "Sy\1060135\78378Y"}
testObject_Connect_user_15 :: Connect
testObject_Connect_user_15 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000007"))), cMessage = Just "\t\70289y\1006132<\DELO", cName = Nothing, cEmail = Just ""}
testObject_Connect_user_16 :: Connect
testObject_Connect_user_16 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000004"))), cMessage = Nothing, cName = Nothing, cEmail = Just ""}
testObject_Connect_user_17 :: Connect
testObject_Connect_user_17 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000400000004"))), cMessage = Nothing, cName = Nothing, cEmail = Nothing}
testObject_Connect_user_18 :: Connect
testObject_Connect_user_18 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000100000003"))), cMessage = Nothing, cName = Just "T\1075012\DEL\153141 [~", cEmail = Nothing}
testObject_Connect_user_19 :: Connect
testObject_Connect_user_19 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000300000008"))), cMessage = Just "J\157350\8935n\132945", cName = Just "~@({", cEmail = Just "p$"}
testObject_Connect_user_20 :: Connect
testObject_Connect_user_20 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000100000003"))), cMessage = Just "\74197\162079\EOT#\47093:v", cName = Just "D\NAK", cEmail = Just "W"}
