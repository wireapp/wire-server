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
testObject_Connect_user_1 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000400000007"))), cMessage = Nothing, cName = Just "`M\174664", cEmail = Just "Dl\993640\DC3\59668>\1044006"}
testObject_Connect_user_2 :: Connect
testObject_Connect_user_2 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000002"))), cMessage = Just "\r\v'8Jf", cName = Just "\13591\SUB\ryD\120159\&4", cEmail = Just "0"}
testObject_Connect_user_3 :: Connect
testObject_Connect_user_3 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000600000005"))), cMessage = Just "\1072520*5\142203", cName = Just "\1021293R\4172.", cEmail = Just "\EMCXp\FSy"}
testObject_Connect_user_4 :: Connect
testObject_Connect_user_4 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000500000001"))), cMessage = Nothing, cName = Just "\1109909\r", cEmail = Just "i\110806\52435\1089789"}
testObject_Connect_user_5 :: Connect
testObject_Connect_user_5 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000500000003"))), cMessage = Just "8}P", cName = Just "*T\1084470", cEmail = Just "gv"}
testObject_Connect_user_6 :: Connect
testObject_Connect_user_6 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000600000002"))), cMessage = Just "\917780O(\EOTk\1067626", cName = Just "N\RS`", cEmail = Just "'\989484f\1075329\25274"}
testObject_Connect_user_7 :: Connect
testObject_Connect_user_7 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000500000001"))), cMessage = Just "Q\DC3f\\{", cName = Just "nRCe(\\", cEmail = Just "P8\CANEq"}
testObject_Connect_user_8 :: Connect
testObject_Connect_user_8 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000200000005"))), cMessage = Just "", cName = Nothing, cEmail = Nothing}
testObject_Connect_user_9 :: Connect
testObject_Connect_user_9 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000100000006"))), cMessage = Just "\69922K\170944K\DC2", cName = Just "\SOH\158114[s\10766", cEmail = Just "\1070098\1047413KA"}
testObject_Connect_user_10 :: Connect
testObject_Connect_user_10 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000300000006"))), cMessage = Nothing, cName = Nothing, cEmail = Just ""}
testObject_Connect_user_11 :: Connect
testObject_Connect_user_11 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000400000005"))), cMessage = Just "\1037918\1112090\\", cName = Just "\161921s&\1073787\FS[", cEmail = Just "\ESC"}
testObject_Connect_user_12 :: Connect
testObject_Connect_user_12 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000002"))), cMessage = Just "\SI1\13650\24089\f\DEL\1017353", cName = Just "$X\11161\22183OA", cEmail = Just "^YM\ACKY\SUB\1016511"}
testObject_Connect_user_13 :: Connect
testObject_Connect_user_13 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000004"))), cMessage = Just "\ESCQ4\\\98593", cName = Just "\1021019|;j2t\1006228", cEmail = Just "b&\EM"}
testObject_Connect_user_14 :: Connect
testObject_Connect_user_14 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000700000006"))), cMessage = Just "\SUB\STX|\nF=\1020950", cName = Nothing, cEmail = Nothing}
testObject_Connect_user_15 :: Connect
testObject_Connect_user_15 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000800000005"))), cMessage = Just "6m", cName = Nothing, cEmail = Just ""}
testObject_Connect_user_16 :: Connect
testObject_Connect_user_16 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-000800000008"))), cMessage = Nothing, cName = Just ",h\v\1036044", cEmail = Nothing}
testObject_Connect_user_17 :: Connect
testObject_Connect_user_17 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000400000001"))), cMessage = Just "@\176655\SYN]a", cName = Just "", cEmail = Just "c\1015405Z"}
testObject_Connect_user_18 :: Connect
testObject_Connect_user_18 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000000000000"))), cMessage = Nothing, cName = Just "O*", cEmail = Just "/"}
testObject_Connect_user_19 :: Connect
testObject_Connect_user_19 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000100000006"))), cMessage = Nothing, cName = Nothing, cEmail = Nothing}
testObject_Connect_user_20 :: Connect
testObject_Connect_user_20 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003"))), cMessage = Just "\65063", cName = Just ":\1088738\1022096\&7", cEmail = Just "_e%\1040215\1033519\1086833"}
