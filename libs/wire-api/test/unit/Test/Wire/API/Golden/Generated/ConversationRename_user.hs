{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationRename_user where

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
testObject_ConversationRename_user_1 :: ConversationRename
testObject_ConversationRename_user_1 = ConversationRename {cupName = "`Z"}
testObject_ConversationRename_user_2 :: ConversationRename
testObject_ConversationRename_user_2 = ConversationRename {cupName = "cV:\DC3\995330\5406\SUBU\DC40>ov\STX04$\128059\1091762\1062069v3\1041963"}
testObject_ConversationRename_user_3 :: ConversationRename
testObject_ConversationRename_user_3 = ConversationRename {cupName = "\1113932G\44138|\ETB\RSk"}
testObject_ConversationRename_user_4 :: ConversationRename
testObject_ConversationRename_user_4 = ConversationRename {cupName = "\1001729s5\SO]Y\58996\&7\1089751q\DELg99"}
testObject_ConversationRename_user_5 :: ConversationRename
testObject_ConversationRename_user_5 = ConversationRename {cupName = "u%\GSI"}
testObject_ConversationRename_user_6 :: ConversationRename
testObject_ConversationRename_user_6 = ConversationRename {cupName = "\16404O\DC3pk):"}
testObject_ConversationRename_user_7 :: ConversationRename
testObject_ConversationRename_user_7 = ConversationRename {cupName = "\177134d\1084264[@\141214\r>7R\94326\1073449\GS|\DC1"}
testObject_ConversationRename_user_8 :: ConversationRename
testObject_ConversationRename_user_8 = ConversationRename {cupName = "T\NULQ_e+\186109\1023451\f\1038133i(JVvA/IQ\ACKT}\1089490E\12581\46806\7763-"}
testObject_ConversationRename_user_9 :: ConversationRename
testObject_ConversationRename_user_9 = ConversationRename {cupName = "\tK<\40942\1047866ODb\GS\131219Q \\"}
testObject_ConversationRename_user_10 :: ConversationRename
testObject_ConversationRename_user_10 = ConversationRename {cupName = "\b=D;e:\GS\GS2N\ENQW\ESC\DC44{\1076852\DC4\40755\135994b\165512\1003698\1022566"}
testObject_ConversationRename_user_11 :: ConversationRename
testObject_ConversationRename_user_11 = ConversationRename {cupName = "Z\166043"}
testObject_ConversationRename_user_12 :: ConversationRename
testObject_ConversationRename_user_12 = ConversationRename {cupName = "\3199y"}
testObject_ConversationRename_user_13 :: ConversationRename
testObject_ConversationRename_user_13 = ConversationRename {cupName = "\1103573\1039594m\69858\US1\68765\190694\&6l0[\SUB\b\153946q\\\ESC`"}
testObject_ConversationRename_user_14 :: ConversationRename
testObject_ConversationRename_user_14 = ConversationRename {cupName = "m7~\1041399\DC1^I\995508m\DC4\RS2\ACK\NUL"}
testObject_ConversationRename_user_15 :: ConversationRename
testObject_ConversationRename_user_15 = ConversationRename {cupName = "\f\42699kH\SOHp\GS>\DLE\\\DC2^\r\1030589\r\SO\32711\\Ro-\SO"}
testObject_ConversationRename_user_16 :: ConversationRename
testObject_ConversationRename_user_16 = ConversationRename {cupName = "\1049917\n@\ETB9zV@|"}
testObject_ConversationRename_user_17 :: ConversationRename
testObject_ConversationRename_user_17 = ConversationRename {cupName = "d\DLE+}+]+\74244\f\1051195\&9<.\v\19003\161386Fn\1032729X\\M"}
testObject_ConversationRename_user_18 :: ConversationRename
testObject_ConversationRename_user_18 = ConversationRename {cupName = "\f\NUL;w\"\1022789e488G\STXm\NAK}m=v\NUL'E"}
testObject_ConversationRename_user_19 :: ConversationRename
testObject_ConversationRename_user_19 = ConversationRename {cupName = "\1089035\1031453:\1093995\DC1LXr"}
testObject_ConversationRename_user_20 :: ConversationRename
testObject_ConversationRename_user_20 = ConversationRename {cupName = "[\SI=\f\145771\1025115\&1S\NAKW\NAK1\1113098\&1"}
