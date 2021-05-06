{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserSSOId_user where

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
testObject_UserSSOId_user_1 :: UserSSOId
testObject_UserSSOId_user_1 = UserSSOId "\b5v\GS~\SOH\r\"r" "\23029\66011\990654a\v\ACKQcq\1011460>G$\t;"
testObject_UserSSOId_user_2 :: UserSSOId
testObject_UserSSOId_user_2 = UserScimExternalId "W*\25679\USh&59\986987"
testObject_UserSSOId_user_3 :: UserSSOId
testObject_UserSSOId_user_3 = UserScimExternalId "\134622\NAK,\FS\NUL\1070823\19404\GS\1110895\EM\991607_o>r\1085263sz\1095945"
testObject_UserSSOId_user_4 :: UserSSOId
testObject_UserSSOId_user_4 = UserScimExternalId "w\188204;tG\178720-\1065656;\1057337P\1014968,\7910\SI\132337\&0\ETXpX\SIo>\n\ESC"
testObject_UserSSOId_user_5 :: UserSSOId
testObject_UserSSOId_user_5 = UserScimExternalId "\SO\1060752x\SYN\5410\1091857?\ETX;]\53197\&5Y\142247\EMO>D|\t\DC2\177050o\29922\52974W\EMw"
testObject_UserSSOId_user_6 :: UserSSOId
testObject_UserSSOId_user_6 = UserScimExternalId "!\996953\1016945\1066560TL>"
testObject_UserSSOId_user_7 :: UserSSOId
testObject_UserSSOId_user_7 = UserScimExternalId "{\1016137\"g\1093373Nm\95097\1001532M E\1062406\1049310\1108414_"
testObject_UserSSOId_user_8 :: UserSSOId
testObject_UserSSOId_user_8 = UserSSOId "l&}\31044:s\DC2ZU\SI\182558\46541" "\DC3q\1056347n"
testObject_UserSSOId_user_9 :: UserSSOId
testObject_UserSSOId_user_9 = UserSSOId "\SIr&QrEx" "\DC2\US\1000176~>\SYNa"
testObject_UserSSOId_user_10 :: UserSSOId
testObject_UserSSOId_user_10 = UserScimExternalId "O\47600`\ACKT\v\1082331\\N\174069\&3"
testObject_UserSSOId_user_11 :: UserSSOId
testObject_UserSSOId_user_11 = UserSSOId "\1064469$]\US\33483\49078" "]\SUB"
testObject_UserSSOId_user_12 :: UserSSOId
testObject_UserSSOId_user_12 = UserSSOId "\NULO\fC]fu\FSk" "2\10251\1093824\STX\CAN\SO-,\ENQI/\1036512\r\DC1@"
testObject_UserSSOId_user_13 :: UserSSOId
testObject_UserSSOId_user_13 = UserSSOId "X\11837\NUL8\1025307" "\188737\DC2\DC4"
testObject_UserSSOId_user_14 :: UserSSOId
testObject_UserSSOId_user_14 = UserSSOId "\1111269IVC.\1049825jZ\SO" "7"
testObject_UserSSOId_user_15 :: UserSSOId
testObject_UserSSOId_user_15 = UserScimExternalId "S\DLE44\ETX=\DC3\DC1Rj\131091Y"
testObject_UserSSOId_user_16 :: UserSSOId
testObject_UserSSOId_user_16 = UserSSOId "K(Myo\1104497\52921W\989363\GS!\45682~`\180988" ""
testObject_UserSSOId_user_17 :: UserSSOId
testObject_UserSSOId_user_17 = UserSSOId "8\991329\NAK:rN\1015701\SYNW" "\1014731_Yb"
testObject_UserSSOId_user_18 :: UserSSOId
testObject_UserSSOId_user_18 = UserScimExternalId "\29065\&8\53092\985519\ESCV\1083754"
testObject_UserSSOId_user_19 :: UserSSOId
testObject_UserSSOId_user_19 = UserScimExternalId "\ETBG\DC4Wl\NULA<abA\GS\96961"
testObject_UserSSOId_user_20 :: UserSSOId
testObject_UserSSOId_user_20 = UserScimExternalId "\150781\GS\GS+yM(\144952[\993496\58160W[\994929!\146980X`y\FSbxv\CAN"
