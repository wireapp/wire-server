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
testObject_UserSSOId_user_1 = UserScimExternalId "ad\SOH}oV\EM\62053\182998\1054345[B%~b\DELvY?\SO\USm8"
testObject_UserSSOId_user_2 :: UserSSOId
testObject_UserSSOId_user_2 = UserSSOId "\"\1028863\v;\1014819}\1080717`e,+9\SIS" "\USy|"
testObject_UserSSOId_user_3 :: UserSSOId
testObject_UserSSOId_user_3 = UserSSOId "?ca}(\1026533\159968\&6\1001893\993707'*" "\45863p,\164171vH \991030\1106137R"
testObject_UserSSOId_user_4 :: UserSSOId
testObject_UserSSOId_user_4 = UserScimExternalId "D\145325Gy\a\19381\29490n7\1104314\&0\1005159+\DLE\n^!"
testObject_UserSSOId_user_5 :: UserSSOId
testObject_UserSSOId_user_5 = UserSSOId "7b\\" "\STX\1047251"
testObject_UserSSOId_user_6 :: UserSSOId
testObject_UserSSOId_user_6 = UserScimExternalId "\20676g?8\bV\DEL\1060642\139882g\1037321yd)]\161768\183679O\1016086Ma\NAK\1074082,\DLEc\37492\ACK\134229"
testObject_UserSSOId_user_7 :: UserSSOId
testObject_UserSSOId_user_7 = UserSSOId "\SYN\1102313" "W\US\SOH\SYN0\DC2\142434^\\W\99308"
testObject_UserSSOId_user_8 :: UserSSOId
testObject_UserSSOId_user_8 = UserScimExternalId "\1005968\65242\SYNu(8m-"
testObject_UserSSOId_user_9 :: UserSSOId
testObject_UserSSOId_user_9 = UserScimExternalId "\35352\1092635+\DC2t\11070bw$OVF"
testObject_UserSSOId_user_10 :: UserSSOId
testObject_UserSSOId_user_10 = UserScimExternalId "\182108\FS\DLEgvB\STX\1105866u\DELX\14699V"
testObject_UserSSOId_user_11 :: UserSSOId
testObject_UserSSOId_user_11 = UserSSOId "\1103366c\176835" "\47983|"
testObject_UserSSOId_user_12 :: UserSSOId
testObject_UserSSOId_user_12 = UserScimExternalId "H\ENQu\t\64448\RSln\1052034p-I7(\135134d\GS\t;"
testObject_UserSSOId_user_13 :: UserSSOId
testObject_UserSSOId_user_13 = UserSSOId "a;\140087N\a" " "
testObject_UserSSOId_user_14 :: UserSSOId
testObject_UserSSOId_user_14 = UserSSOId "G" "\1029579\147560\&7Pc\163331\nyP\174648\16255\190692%\133947#"
testObject_UserSSOId_user_15 :: UserSSOId
testObject_UserSSOId_user_15 = UserScimExternalId "\60171W\1002278!\1035942W\1032754I\SUBn:UG"
testObject_UserSSOId_user_16 :: UserSSOId
testObject_UserSSOId_user_16 = UserScimExternalId "\1102587%\STX$\CAN\142403\1037601V\DC2h\"\1061597$\46660?\1012386\1050993J\1083900@J\169783\&2\ACK6"
testObject_UserSSOId_user_17 :: UserSSOId
testObject_UserSSOId_user_17 = UserScimExternalId "\f$p4\FS:\180007Hur?\1074571e\SOHv<\118936L\20039s\190922"
testObject_UserSSOId_user_18 :: UserSSOId
testObject_UserSSOId_user_18 = UserSSOId ".p-\37933\1003151\NAK,\984115\t" "\b\r5\8949"
testObject_UserSSOId_user_19 :: UserSSOId
testObject_UserSSOId_user_19 = UserScimExternalId "R\SI"
testObject_UserSSOId_user_20 :: UserSSOId
testObject_UserSSOId_user_20 = UserScimExternalId "yK(ZFY\n\DC4>\DC1\3431rX\15830Y\147914\987534\FS\33073"
