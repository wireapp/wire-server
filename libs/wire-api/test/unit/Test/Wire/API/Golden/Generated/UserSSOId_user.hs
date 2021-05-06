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
testObject_UserSSOId_user_1 = UserScimExternalId "\163124\SUBR\STX\rq'\45237\186377OoL\STX\DLE{"
testObject_UserSSOId_user_2 :: UserSSOId
testObject_UserSSOId_user_2 = UserScimExternalId "9\1049213\&4\NAKQ\1078369UW(\167077\1056832\991361W\1020242\1081727\NAK]z\20691\v*f)\1017557-XP\1059721"
testObject_UserSSOId_user_3 :: UserSSOId
testObject_UserSSOId_user_3 = UserScimExternalId "\1096080v\ETB+\988045VR!C\1022234\1061430n\SO\1042225s'bl"
testObject_UserSSOId_user_4 :: UserSSOId
testObject_UserSSOId_user_4 = UserSSOId ",i27R\f\FSWx'\189043+t\155698<" "J8J\13737\GS`_\1041058\1024176~f\184579"
testObject_UserSSOId_user_5 :: UserSSOId
testObject_UserSSOId_user_5 = UserScimExternalId "lZ\DC2"
testObject_UserSSOId_user_6 :: UserSSOId
testObject_UserSSOId_user_6 = UserScimExternalId "S\156199\ESC\53535<\1020991+l\988099\DEL"
testObject_UserSSOId_user_7 :: UserSSOId
testObject_UserSSOId_user_7 = UserScimExternalId "s\GSGq1\152158\58201gm\a"
testObject_UserSSOId_user_8 :: UserSSOId
testObject_UserSSOId_user_8 = UserScimExternalId "3 z\DC1y\987542\STX\SYN\1012423\1076248;\1048931M\b\GSx\"_\78866\EOT\US[s\996842\ESCo\1041668TI"
testObject_UserSSOId_user_9 :: UserSSOId
testObject_UserSSOId_user_9 = UserSSOId "" "}PKE"
testObject_UserSSOId_user_10 :: UserSSOId
testObject_UserSSOId_user_10 = UserScimExternalId "\1049245\a6\STX4bB\983568\EM\149198\987577k\34994\ESCP\v\1039724h'"
testObject_UserSSOId_user_11 :: UserSSOId
testObject_UserSSOId_user_11 = UserScimExternalId ""
testObject_UserSSOId_user_12 :: UserSSOId
testObject_UserSSOId_user_12 = UserSSOId "RPc\185141T+\DC1" "\997725"
testObject_UserSSOId_user_13 :: UserSSOId
testObject_UserSSOId_user_13 = UserScimExternalId "\1047617\&2\144428\144773`\1102726m0\2221p+\96758\1028054@\1014060\\F\CAN\1059051S"
testObject_UserSSOId_user_14 :: UserSSOId
testObject_UserSSOId_user_14 = UserSSOId "\NUL\f\US\DC1" "\1061565K\1056438Sx\12863\176990\&1O\r\992841X\95327"
testObject_UserSSOId_user_15 :: UserSSOId
testObject_UserSSOId_user_15 = UserSSOId "Z?\DC46\1047792{i\vL(s2" "WyzR\ESC\1112881\1076820\t2;\20835jKB\983924"
testObject_UserSSOId_user_16 :: UserSSOId
testObject_UserSSOId_user_16 = UserScimExternalId "\8580BKk\NUL7V \DC4,-\NAK#\1110145lE\vie\SUB\1089721Q\186904F"
testObject_UserSSOId_user_17 :: UserSSOId
testObject_UserSSOId_user_17 = UserScimExternalId "N\ETX\CAN=p\1028048\v\61835|\NUL\145980"
testObject_UserSSOId_user_18 :: UserSSOId
testObject_UserSSOId_user_18 = UserScimExternalId "*\158293\DC10Jl\1098615\&1"
testObject_UserSSOId_user_19 :: UserSSOId
testObject_UserSSOId_user_19 = UserSSOId "BE" "\t[\1098710]?O\993296\189483\SOH#\SOH\ETB/\DLEi"
testObject_UserSSOId_user_20 :: UserSSOId
testObject_UserSSOId_user_20 = UserScimExternalId "Da5A\1026791\NUL\SYN.\48652E\EOTI'l\49012Y\993136*|\"\b"
