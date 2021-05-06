{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PendingLoginCode_user where

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
testObject_PendingLoginCode_user_1 :: PendingLoginCode
testObject_PendingLoginCode_user_1 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "GEX\120457X\ENQ|"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-2.000000000000)))}
testObject_PendingLoginCode_user_2 :: PendingLoginCode
testObject_PendingLoginCode_user_2 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\b"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-4.000000000000)))}
testObject_PendingLoginCode_user_3 :: PendingLoginCode
testObject_PendingLoginCode_user_3 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1050342\EOTeaGh\SI3\1071080Q\1075198"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (14.000000000000)))}
testObject_PendingLoginCode_user_4 :: PendingLoginCode
testObject_PendingLoginCode_user_4 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\v/G\137839\GSW\169973\1100390\1102498N|\174053\1075065"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (10.000000000000)))}
testObject_PendingLoginCode_user_5 :: PendingLoginCode
testObject_PendingLoginCode_user_5 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "_=\151358\FS)hG$"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-15.000000000000)))}
testObject_PendingLoginCode_user_6 :: PendingLoginCode
testObject_PendingLoginCode_user_6 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = ""}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (12.000000000000)))}
testObject_PendingLoginCode_user_7 :: PendingLoginCode
testObject_PendingLoginCode_user_7 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\187574w\fjX\ESCHY&"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (10.000000000000)))}
testObject_PendingLoginCode_user_8 :: PendingLoginCode
testObject_PendingLoginCode_user_8 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "A:\1059452\ACK\63876^.f\1055191X\38097?\SO"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-8.000000000000)))}
testObject_PendingLoginCode_user_9 :: PendingLoginCode
testObject_PendingLoginCode_user_9 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "Q;\GSm8z\DC3\GSO5\60609w\61121\SYN\""}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-15.000000000000)))}
testObject_PendingLoginCode_user_10 :: PendingLoginCode
testObject_PendingLoginCode_user_10 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1044597\r*!\16681\48069z"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (1.000000000000)))}
testObject_PendingLoginCode_user_11 :: PendingLoginCode
testObject_PendingLoginCode_user_11 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1081148\146207LL\39489-\DC39U\FS\1012578 \1014056c&"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (10.000000000000)))}
testObject_PendingLoginCode_user_12 :: PendingLoginCode
testObject_PendingLoginCode_user_12 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = ""}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (12.000000000000)))}
testObject_PendingLoginCode_user_13 :: PendingLoginCode
testObject_PendingLoginCode_user_13 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "s.\1091846"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (3.000000000000)))}
testObject_PendingLoginCode_user_14 :: PendingLoginCode
testObject_PendingLoginCode_user_14 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1111800K!\134040\24232\ESCV1\1051659"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (10.000000000000)))}
testObject_PendingLoginCode_user_15 :: PendingLoginCode
testObject_PendingLoginCode_user_15 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1050462\1005541~\1071124\1052740\53203x\DC2,\1003442\&6"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-9.000000000000)))}
testObject_PendingLoginCode_user_16 :: PendingLoginCode
testObject_PendingLoginCode_user_16 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\GS\23240\SUB\1026098[\f\1103386%*c\1076800M"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (6.000000000000)))}
testObject_PendingLoginCode_user_17 :: PendingLoginCode
testObject_PendingLoginCode_user_17 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "&="}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (9.000000000000)))}
testObject_PendingLoginCode_user_18 :: PendingLoginCode
testObject_PendingLoginCode_user_18 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\23115,G\DC1L\DC3h\1074164"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-3.000000000000)))}
testObject_PendingLoginCode_user_19 :: PendingLoginCode
testObject_PendingLoginCode_user_19 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\5139\ETB"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-1.000000000000)))}
testObject_PendingLoginCode_user_20 :: PendingLoginCode
testObject_PendingLoginCode_user_20 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "xG\DC1\1097667\DC3\189196%"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-10.000000000000)))}
