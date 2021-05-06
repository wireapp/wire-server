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
testObject_PendingLoginCode_user_1 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "-,noX\vdua"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (9.000000000000)))}
testObject_PendingLoginCode_user_2 :: PendingLoginCode
testObject_PendingLoginCode_user_2 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\SOH\1087192\31115"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (0.000000000000)))}
testObject_PendingLoginCode_user_3 :: PendingLoginCode
testObject_PendingLoginCode_user_3 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\n5\145090\NAK_W8R\EM"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (6.000000000000)))}
testObject_PendingLoginCode_user_4 :: PendingLoginCode
testObject_PendingLoginCode_user_4 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1088172("}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (0.000000000000)))}
testObject_PendingLoginCode_user_5 :: PendingLoginCode
testObject_PendingLoginCode_user_5 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "l\\\1009385\EMC3dR?"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (9.000000000000)))}
testObject_PendingLoginCode_user_6 :: PendingLoginCode
testObject_PendingLoginCode_user_6 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\39770\172128\1087702\61967]N\166231\"+c5v\987331Y"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-6.000000000000)))}
testObject_PendingLoginCode_user_7 :: PendingLoginCode
testObject_PendingLoginCode_user_7 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "7\1108562\38027i\60180[h"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (9.000000000000)))}
testObject_PendingLoginCode_user_8 :: PendingLoginCode
testObject_PendingLoginCode_user_8 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\149018\v"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-7.000000000000)))}
testObject_PendingLoginCode_user_9 :: PendingLoginCode
testObject_PendingLoginCode_user_9 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "*M\NULjIY\ETX"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (15.000000000000)))}
testObject_PendingLoginCode_user_10 :: PendingLoginCode
testObject_PendingLoginCode_user_10 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\t/\158503\ACKDHU"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-3.000000000000)))}
testObject_PendingLoginCode_user_11 :: PendingLoginCode
testObject_PendingLoginCode_user_11 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "JtQ\165725cx"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (5.000000000000)))}
testObject_PendingLoginCode_user_12 :: PendingLoginCode
testObject_PendingLoginCode_user_12 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\SUB2\29078T\1093525\17705\1042419\1044684\ETX^\176050\1016120\1048055"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-3.000000000000)))}
testObject_PendingLoginCode_user_13 :: PendingLoginCode
testObject_PendingLoginCode_user_13 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\98894\1073453\&8\b\148192\150086%9\EOTP"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-14.000000000000)))}
testObject_PendingLoginCode_user_14 :: PendingLoginCode
testObject_PendingLoginCode_user_14 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "x\ESCQ\20450Cc\1001164\&1}"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (9.000000000000)))}
testObject_PendingLoginCode_user_15 :: PendingLoginCode
testObject_PendingLoginCode_user_15 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1072691KQz\187787d\68819h\42083\1001745"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-11.000000000000)))}
testObject_PendingLoginCode_user_16 :: PendingLoginCode
testObject_PendingLoginCode_user_16 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\986660\45310\1039365Rl\162430\DC1-\n\1094891\ETBc"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (1.000000000000)))}
testObject_PendingLoginCode_user_17 :: PendingLoginCode
testObject_PendingLoginCode_user_17 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "S\1112667C6t@z\EOTq\DC3\31159\EOT"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (14.000000000000)))}
testObject_PendingLoginCode_user_18 :: PendingLoginCode
testObject_PendingLoginCode_user_18 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\50562%*`\1104520\"#"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-13.000000000000)))}
testObject_PendingLoginCode_user_19 :: PendingLoginCode
testObject_PendingLoginCode_user_19 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\139703\1040928\21186\1094138\191391h\""}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-10.000000000000)))}
testObject_PendingLoginCode_user_20 :: PendingLoginCode
testObject_PendingLoginCode_user_20 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\tM\133109\1400\1068186T"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-10.000000000000)))}
