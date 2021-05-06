{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LoginCodeTimeout_user where

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
testObject_LoginCodeTimeout_user_1 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_1 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (11.000000000000)))}
testObject_LoginCodeTimeout_user_2 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_2 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-18.000000000000)))}
testObject_LoginCodeTimeout_user_3 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_3 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-28.000000000000)))}
testObject_LoginCodeTimeout_user_4 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_4 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (5.000000000000)))}
testObject_LoginCodeTimeout_user_5 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_5 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-10.000000000000)))}
testObject_LoginCodeTimeout_user_6 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_6 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (27.000000000000)))}
testObject_LoginCodeTimeout_user_7 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_7 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (3.000000000000)))}
testObject_LoginCodeTimeout_user_8 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_8 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-4.000000000000)))}
testObject_LoginCodeTimeout_user_9 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_9 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (12.000000000000)))}
testObject_LoginCodeTimeout_user_10 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_10 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-7.000000000000)))}
testObject_LoginCodeTimeout_user_11 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_11 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-16.000000000000)))}
testObject_LoginCodeTimeout_user_12 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_12 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (21.000000000000)))}
testObject_LoginCodeTimeout_user_13 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_13 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (28.000000000000)))}
testObject_LoginCodeTimeout_user_14 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_14 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-7.000000000000)))}
testObject_LoginCodeTimeout_user_15 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_15 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (7.000000000000)))}
testObject_LoginCodeTimeout_user_16 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_16 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (7.000000000000)))}
testObject_LoginCodeTimeout_user_17 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_17 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-7.000000000000)))}
testObject_LoginCodeTimeout_user_18 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_18 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-15.000000000000)))}
testObject_LoginCodeTimeout_user_19 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_19 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-9.000000000000)))}
testObject_LoginCodeTimeout_user_20 :: LoginCodeTimeout
testObject_LoginCodeTimeout_user_20 = LoginCodeTimeout {fromLoginCodeTimeout = (Timeout (secondsToNominalDiffTime (-11.000000000000)))}
