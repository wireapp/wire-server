{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ActivationCode_user where

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
testObject_ActivationCode_user_1 :: ActivationCode
testObject_ActivationCode_user_1 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("IffMZINHYfu-MB1_TlbrvA==")))}
testObject_ActivationCode_user_2 :: ActivationCode
testObject_ActivationCode_user_2 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("8Tf5twVpcak2Y-872Y-4SKItQr_GoYIEZCvVkQ==")))}
testObject_ActivationCode_user_3 :: ActivationCode
testObject_ActivationCode_user_3 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("SEe63PSbY0kU")))}
testObject_ActivationCode_user_4 :: ActivationCode
testObject_ActivationCode_user_4 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("_N5SpxUk9lqUdkRE4t14llxz2sv_X_9LWlMK7w==")))}
testObject_ActivationCode_user_5 :: ActivationCode
testObject_ActivationCode_user_5 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("WMOOloHSAasIBLFgQe0=")))}
testObject_ActivationCode_user_6 :: ActivationCode
testObject_ActivationCode_user_6 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ADKvrkKNcouZIs8XDP8JDYM=")))}
testObject_ActivationCode_user_7 :: ActivationCode
testObject_ActivationCode_user_7 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("9SZ1GRcy94SS4RbslSZsMEMyU3eh-Q==")))}
testObject_ActivationCode_user_8 :: ActivationCode
testObject_ActivationCode_user_8 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("aquDbe5NTzl_Nd8MRW-aOF8sQ-Afblg=")))}
testObject_ActivationCode_user_9 :: ActivationCode
testObject_ActivationCode_user_9 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("BoVIFvSqntm5l5Mj05yZ6VXHMCqwNixvSQ==")))}
testObject_ActivationCode_user_10 :: ActivationCode
testObject_ActivationCode_user_10 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("vmEuzhKsCdLXxQ==")))}
testObject_ActivationCode_user_11 :: ActivationCode
testObject_ActivationCode_user_11 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("htUGqisbb710Nop4FFGLcitcwg==")))}
testObject_ActivationCode_user_12 :: ActivationCode
testObject_ActivationCode_user_12 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("JxZlNOPweWUpPyLie8JWBw==")))}
testObject_ActivationCode_user_13 :: ActivationCode
testObject_ActivationCode_user_13 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("tvJ63Rj-AuRNYUhNJNByj9KdndvqfAbZdGoMNA==")))}
testObject_ActivationCode_user_14 :: ActivationCode
testObject_ActivationCode_user_14 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("pXDJZth3rWLccsyj0w==")))}
testObject_ActivationCode_user_15 :: ActivationCode
testObject_ActivationCode_user_15 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("3SQv5NeKSjMgukH45MAd6sk7t-u8ZxWHviKmdQ==")))}
testObject_ActivationCode_user_16 :: ActivationCode
testObject_ActivationCode_user_16 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("B4FllgX-rSuuM91wNHGJJC-k3HaXznYKo68fFg==")))}
testObject_ActivationCode_user_17 :: ActivationCode
testObject_ActivationCode_user_17 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("dD_sCVSDhllvIQWZ")))}
testObject_ActivationCode_user_18 :: ActivationCode
testObject_ActivationCode_user_18 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("f8irP-G0x19i_Q5hCA==")))}
testObject_ActivationCode_user_19 :: ActivationCode
testObject_ActivationCode_user_19 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ag==")))}
testObject_ActivationCode_user_20 :: ActivationCode
testObject_ActivationCode_user_20 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("LgAhl3KXG4ZwKHpC2HzN3A==")))}
