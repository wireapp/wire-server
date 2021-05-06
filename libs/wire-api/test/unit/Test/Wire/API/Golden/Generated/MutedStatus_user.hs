{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.MutedStatus_user where

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
testObject_MutedStatus_user_1 :: MutedStatus
testObject_MutedStatus_user_1 = MutedStatus {fromMutedStatus = -12718}
testObject_MutedStatus_user_2 :: MutedStatus
testObject_MutedStatus_user_2 = MutedStatus {fromMutedStatus = 9551}
testObject_MutedStatus_user_3 :: MutedStatus
testObject_MutedStatus_user_3 = MutedStatus {fromMutedStatus = -30336}
testObject_MutedStatus_user_4 :: MutedStatus
testObject_MutedStatus_user_4 = MutedStatus {fromMutedStatus = -6419}
testObject_MutedStatus_user_5 :: MutedStatus
testObject_MutedStatus_user_5 = MutedStatus {fromMutedStatus = -24472}
testObject_MutedStatus_user_6 :: MutedStatus
testObject_MutedStatus_user_6 = MutedStatus {fromMutedStatus = -28195}
testObject_MutedStatus_user_7 :: MutedStatus
testObject_MutedStatus_user_7 = MutedStatus {fromMutedStatus = -2169}
testObject_MutedStatus_user_8 :: MutedStatus
testObject_MutedStatus_user_8 = MutedStatus {fromMutedStatus = -2361}
testObject_MutedStatus_user_9 :: MutedStatus
testObject_MutedStatus_user_9 = MutedStatus {fromMutedStatus = 7018}
testObject_MutedStatus_user_10 :: MutedStatus
testObject_MutedStatus_user_10 = MutedStatus {fromMutedStatus = 19797}
testObject_MutedStatus_user_11 :: MutedStatus
testObject_MutedStatus_user_11 = MutedStatus {fromMutedStatus = -2603}
testObject_MutedStatus_user_12 :: MutedStatus
testObject_MutedStatus_user_12 = MutedStatus {fromMutedStatus = 21738}
testObject_MutedStatus_user_13 :: MutedStatus
testObject_MutedStatus_user_13 = MutedStatus {fromMutedStatus = -28917}
testObject_MutedStatus_user_14 :: MutedStatus
testObject_MutedStatus_user_14 = MutedStatus {fromMutedStatus = -152}
testObject_MutedStatus_user_15 :: MutedStatus
testObject_MutedStatus_user_15 = MutedStatus {fromMutedStatus = 6773}
testObject_MutedStatus_user_16 :: MutedStatus
testObject_MutedStatus_user_16 = MutedStatus {fromMutedStatus = 5713}
testObject_MutedStatus_user_17 :: MutedStatus
testObject_MutedStatus_user_17 = MutedStatus {fromMutedStatus = 4657}
testObject_MutedStatus_user_18 :: MutedStatus
testObject_MutedStatus_user_18 = MutedStatus {fromMutedStatus = 4629}
testObject_MutedStatus_user_19 :: MutedStatus
testObject_MutedStatus_user_19 = MutedStatus {fromMutedStatus = 5404}
testObject_MutedStatus_user_20 :: MutedStatus
testObject_MutedStatus_user_20 = MutedStatus {fromMutedStatus = 2561}
