{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AssetSettings_user where

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
testObject_AssetSettings_user_1 :: AssetSettings
testObject_AssetSettings_user_1 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetPersistent))
testObject_AssetSettings_user_2 :: AssetSettings
testObject_AssetSettings_user_2 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetVolatile))
testObject_AssetSettings_user_3 :: AssetSettings
testObject_AssetSettings_user_3 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetExpiring))
testObject_AssetSettings_user_4 :: AssetSettings
testObject_AssetSettings_user_4 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetExpiring))
testObject_AssetSettings_user_5 :: AssetSettings
testObject_AssetSettings_user_5 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Nothing))
testObject_AssetSettings_user_6 :: AssetSettings
testObject_AssetSettings_user_6 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Nothing))
testObject_AssetSettings_user_7 :: AssetSettings
testObject_AssetSettings_user_7 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetEternal))
testObject_AssetSettings_user_8 :: AssetSettings
testObject_AssetSettings_user_8 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetEternal))
testObject_AssetSettings_user_9 :: AssetSettings
testObject_AssetSettings_user_9 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetExpiring))
testObject_AssetSettings_user_10 :: AssetSettings
testObject_AssetSettings_user_10 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetPersistent))
testObject_AssetSettings_user_11 :: AssetSettings
testObject_AssetSettings_user_11 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetExpiring))
testObject_AssetSettings_user_12 :: AssetSettings
testObject_AssetSettings_user_12 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Nothing))
testObject_AssetSettings_user_13 :: AssetSettings
testObject_AssetSettings_user_13 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetVolatile))
testObject_AssetSettings_user_14 :: AssetSettings
testObject_AssetSettings_user_14 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Nothing))
testObject_AssetSettings_user_15 :: AssetSettings
testObject_AssetSettings_user_15 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetExpiring))
testObject_AssetSettings_user_16 :: AssetSettings
testObject_AssetSettings_user_16 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Nothing))
testObject_AssetSettings_user_17 :: AssetSettings
testObject_AssetSettings_user_17 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetEternal))
testObject_AssetSettings_user_18 :: AssetSettings
testObject_AssetSettings_user_18 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetVolatile))
testObject_AssetSettings_user_19 :: AssetSettings
testObject_AssetSettings_user_19 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetExpiring))
testObject_AssetSettings_user_20 :: AssetSettings
testObject_AssetSettings_user_20 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetEternalInfrequentAccess))
