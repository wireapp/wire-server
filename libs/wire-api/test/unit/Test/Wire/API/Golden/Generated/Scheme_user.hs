{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Scheme_user where

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
testObject_Scheme_user_1 :: CallConfig.Scheme
testObject_Scheme_user_1 = SchemeTurns
testObject_Scheme_user_2 :: CallConfig.Scheme
testObject_Scheme_user_2 = SchemeTurns
testObject_Scheme_user_3 :: CallConfig.Scheme
testObject_Scheme_user_3 = SchemeTurn
testObject_Scheme_user_4 :: CallConfig.Scheme
testObject_Scheme_user_4 = SchemeTurn
testObject_Scheme_user_5 :: CallConfig.Scheme
testObject_Scheme_user_5 = SchemeTurns
testObject_Scheme_user_6 :: CallConfig.Scheme
testObject_Scheme_user_6 = SchemeTurns
testObject_Scheme_user_7 :: CallConfig.Scheme
testObject_Scheme_user_7 = SchemeTurn
testObject_Scheme_user_8 :: CallConfig.Scheme
testObject_Scheme_user_8 = SchemeTurns
testObject_Scheme_user_9 :: CallConfig.Scheme
testObject_Scheme_user_9 = SchemeTurns
testObject_Scheme_user_10 :: CallConfig.Scheme
testObject_Scheme_user_10 = SchemeTurns
testObject_Scheme_user_11 :: CallConfig.Scheme
testObject_Scheme_user_11 = SchemeTurn
testObject_Scheme_user_12 :: CallConfig.Scheme
testObject_Scheme_user_12 = SchemeTurns
testObject_Scheme_user_13 :: CallConfig.Scheme
testObject_Scheme_user_13 = SchemeTurn
testObject_Scheme_user_14 :: CallConfig.Scheme
testObject_Scheme_user_14 = SchemeTurns
testObject_Scheme_user_15 :: CallConfig.Scheme
testObject_Scheme_user_15 = SchemeTurns
testObject_Scheme_user_16 :: CallConfig.Scheme
testObject_Scheme_user_16 = SchemeTurns
testObject_Scheme_user_17 :: CallConfig.Scheme
testObject_Scheme_user_17 = SchemeTurn
testObject_Scheme_user_18 :: CallConfig.Scheme
testObject_Scheme_user_18 = SchemeTurns
testObject_Scheme_user_19 :: CallConfig.Scheme
testObject_Scheme_user_19 = SchemeTurns
testObject_Scheme_user_20 :: CallConfig.Scheme
testObject_Scheme_user_20 = SchemeTurn
