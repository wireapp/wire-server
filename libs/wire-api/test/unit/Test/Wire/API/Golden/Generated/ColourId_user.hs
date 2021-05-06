{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ColourId_user where

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
testObject_ColourId_user_1 :: ColourId
testObject_ColourId_user_1 = ColourId {fromColourId = 7317}
testObject_ColourId_user_2 :: ColourId
testObject_ColourId_user_2 = ColourId {fromColourId = 13200}
testObject_ColourId_user_3 :: ColourId
testObject_ColourId_user_3 = ColourId {fromColourId = 29286}
testObject_ColourId_user_4 :: ColourId
testObject_ColourId_user_4 = ColourId {fromColourId = -31470}
testObject_ColourId_user_5 :: ColourId
testObject_ColourId_user_5 = ColourId {fromColourId = -5841}
testObject_ColourId_user_6 :: ColourId
testObject_ColourId_user_6 = ColourId {fromColourId = 25375}
testObject_ColourId_user_7 :: ColourId
testObject_ColourId_user_7 = ColourId {fromColourId = -237}
testObject_ColourId_user_8 :: ColourId
testObject_ColourId_user_8 = ColourId {fromColourId = 6722}
testObject_ColourId_user_9 :: ColourId
testObject_ColourId_user_9 = ColourId {fromColourId = 17807}
testObject_ColourId_user_10 :: ColourId
testObject_ColourId_user_10 = ColourId {fromColourId = -16620}
testObject_ColourId_user_11 :: ColourId
testObject_ColourId_user_11 = ColourId {fromColourId = 16697}
testObject_ColourId_user_12 :: ColourId
testObject_ColourId_user_12 = ColourId {fromColourId = 19909}
testObject_ColourId_user_13 :: ColourId
testObject_ColourId_user_13 = ColourId {fromColourId = 26339}
testObject_ColourId_user_14 :: ColourId
testObject_ColourId_user_14 = ColourId {fromColourId = -2897}
testObject_ColourId_user_15 :: ColourId
testObject_ColourId_user_15 = ColourId {fromColourId = 10680}
testObject_ColourId_user_16 :: ColourId
testObject_ColourId_user_16 = ColourId {fromColourId = 15027}
testObject_ColourId_user_17 :: ColourId
testObject_ColourId_user_17 = ColourId {fromColourId = 3610}
testObject_ColourId_user_18 :: ColourId
testObject_ColourId_user_18 = ColourId {fromColourId = -23760}
testObject_ColourId_user_19 :: ColourId
testObject_ColourId_user_19 = ColourId {fromColourId = 6181}
testObject_ColourId_user_20 :: ColourId
testObject_ColourId_user_20 = ColourId {fromColourId = 7656}
