{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PrekeyId_user where

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
testObject_PrekeyId_user_1 :: PrekeyId
testObject_PrekeyId_user_1 = PrekeyId {keyId = 29447}
testObject_PrekeyId_user_2 :: PrekeyId
testObject_PrekeyId_user_2 = PrekeyId {keyId = 16179}
testObject_PrekeyId_user_3 :: PrekeyId
testObject_PrekeyId_user_3 = PrekeyId {keyId = 5619}
testObject_PrekeyId_user_4 :: PrekeyId
testObject_PrekeyId_user_4 = PrekeyId {keyId = 6384}
testObject_PrekeyId_user_5 :: PrekeyId
testObject_PrekeyId_user_5 = PrekeyId {keyId = 3051}
testObject_PrekeyId_user_6 :: PrekeyId
testObject_PrekeyId_user_6 = PrekeyId {keyId = 3951}
testObject_PrekeyId_user_7 :: PrekeyId
testObject_PrekeyId_user_7 = PrekeyId {keyId = 19670}
testObject_PrekeyId_user_8 :: PrekeyId
testObject_PrekeyId_user_8 = PrekeyId {keyId = 8826}
testObject_PrekeyId_user_9 :: PrekeyId
testObject_PrekeyId_user_9 = PrekeyId {keyId = 4787}
testObject_PrekeyId_user_10 :: PrekeyId
testObject_PrekeyId_user_10 = PrekeyId {keyId = 8734}
testObject_PrekeyId_user_11 :: PrekeyId
testObject_PrekeyId_user_11 = PrekeyId {keyId = 27616}
testObject_PrekeyId_user_12 :: PrekeyId
testObject_PrekeyId_user_12 = PrekeyId {keyId = 13751}
testObject_PrekeyId_user_13 :: PrekeyId
testObject_PrekeyId_user_13 = PrekeyId {keyId = 8276}
testObject_PrekeyId_user_14 :: PrekeyId
testObject_PrekeyId_user_14 = PrekeyId {keyId = 19359}
testObject_PrekeyId_user_15 :: PrekeyId
testObject_PrekeyId_user_15 = PrekeyId {keyId = 12106}
testObject_PrekeyId_user_16 :: PrekeyId
testObject_PrekeyId_user_16 = PrekeyId {keyId = 31835}
testObject_PrekeyId_user_17 :: PrekeyId
testObject_PrekeyId_user_17 = PrekeyId {keyId = 26569}
testObject_PrekeyId_user_18 :: PrekeyId
testObject_PrekeyId_user_18 = PrekeyId {keyId = 23930}
testObject_PrekeyId_user_19 :: PrekeyId
testObject_PrekeyId_user_19 = PrekeyId {keyId = 32162}
testObject_PrekeyId_user_20 :: PrekeyId
testObject_PrekeyId_user_20 = PrekeyId {keyId = 18826}
