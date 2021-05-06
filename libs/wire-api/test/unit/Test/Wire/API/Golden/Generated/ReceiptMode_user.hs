{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ReceiptMode_user where

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
testObject_ReceiptMode_user_1 :: ReceiptMode
testObject_ReceiptMode_user_1 = ReceiptMode {unReceiptMode = 27454}
testObject_ReceiptMode_user_2 :: ReceiptMode
testObject_ReceiptMode_user_2 = ReceiptMode {unReceiptMode = 31593}
testObject_ReceiptMode_user_3 :: ReceiptMode
testObject_ReceiptMode_user_3 = ReceiptMode {unReceiptMode = 30757}
testObject_ReceiptMode_user_4 :: ReceiptMode
testObject_ReceiptMode_user_4 = ReceiptMode {unReceiptMode = -26916}
testObject_ReceiptMode_user_5 :: ReceiptMode
testObject_ReceiptMode_user_5 = ReceiptMode {unReceiptMode = -14747}
testObject_ReceiptMode_user_6 :: ReceiptMode
testObject_ReceiptMode_user_6 = ReceiptMode {unReceiptMode = 26205}
testObject_ReceiptMode_user_7 :: ReceiptMode
testObject_ReceiptMode_user_7 = ReceiptMode {unReceiptMode = 6772}
testObject_ReceiptMode_user_8 :: ReceiptMode
testObject_ReceiptMode_user_8 = ReceiptMode {unReceiptMode = 1190}
testObject_ReceiptMode_user_9 :: ReceiptMode
testObject_ReceiptMode_user_9 = ReceiptMode {unReceiptMode = 17461}
testObject_ReceiptMode_user_10 :: ReceiptMode
testObject_ReceiptMode_user_10 = ReceiptMode {unReceiptMode = 30727}
testObject_ReceiptMode_user_11 :: ReceiptMode
testObject_ReceiptMode_user_11 = ReceiptMode {unReceiptMode = 15489}
testObject_ReceiptMode_user_12 :: ReceiptMode
testObject_ReceiptMode_user_12 = ReceiptMode {unReceiptMode = -540}
testObject_ReceiptMode_user_13 :: ReceiptMode
testObject_ReceiptMode_user_13 = ReceiptMode {unReceiptMode = 24165}
testObject_ReceiptMode_user_14 :: ReceiptMode
testObject_ReceiptMode_user_14 = ReceiptMode {unReceiptMode = -22517}
testObject_ReceiptMode_user_15 :: ReceiptMode
testObject_ReceiptMode_user_15 = ReceiptMode {unReceiptMode = -19708}
testObject_ReceiptMode_user_16 :: ReceiptMode
testObject_ReceiptMode_user_16 = ReceiptMode {unReceiptMode = 262}
testObject_ReceiptMode_user_17 :: ReceiptMode
testObject_ReceiptMode_user_17 = ReceiptMode {unReceiptMode = 2341}
testObject_ReceiptMode_user_18 :: ReceiptMode
testObject_ReceiptMode_user_18 = ReceiptMode {unReceiptMode = 1138}
testObject_ReceiptMode_user_19 :: ReceiptMode
testObject_ReceiptMode_user_19 = ReceiptMode {unReceiptMode = 16685}
testObject_ReceiptMode_user_20 :: ReceiptMode
testObject_ReceiptMode_user_20 = ReceiptMode {unReceiptMode = 826}
