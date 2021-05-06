{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConvType_user where

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
testObject_ConvType_user_1 :: ConvType
testObject_ConvType_user_1 = SelfConv
testObject_ConvType_user_2 :: ConvType
testObject_ConvType_user_2 = ConnectConv
testObject_ConvType_user_3 :: ConvType
testObject_ConvType_user_3 = One2OneConv
testObject_ConvType_user_4 :: ConvType
testObject_ConvType_user_4 = SelfConv
testObject_ConvType_user_5 :: ConvType
testObject_ConvType_user_5 = One2OneConv
testObject_ConvType_user_6 :: ConvType
testObject_ConvType_user_6 = ConnectConv
testObject_ConvType_user_7 :: ConvType
testObject_ConvType_user_7 = ConnectConv
testObject_ConvType_user_8 :: ConvType
testObject_ConvType_user_8 = One2OneConv
testObject_ConvType_user_9 :: ConvType
testObject_ConvType_user_9 = RegularConv
testObject_ConvType_user_10 :: ConvType
testObject_ConvType_user_10 = ConnectConv
testObject_ConvType_user_11 :: ConvType
testObject_ConvType_user_11 = One2OneConv
testObject_ConvType_user_12 :: ConvType
testObject_ConvType_user_12 = RegularConv
testObject_ConvType_user_13 :: ConvType
testObject_ConvType_user_13 = SelfConv
testObject_ConvType_user_14 :: ConvType
testObject_ConvType_user_14 = One2OneConv
testObject_ConvType_user_15 :: ConvType
testObject_ConvType_user_15 = RegularConv
testObject_ConvType_user_16 :: ConvType
testObject_ConvType_user_16 = SelfConv
testObject_ConvType_user_17 :: ConvType
testObject_ConvType_user_17 = One2OneConv
testObject_ConvType_user_18 :: ConvType
testObject_ConvType_user_18 = ConnectConv
testObject_ConvType_user_19 :: ConvType
testObject_ConvType_user_19 = ConnectConv
testObject_ConvType_user_20 :: ConvType
testObject_ConvType_user_20 = RegularConv
