{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Offset_user where

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
testObject_Offset_user_1 :: Offset
testObject_Offset_user_1 = Offset {offsetBytes = 7}
testObject_Offset_user_2 :: Offset
testObject_Offset_user_2 = Offset {offsetBytes = 29}
testObject_Offset_user_3 :: Offset
testObject_Offset_user_3 = Offset {offsetBytes = 28}
testObject_Offset_user_4 :: Offset
testObject_Offset_user_4 = Offset {offsetBytes = 11}
testObject_Offset_user_5 :: Offset
testObject_Offset_user_5 = Offset {offsetBytes = 29}
testObject_Offset_user_6 :: Offset
testObject_Offset_user_6 = Offset {offsetBytes = 3}
testObject_Offset_user_7 :: Offset
testObject_Offset_user_7 = Offset {offsetBytes = 30}
testObject_Offset_user_8 :: Offset
testObject_Offset_user_8 = Offset {offsetBytes = 24}
testObject_Offset_user_9 :: Offset
testObject_Offset_user_9 = Offset {offsetBytes = 11}
testObject_Offset_user_10 :: Offset
testObject_Offset_user_10 = Offset {offsetBytes = 22}
testObject_Offset_user_11 :: Offset
testObject_Offset_user_11 = Offset {offsetBytes = 7}
testObject_Offset_user_12 :: Offset
testObject_Offset_user_12 = Offset {offsetBytes = 23}
testObject_Offset_user_13 :: Offset
testObject_Offset_user_13 = Offset {offsetBytes = 27}
testObject_Offset_user_14 :: Offset
testObject_Offset_user_14 = Offset {offsetBytes = 24}
testObject_Offset_user_15 :: Offset
testObject_Offset_user_15 = Offset {offsetBytes = 8}
testObject_Offset_user_16 :: Offset
testObject_Offset_user_16 = Offset {offsetBytes = 1}
testObject_Offset_user_17 :: Offset
testObject_Offset_user_17 = Offset {offsetBytes = 10}
testObject_Offset_user_18 :: Offset
testObject_Offset_user_18 = Offset {offsetBytes = 0}
testObject_Offset_user_19 :: Offset
testObject_Offset_user_19 = Offset {offsetBytes = 8}
testObject_Offset_user_20 :: Offset
testObject_Offset_user_20 = Offset {offsetBytes = 12}
