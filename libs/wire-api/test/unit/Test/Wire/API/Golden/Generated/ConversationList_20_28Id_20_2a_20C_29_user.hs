{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationList_20_28Id_20_2a_20C_29_user where

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
testObject_ConversationList_20_28Id_20_2a_20C_29_user_1 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_1 = ConversationList {convList = [], convHasMore = True}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_2 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_2 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000000"))),(Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000000")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_3 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_3 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000500000003"))),(Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000700000006")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_4 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_4 = ConversationList {convList = [(Id (fromJust (UUID.fromString "0000004c-0000-0007-0000-005900000002")))], convHasMore = True}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_5 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_5 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000047-0000-0079-0000-003800000063")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_6 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_6 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_7 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_7 = ConversationList {convList = [], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_8 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_8 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_9 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_9 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002")))], convHasMore = True}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_10 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_10 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))], convHasMore = True}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_11 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_11 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000003"))),(Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000002")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_12 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_12 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000700000003"))),(Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000000000006")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_13 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_13 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))], convHasMore = True}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_14 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_14 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000000000002")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_15 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_15 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_16 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_16 = ConversationList {convList = [(Id (fromJust (UUID.fromString "0000001b-0000-0033-0000-004f00000053")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_17 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_17 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000026-0000-001b-0000-001c00000001")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_18 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_18 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000500000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000200000001")))], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_19 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_19 = ConversationList {convList = [], convHasMore = False}
testObject_ConversationList_20_28Id_20_2a_20C_29_user_20 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_20 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000000"))),(Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000002")))], convHasMore = True}
