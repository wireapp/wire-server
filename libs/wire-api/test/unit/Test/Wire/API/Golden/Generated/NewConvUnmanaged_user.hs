{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewConvUnmanaged_user where

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
testObject_NewConvUnmanaged_1 :: NewConvUnmanaged
testObject_NewConvUnmanaged_1 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 3}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "gbmue0fqp786hyr8l9q2gklj4pqn3iayn7pe384hh4cv9ljxzcs6sdooxmnma1770ifhwze4ppr1sagxcsmajgq301wvri5xv56qn7shqyn0_4qyts"))})
testObject_NewConvUnmanaged_2 :: NewConvUnmanaged
testObject_NewConvUnmanaged_2 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001")))], newConvName = Just "G\38915d", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 0}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "pngj2s_q9xa4p342vdn9jzw0p756no0lk4dal91j5h7otzshfmevbrto8b5y"))})
testObject_NewConvUnmanaged_3 :: NewConvUnmanaged
testObject_NewConvUnmanaged_3 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))], newConvName = Nothing, newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "sggx7nmc92dje8ytuf_lw3bl4ee1bpn2_rvtubeqglfdppzdahxnwb0lkqq3_f0f7acycc5v5rr577uedcbx0zi7t1asxxe2bc49vlo7sp6"))})
testObject_NewConvUnmanaged_4 :: NewConvUnmanaged
testObject_NewConvUnmanaged_4 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess,CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 3}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "9_6j178xauuolcvmccui3adzc2qpmn_uozlya9s73y71v0l4kdd2yflywwx1tzkranjakc80ten7inr8s_"))})
testObject_NewConvUnmanaged_5 :: NewConvUnmanaged
testObject_NewConvUnmanaged_5 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000003")))], newConvName = Just "\1095267\144709W", newConvAccess = Set.fromList [PrivateAccess,InviteAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 8}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "p5k_30bp9"))})
