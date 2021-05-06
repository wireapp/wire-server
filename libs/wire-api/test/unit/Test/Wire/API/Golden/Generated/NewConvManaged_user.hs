{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewConvManaged_user where

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
testObject_NewConvManaged_user_1 :: NewConvManaged
testObject_NewConvManaged_user_1 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "C\1009802\17588", newConvAccess = Set.fromList [], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 1}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "a47i1h96m7i42jf7pjopi3jn1sjvernbzaf2pz_syiaoltkbhz6ikkoce2ai5p3v94nzy8hthlcio11l3uc_4j7rc_zgif844cv4zga7x5ycq5"))})
testObject_NewConvManaged_user_2 :: NewConvManaged
testObject_NewConvManaged_user_2 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")))], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "z5u4rhs636h2bxwjacj6_62aqsasx_kn2q998d8ds84h63e72pzo62bzee3jpliky32u96m74evyb927flb8q4zycjtz9e5qrfpq"))})
testObject_NewConvManaged_user_3 :: NewConvManaged
testObject_NewConvManaged_user_3 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000001")))], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess,InviteAccess,LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "ujs67psi7lnylfkgdzci3h4uz6sad674w602xxlbqj1v59d97u4l3zaagg7o5uc24thz1t51xnq23tft8jqxt1"))})
testObject_NewConvManaged_user_4 :: NewConvManaged
testObject_NewConvManaged_user_4 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\ACK65", newConvAccess = Set.fromList [], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "lx9m_hwm0ifeh5tqgoaejn6rxojic3rh9ih4bydc4bu0hfz2bn6qgtov2hbca"))})
testObject_NewConvManaged_user_5 :: NewConvManaged
testObject_NewConvManaged_user_5 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\f", newConvAccess = Set.fromList [PrivateAccess,InviteAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "hoj"))})
