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
testObject_NewConvManaged_user_1 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5841910665092298}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "1yqdr7d9bynn1cdwv7kacjguhb"))})
testObject_NewConvManaged_user_2 :: NewConvManaged
testObject_NewConvManaged_user_2 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "B\NUL", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3591285183237075}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "fl5wqn58spauc6vafnah8jwbq86tur0pavjieh72sx1avqbzm_fcq5h67jkm5ptewb5_euy3vb_"))})
testObject_NewConvManaged_user_3 :: NewConvManaged
testObject_NewConvManaged_user_3 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess,LinkAccess,CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 685549364195491}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "36kzj5th4fh"))})
testObject_NewConvManaged_user_4 :: NewConvManaged
testObject_NewConvManaged_user_4 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 492987567580952}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "41qkcalq05c5kud2_r6lu_e61bx3_cy9rzbkesww_9r1f_iu81n79q6n9y0xij1hdd0t1z5vh3ycrt2wzbq01x47xhy7qyo0v94mk6crbg0ys3"))})
testObject_NewConvManaged_user_5 :: NewConvManaged
testObject_NewConvManaged_user_5 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))], newConvName = Just ")I", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "zx510qzj12dtv5ivqxj6hz7ko833fv737ektkbaebnyle_70vkybw80p4g_ijwce0xz_o0erijatso_11asym3ago6d"))})
