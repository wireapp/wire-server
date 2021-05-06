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
testObject_NewConvUnmanaged_user_1 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_1 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess,CodeAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 7953944010411668}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "i10q2554p7q"))})
testObject_NewConvUnmanaged_user_2 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_2 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002")))], newConvName = Just "J\DELl", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 7957533694898449}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "dawh5g38y1sqzdt8jvpz0gdhbn005marqq3nk73s5ioqwh0ongauxiiag7w62t"))})
testObject_NewConvUnmanaged_user_3 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_3 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Just TeamAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 621270871718809}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "d3cd"))})
testObject_NewConvUnmanaged_user_4 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_4 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess,LinkAccess,CodeAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 5768560294031709}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -3}), newConvUsersRole = (fromJust (parseRoleName "cv477q40i7xeqt772wf3684aaocexj5h4gu4jses30434bicjj2n"))})
testObject_NewConvUnmanaged_user_5 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_5 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess,CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 1922747955325638}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "e1baigr0c"))})
testObject_NewConvUnmanaged_user_6 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_6 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))], newConvName = Just "[", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Nothing, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 7375397758504062}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "9sw_"))})
testObject_NewConvUnmanaged_user_7 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_7 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000")))], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "hdvkprdw0ua1bmsee_ytgo9q8gegu9qf2fv1egsj1p2gtb9v"))})
testObject_NewConvUnmanaged_user_8 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_8 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "e>", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "wjl07z9r108kll3o7p20nqdr_flte0u6noamc8zgkw8phs1qts3o5aomur2a_0xx2y047vunadm2wo52paegk"))})
testObject_NewConvUnmanaged_user_9 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_9 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess,CodeAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 5917647438329827}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "y0t840f24y32p2"))})
testObject_NewConvUnmanaged_user_10 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_10 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002")))], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "r1svbz64zvwr7pkcpxt"))})
testObject_NewConvUnmanaged_user_11 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_11 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\n", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Nothing, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 3613266228365499}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "7tjmachiku8sbh1uh46iijzpb5y03zsj3bxw0mon9ju6kdai2poc7gkfwk_unmzcartv6w_4aj0u1ehylv86ayekh0yi6p8t5wq0bvj7wlackif"))})
testObject_NewConvUnmanaged_user_12 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_12 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "^\f\f", newConvAccess = Set.fromList [], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "2q06_jcimuaavl7x9jdftir"))})
testObject_NewConvUnmanaged_user_13 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_13 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just ";%", newConvAccess = Set.fromList [InviteAccess,CodeAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 3166164442907726}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "5xu2f3jz7sqxcupjfj9wljdcwed6"))})
testObject_NewConvUnmanaged_user_14 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_14 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "Pkb", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "gtwr1hq4x86pveemz3qtp21s0d5umwoyb9ohd0iqnq_4vr6z1f9mr68jsek8ek2nc22kqxkcrdbl"))})
testObject_NewConvUnmanaged_user_15 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_15 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just ">X`", newConvAccess = Set.fromList [PrivateAccess,LinkAccess,CodeAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 5348906628147511}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "edel9ngchzzmamf2r2d9g74gxhbwm"))})
testObject_NewConvUnmanaged_user_16 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_16 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 6324436851693257}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "2ynexgcysyyc2pxhd97y7pbfq3gw471uhpfbenssvlmf6hacl5iu_"))})
testObject_NewConvUnmanaged_user_17 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_17 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "N", newConvAccess = Set.fromList [InviteAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4466123476816096}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "b1ceez8snz9j1wrwdg1j3g38s9_hwz738nrfzuykp6tt_pytcunrx2je6gavrjf2nt39"))})
testObject_NewConvUnmanaged_user_18 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_18 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000")))], newConvName = Just "U", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "rwv7cx4u1_o9pgq48xw1jyejfao96ymsp_9myit5xx1g77kgxqg5ifhn5j0zhh8z359dx0yzotf2xtakg0r_egr75lu8ugcn8c5gecet573dqta_irn85"))})
testObject_NewConvUnmanaged_user_19 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_19 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "pe\n", newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 171583163875814}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "yeeid80oz4b9atqrv1_d925o6opw_9i92p34u8i6mghsdg1cx40e611mz0cljch2f0zyj73unvdkckz3q6n_pzym7nwxfv_1y0d44p0"))})
testObject_NewConvUnmanaged_user_20 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_20 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))], newConvName = Just "6}", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 1155173147868156}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "zklinnuv306yxg0zhlkx1gq3s543d36ts2szay7rldcdy_2w_a6il1i08f9elz"))})
