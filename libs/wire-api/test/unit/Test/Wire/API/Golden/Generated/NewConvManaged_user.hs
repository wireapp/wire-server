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
testObject_NewConvManaged_1 :: NewConvManaged
testObject_NewConvManaged_1 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "r_aw5dotudqfuzvwuyj4ealcfmfftzaux79w75dn6ufu_frwz_mj1x6lrtlc7pbc_ulg5r6ktz3h4xhzvt0c688ip047ckwej3"))})
testObject_NewConvManaged_2 :: NewConvManaged
testObject_NewConvManaged_2 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))], newConvName = Just "\\", newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 0}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "djbju7zyik8nwa"))})
testObject_NewConvManaged_3 :: NewConvManaged
testObject_NewConvManaged_3 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], newConvName = Just "\SYN\DLE\t\SI", newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "ofm4gomt52kaoxmns77l_4lo2zrme0rcmmvc5m4w827m19c_ce2w1os_zp3kmrr4lwqpt0dtmerrxfrj6sx70e4m2wevj9zg0ot"))})
testObject_NewConvManaged_4 :: NewConvManaged
testObject_NewConvManaged_4 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\170261\r", newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 0}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 4}), newConvUsersRole = (fromJust (parseRoleName "z6wgele1qe4zttez8bv1plebrs2r0t152f0idma1knai62jv_0gg1vnikb800yf90"))})
testObject_NewConvManaged_5 :: NewConvManaged
testObject_NewConvManaged_5 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "d0rft871q0_cgbnmaxrjmjrn1rhnslo8ogtv7njr7eywxuts4nxe9_"))})
