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
testObject_NewConvUnmanaged_user_1 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002")))], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "m8lrphgq77mqo9jvgxzamoklvktnwgjgtlqr5gohyax2luth9sl2qpjsqxq1__wrk0kk4jpglmysd"))})
testObject_NewConvUnmanaged_user_2 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_2 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))], newConvName = Nothing, newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 3889950180834042}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "4uwqntpbvw90"))})
testObject_NewConvUnmanaged_user_3 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_3 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001")))], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 3625494488854770}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "3u6wd263xn_1v3w520c959mh4x8xst6wpacucp0qclgkmax7l7sgq2_oqqm49wdpywe2m7md1mnbctc096ns6pmx1ri_ghlzlfam4nbj1ti9u3ueqyle6_s"))})
testObject_NewConvUnmanaged_user_4 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_4 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001")))], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "efvk6wumzk7cmlkxpfakx4q0zjk5e8teq8tm7"))})
testObject_NewConvUnmanaged_user_5 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_5 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], newConvName = Just "~", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "2xzt_ndzy1rsn_9n4c3mh012d2suxofny1at"))})
