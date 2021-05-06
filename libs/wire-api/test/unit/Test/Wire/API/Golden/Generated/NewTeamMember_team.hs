{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewTeamMember_team where

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
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_NewTeamMember_team_1 :: NewTeamMember
testObject_NewTeamMember_team_1 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000006"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetTeamData,SetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000800000006"))),(fromJust (readUTCTimeMillis "1864-05-08T00:39:11.966Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_2 :: NewTeamMember
testObject_NewTeamMember_team_2 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000500000003"))), _permissions = Permissions {_self = fromList [RemoveTeamMember], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_3 :: NewTeamMember
testObject_NewTeamMember_team_3 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000400000003"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,SetMemberPermissions], _copy = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedModifyConvName,SetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T11:09:14.133Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_4 :: NewTeamMember
testObject_NewTeamMember_team_4 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000100000002"))), _permissions = Permissions {_self = fromList [SetBilling,SetTeamData,SetMemberPermissions], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_5 :: NewTeamMember
testObject_NewTeamMember_team_5 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000600000007"))), _permissions = Permissions {_self = fromList [AddTeamMember,SetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations], _copy = fromList [AddTeamMember,GetMemberPermissions,GetTeamConversations]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000500000002"))),(fromJust (readUTCTimeMillis "1864-05-12T12:18:55.919Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_6 :: NewTeamMember
testObject_NewTeamMember_team_6 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000500000007"))), _permissions = Permissions {_self = fromList [DeleteTeam], _copy = fromList [DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000700000002"))),(fromJust (readUTCTimeMillis "1864-05-10T06:30:01.034Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_7 :: NewTeamMember
testObject_NewTeamMember_team_7 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000500000007"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedModifyConvName,GetMemberPermissions], _copy = fromList [CreateConversation]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000003"))),(fromJust (readUTCTimeMillis "1864-05-04T04:34:24.137Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_8 :: NewTeamMember
testObject_NewTeamMember_team_8 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000600000003"))), _permissions = Permissions {_self = fromList [GetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-07T08:59:26.794Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_9 :: NewTeamMember
testObject_NewTeamMember_team_9 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000400000003"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,GetBilling,SetMemberPermissions,DeleteTeam], _copy = fromList [RemoveTeamMember,SetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-07T05:28:09.745Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_10 :: NewTeamMember
testObject_NewTeamMember_team_10 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000300000001"))), _permissions = Permissions {_self = fromList [GetBilling], _copy = fromList [GetBilling]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000004"))),(fromJust (readUTCTimeMillis "1864-05-16T04:45:37.924Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_11 :: NewTeamMember
testObject_NewTeamMember_team_11 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000200000007"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,SetBilling,SetTeamData,GetTeamConversations,DeleteTeam], _copy = fromList [SetTeamData,GetTeamConversations]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000800000008"))),(fromJust (readUTCTimeMillis "1864-05-10T02:45:48.563Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_12 :: NewTeamMember
testObject_NewTeamMember_team_12 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000700000004"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedModifyConvName], _copy = fromList [DoNotUseDeprecatedModifyConvName]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000500000000"))),(fromJust (readUTCTimeMillis "1864-05-13T16:04:34.508Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_13 :: NewTeamMember
testObject_NewTeamMember_team_13 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000300000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,SetMemberPermissions,GetTeamConversations], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,SetBilling]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_14 :: NewTeamMember
testObject_NewTeamMember_team_14 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000200000004"))), _permissions = Permissions {_self = fromList [AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetMemberPermissions,DeleteTeam], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000400000008"))),(fromJust (readUTCTimeMillis "1864-05-15T07:31:15.510Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_15 :: NewTeamMember
testObject_NewTeamMember_team_15 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000300000002"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000500000008"))),(fromJust (readUTCTimeMillis "1864-05-12T20:43:03.606Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_16 :: NewTeamMember
testObject_NewTeamMember_team_16 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000006"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000100000007"))),(fromJust (readUTCTimeMillis "1864-05-13T20:41:20.478Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_17 :: NewTeamMember
testObject_NewTeamMember_team_17 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000100000007"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,SetTeamData,SetMemberPermissions], _copy = fromList [SetTeamData,SetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000000000003"))),(fromJust (readUTCTimeMillis "1864-05-15T07:41:15.923Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_18 :: NewTeamMember
testObject_NewTeamMember_team_18 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000700000005"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedModifyConvName,SetBilling,SetMemberPermissions,GetTeamConversations], _copy = fromList [GetTeamConversations]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000000000004"))),(fromJust (readUTCTimeMillis "1864-05-07T18:38:56.062Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_19 :: NewTeamMember
testObject_NewTeamMember_team_19 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000700000007"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,GetMemberPermissions,GetTeamConversations], _copy = fromList [GetTeamConversations]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000700000003"))),(fromJust (readUTCTimeMillis "1864-05-12T07:30:54.358Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_20 :: NewTeamMember
testObject_NewTeamMember_team_20 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000800000007"))), _permissions = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,GetMemberPermissions,SetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000300000002"))),(fromJust (readUTCTimeMillis "1864-05-07T11:39:45.962Z"))), _legalHoldStatus = UserLegalHoldPending}))
