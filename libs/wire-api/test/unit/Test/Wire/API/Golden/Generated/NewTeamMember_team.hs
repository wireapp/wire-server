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
testObject_NewTeamMember_team_1 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000100000008"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_2 :: NewTeamMember
testObject_NewTeamMember_team_2 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000300000006"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000700000008"))),(fromJust (readUTCTimeMillis "1864-05-04T06:34:48.251Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_3 :: NewTeamMember
testObject_NewTeamMember_team_3 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000800000006"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling], _copy = fromList [GetBilling]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_4 :: NewTeamMember
testObject_NewTeamMember_team_4 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000008"))), _permissions = Permissions {_self = fromList [GetMemberPermissions], _copy = fromList [GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000300000008"))),(fromJust (readUTCTimeMillis "1864-05-13T00:09:10.655Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_5 :: NewTeamMember
testObject_NewTeamMember_team_5 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000200000008"))), _permissions = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,SetBilling,GetMemberPermissions], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_6 :: NewTeamMember
testObject_NewTeamMember_team_6 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000006"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation], _copy = fromList [DoNotUseDeprecatedDeleteConversation]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000500000007"))),(fromJust (readUTCTimeMillis "1864-05-14T05:06:35.005Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_7 :: NewTeamMember
testObject_NewTeamMember_team_7 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000800000005"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetBilling,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000600000000"))),(fromJust (readUTCTimeMillis "1864-05-07T15:17:15.713Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_8 :: NewTeamMember
testObject_NewTeamMember_team_8 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000200000002"))), _permissions = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,SetBilling,DeleteTeam], _copy = fromList [AddTeamMember,SetBilling,DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000600000002"))),(fromJust (readUTCTimeMillis "1864-05-05T07:15:24.888Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_9 :: NewTeamMember
testObject_NewTeamMember_team_9 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000600000006"))), _permissions = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,SetTeamData,GetMemberPermissions], _copy = fromList [AddTeamMember,SetTeamData]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-07T20:36:46.322Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_10 :: NewTeamMember
testObject_NewTeamMember_team_10 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000700000004"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,SetTeamData,GetMemberPermissions,SetMemberPermissions,DeleteTeam], _copy = fromList [SetMemberPermissions,DeleteTeam]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_11 :: NewTeamMember
testObject_NewTeamMember_team_11 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000700000003"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000300000007"))),(fromJust (readUTCTimeMillis "1864-05-12T04:25:39.676Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_12 :: NewTeamMember
testObject_NewTeamMember_team_12 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000600000007"))), _permissions = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,SetTeamData], _copy = fromList [AddTeamMember,RemoveTeamMember,SetTeamData]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_13 :: NewTeamMember
testObject_NewTeamMember_team_13 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000600000003"))), _permissions = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000006"))),(fromJust (readUTCTimeMillis "1864-05-02T04:41:12.163Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_14 :: NewTeamMember
testObject_NewTeamMember_team_14 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000000000003"))),(fromJust (readUTCTimeMillis "1864-05-06T09:21:25.908Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_15 :: NewTeamMember
testObject_NewTeamMember_team_15 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000700000008"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000000000003"))),(fromJust (readUTCTimeMillis "1864-05-06T06:55:38.558Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_16 :: NewTeamMember
testObject_NewTeamMember_team_16 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000000000006"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName], _copy = fromList [DoNotUseDeprecatedModifyConvName]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_17 :: NewTeamMember
testObject_NewTeamMember_team_17 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [CreateConversation,AddTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling], _copy = fromList [CreateConversation,AddTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000800000008"))),(fromJust (readUTCTimeMillis "1864-05-10T05:36:02.296Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_18 :: NewTeamMember
testObject_NewTeamMember_team_18 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000700000000"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedModifyConvName,SetTeamData,SetMemberPermissions], _copy = fromList [SetTeamData]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000600000006"))),(fromJust (readUTCTimeMillis "1864-05-08T22:31:24.906Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_19 :: NewTeamMember
testObject_NewTeamMember_team_19 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000300000006"))), _permissions = Permissions {_self = fromList [CreateConversation,AddTeamMember,SetBilling], _copy = fromList [AddTeamMember,SetBilling]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000400000002"))),(fromJust (readUTCTimeMillis "1864-05-11T02:31:41.850Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_20 :: NewTeamMember
testObject_NewTeamMember_team_20 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,SetMemberPermissions,DeleteTeam], _copy = fromList [DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000600000001"))),(fromJust (readUTCTimeMillis "1864-05-13T20:13:57.694Z"))), _legalHoldStatus = UserLegalHoldPending}))
