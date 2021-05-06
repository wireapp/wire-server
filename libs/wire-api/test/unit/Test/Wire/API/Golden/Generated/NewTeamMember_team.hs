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
testObject_NewTeamMember_team_1 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000700000002"))), _permissions = Permissions {_self = fromList [GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000400000008"))),(fromJust (readUTCTimeMillis "1864-05-14T14:36:29.453Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_2 :: NewTeamMember
testObject_NewTeamMember_team_2 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000600000001"))), _permissions = Permissions {_self = fromList [CreateConversation,AddTeamMember,RemoveTeamMember,SetBilling,GetMemberPermissions,DeleteTeam], _copy = fromList [RemoveTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000500000008"))),(fromJust (readUTCTimeMillis "1864-05-03T10:59:57.001Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_3 :: NewTeamMember
testObject_NewTeamMember_team_3 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000200000004"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,GetMemberPermissions,SetMemberPermissions], _copy = fromList [RemoveTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000300000004"))),(fromJust (readUTCTimeMillis "1864-05-14T20:55:10.041Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_4 :: NewTeamMember
testObject_NewTeamMember_team_4 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000000000003"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,GetBilling,SetBilling,SetMemberPermissions,DeleteTeam], _copy = fromList [RemoveTeamMember,SetBilling,SetMemberPermissions,DeleteTeam]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_5 :: NewTeamMember
testObject_NewTeamMember_team_5 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000200000005"))), _permissions = Permissions {_self = fromList [GetTeamConversations], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_6 :: NewTeamMember
testObject_NewTeamMember_team_6 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000600000001"))), _permissions = Permissions {_self = fromList [GetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000006"))),(fromJust (readUTCTimeMillis "1864-05-12T23:15:07.006Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_7 :: NewTeamMember
testObject_NewTeamMember_team_7 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000600000008"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,SetBilling,SetTeamData], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000005"))),(fromJust (readUTCTimeMillis "1864-05-04T02:11:55.800Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_8 :: NewTeamMember
testObject_NewTeamMember_team_8 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000002"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000700000000"))),(fromJust (readUTCTimeMillis "1864-05-12T08:45:04.984Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_9 :: NewTeamMember
testObject_NewTeamMember_team_9 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000700000008"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,GetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations], _copy = fromList [GetTeamConversations]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000800000004"))),(fromJust (readUTCTimeMillis "1864-05-08T20:29:23.691Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_10 :: NewTeamMember
testObject_NewTeamMember_team_10 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000600000005"))), _permissions = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000600000005"))),(fromJust (readUTCTimeMillis "1864-05-07T03:12:23.170Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_11 :: NewTeamMember
testObject_NewTeamMember_team_11 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000300000004"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000300000003"))),(fromJust (readUTCTimeMillis "1864-05-16T02:12:38.572Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_12 :: NewTeamMember
testObject_NewTeamMember_team_12 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000005"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,SetBilling,SetTeamData,GetMemberPermissions,GetTeamConversations], _copy = fromList [SetBilling]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000800000005"))),(fromJust (readUTCTimeMillis "1864-05-06T21:55:47.986Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_13 :: NewTeamMember
testObject_NewTeamMember_team_13 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000400000005"))), _permissions = Permissions {_self = fromList [GetBilling,SetBilling,SetTeamData,SetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000600000000"))),(fromJust (readUTCTimeMillis "1864-05-15T18:16:36.079Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_14 :: NewTeamMember
testObject_NewTeamMember_team_14 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000500000007"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations], _copy = fromList [SetTeamData]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_15 :: NewTeamMember
testObject_NewTeamMember_team_15 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000600000008"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedAddRemoveConvMember,SetTeamData], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000300000002"))),(fromJust (readUTCTimeMillis "1864-05-07T15:05:27.490Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_16 :: NewTeamMember
testObject_NewTeamMember_team_16 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000800000000"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling,SetTeamData,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000300000005"))),(fromJust (readUTCTimeMillis "1864-05-08T06:07:37.110Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_17 :: NewTeamMember
testObject_NewTeamMember_team_17 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000400000000"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,GetBilling], _copy = fromList [DoNotUseDeprecatedDeleteConversation]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000200000000"))),(fromJust (readUTCTimeMillis "1864-05-06T06:51:56.509Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_18 :: NewTeamMember
testObject_NewTeamMember_team_18 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000800000007"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000600000002"))),(fromJust (readUTCTimeMillis "1864-05-07T11:12:47.824Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_19 :: NewTeamMember
testObject_NewTeamMember_team_19 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000800000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,GetTeamConversations], _copy = fromList [SetBilling]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_20 :: NewTeamMember
testObject_NewTeamMember_team_20 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000008"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
