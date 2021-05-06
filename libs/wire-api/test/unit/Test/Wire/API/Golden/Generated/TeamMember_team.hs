{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamMember_team where

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
testObject_TeamMember_team_1 :: TeamMember
testObject_TeamMember_team_1 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000000000008"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_2 :: TeamMember
testObject_TeamMember_team_2 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,GetMemberPermissions,SetMemberPermissions], _copy = fromList [GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000800000004"))),(fromJust (readUTCTimeMillis "1864-05-15T01:06:29.074Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_3 :: TeamMember
testObject_TeamMember_team_3 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000800000006"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_4 :: TeamMember
testObject_TeamMember_team_4 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000500000007"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,GetBilling,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000700000000"))),(fromJust (readUTCTimeMillis "1864-05-11T11:00:13.283Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_5 :: TeamMember
testObject_TeamMember_team_5 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000004"))), _permissions = Permissions {_self = fromList [AddTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetMemberPermissions,DeleteTeam], _copy = fromList [AddTeamMember,GetBilling,SetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000000000003"))),(fromJust (readUTCTimeMillis "1864-05-11T12:30:26.092Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_6 :: TeamMember
testObject_TeamMember_team_6 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000700000000"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetTeamData,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000700000005"))),(fromJust (readUTCTimeMillis "1864-05-16T14:03:01.103Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_7 :: TeamMember
testObject_TeamMember_team_7 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000100000007"))), _permissions = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName], _copy = fromList [AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000700000007"))),(fromJust (readUTCTimeMillis "1864-05-09T10:40:02.682Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_8 :: TeamMember
testObject_TeamMember_team_8 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000000"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetMemberPermissions], _copy = fromList [AddTeamMember,DoNotUseDeprecatedModifyConvName,SetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000000000002"))),(fromJust (readUTCTimeMillis "1864-05-13T17:16:41.656Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_9 :: TeamMember
testObject_TeamMember_team_9 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001"))), _permissions = Permissions {_self = fromList [CreateConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,SetBilling,DeleteTeam], _copy = fromList [SetBilling]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000300000005"))),(fromJust (readUTCTimeMillis "1864-05-16T05:29:31.814Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_10 :: TeamMember
testObject_TeamMember_team_10 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_11 :: TeamMember
testObject_TeamMember_team_11 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000400000008"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000600000000"))),(fromJust (readUTCTimeMillis "1864-05-12T03:40:38.202Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_12 :: TeamMember
testObject_TeamMember_team_12 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,DeleteTeam], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_13 :: TeamMember
testObject_TeamMember_team_13 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000002"))), _permissions = Permissions {_self = fromList [AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000400000005"))),(fromJust (readUTCTimeMillis "1864-05-03T01:39:45.961Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_14 :: TeamMember
testObject_TeamMember_team_14 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000400000002"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,GetBilling,SetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000600000003"))),(fromJust (readUTCTimeMillis "1864-05-06T18:24:51.381Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_15 :: TeamMember
testObject_TeamMember_team_15 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000002"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,SetTeamData,GetMemberPermissions], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_16 :: TeamMember
testObject_TeamMember_team_16 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000400000007"))), _permissions = Permissions {_self = fromList [SetBilling,SetTeamData,GetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [SetTeamData,GetMemberPermissions,DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000400000006"))),(fromJust (readUTCTimeMillis "1864-05-12T23:46:10.599Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_17 :: TeamMember
testObject_TeamMember_team_17 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000800000005"))), _permissions = Permissions {_self = fromList [SetBilling], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_18 :: TeamMember
testObject_TeamMember_team_18 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000000000008"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,GetBilling,SetBilling], _copy = fromList [AddTeamMember,GetBilling,SetBilling]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000300000007"))),(fromJust (readUTCTimeMillis "1864-05-06T03:18:30.615Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_19 :: TeamMember
testObject_TeamMember_team_19 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000600000002"))), _permissions = Permissions {_self = fromList [CreateConversation,GetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000700000006"))),(fromJust (readUTCTimeMillis "1864-05-02T22:31:21.168Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_20 :: TeamMember
testObject_TeamMember_team_20 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000300000004"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,GetBilling,SetBilling,SetMemberPermissions,DeleteTeam], _copy = fromList [GetBilling,SetBilling,DeleteTeam]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}
