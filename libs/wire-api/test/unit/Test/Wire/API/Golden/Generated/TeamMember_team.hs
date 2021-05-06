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
testObject_TeamMember_team_1 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000005"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,SetTeamData,DeleteTeam], _copy = fromList [SetTeamData,DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000001"))),(fromJust (readUTCTimeMillis "1864-05-07T03:14:39.374Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_2 :: TeamMember
testObject_TeamMember_team_2 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000400000000"))), _permissions = Permissions {_self = fromList [AddTeamMember], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000000000006"))),(fromJust (readUTCTimeMillis "1864-05-03T11:33:02.960Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_3 :: TeamMember
testObject_TeamMember_team_3 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000400000007"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedModifyConvName], _copy = fromList [DoNotUseDeprecatedModifyConvName]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_4 :: TeamMember
testObject_TeamMember_team_4 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000800000007"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetTeamData,SetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000008"))),(fromJust (readUTCTimeMillis "1864-05-02T15:40:12.071Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_5 :: TeamMember
testObject_TeamMember_team_5 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000000000000"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,SetTeamData,GetMemberPermissions], _copy = fromList [CreateConversation,AddTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000400000001"))),(fromJust (readUTCTimeMillis "1864-05-06T05:07:01.074Z"))), _legalHoldStatus = UserLegalHoldPending}
