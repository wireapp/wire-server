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
testObject_NewTeamMember_team_1 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000005"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000400000006"))),(fromJust (readUTCTimeMillis "1864-05-07T14:27:51.332Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_2 :: NewTeamMember
testObject_NewTeamMember_team_2 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000100000003"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000200000000"))),(fromJust (readUTCTimeMillis "1864-05-10T07:33:38.904Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_3 :: NewTeamMember
testObject_NewTeamMember_team_3 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000200000000"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedAddRemoveConvMember,SetMemberPermissions], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,SetMemberPermissions]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_4 :: NewTeamMember
testObject_NewTeamMember_team_4 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-000100000000"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,GetMemberPermissions], _copy = fromList [CreateConversation]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_5 :: NewTeamMember
testObject_NewTeamMember_team_5 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000500000008"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,GetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000500000006"))),(fromJust (readUTCTimeMillis "1864-05-04T21:24:40.045Z"))), _legalHoldStatus = UserLegalHoldPending}))
