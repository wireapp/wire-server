{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Permissions_team where

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
testObject_Permissions_team_1 :: Permissions
testObject_Permissions_team_1 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,GetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_2 :: Permissions
testObject_Permissions_team_2 = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,DeleteTeam], _copy = fromList [AddTeamMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions]}
testObject_Permissions_team_3 :: Permissions
testObject_Permissions_team_3 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations]}
testObject_Permissions_team_4 :: Permissions
testObject_Permissions_team_4 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetMemberPermissions], _copy = fromList []}
testObject_Permissions_team_5 :: Permissions
testObject_Permissions_team_5 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,SetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,SetBilling,GetMemberPermissions,SetMemberPermissions,DeleteTeam]}
