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
testObject_Permissions_team_1 = Permissions {_self = fromList [], _copy = fromList []}
testObject_Permissions_team_2 :: Permissions
testObject_Permissions_team_2 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,GetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_3 :: Permissions
testObject_Permissions_team_3 = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetTeamData,SetMemberPermissions,GetTeamConversations], _copy = fromList [DoNotUseDeprecatedModifyConvName,SetTeamData,SetMemberPermissions,GetTeamConversations]}
testObject_Permissions_team_4 :: Permissions
testObject_Permissions_team_4 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,DeleteTeam], _copy = fromList []}
testObject_Permissions_team_5 :: Permissions
testObject_Permissions_team_5 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions], _copy = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,SetTeamData,SetMemberPermissions]}
testObject_Permissions_team_6 :: Permissions
testObject_Permissions_team_6 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_7 :: Permissions
testObject_Permissions_team_7 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,SetBilling,SetTeamData,GetTeamConversations], _copy = fromList [GetTeamConversations]}
testObject_Permissions_team_8 :: Permissions
testObject_Permissions_team_8 = Permissions {_self = fromList [RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetTeamConversations], _copy = fromList [RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetTeamConversations]}
testObject_Permissions_team_9 :: Permissions
testObject_Permissions_team_9 = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_10 :: Permissions
testObject_Permissions_team_10 = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember]}
testObject_Permissions_team_11 :: Permissions
testObject_Permissions_team_11 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,GetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_12 :: Permissions
testObject_Permissions_team_12 = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetMemberPermissions], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetMemberPermissions]}
testObject_Permissions_team_13 :: Permissions
testObject_Permissions_team_13 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,GetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_14 :: Permissions
testObject_Permissions_team_14 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling,SetTeamData,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_15 :: Permissions
testObject_Permissions_team_15 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling]}
testObject_Permissions_team_16 :: Permissions
testObject_Permissions_team_16 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_17 :: Permissions
testObject_Permissions_team_17 = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling], _copy = fromList [DoNotUseDeprecatedModifyConvName,GetBilling]}
testObject_Permissions_team_18 :: Permissions
testObject_Permissions_team_18 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,SetBilling], _copy = fromList [DoNotUseDeprecatedDeleteConversation,SetBilling]}
testObject_Permissions_team_19 :: Permissions
testObject_Permissions_team_19 = Permissions {_self = fromList [RemoveTeamMember,GetMemberPermissions,DeleteTeam], _copy = fromList [RemoveTeamMember,GetMemberPermissions,DeleteTeam]}
testObject_Permissions_team_20 :: Permissions
testObject_Permissions_team_20 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedModifyConvName,GetBilling,GetMemberPermissions,SetMemberPermissions], _copy = fromList [SetMemberPermissions]}
