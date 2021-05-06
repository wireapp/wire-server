{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Role_team where

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
testObject_Role_team_1 :: Role
testObject_Role_team_1 = RoleExternalPartner
testObject_Role_team_2 :: Role
testObject_Role_team_2 = RoleExternalPartner
testObject_Role_team_3 :: Role
testObject_Role_team_3 = RoleOwner
testObject_Role_team_4 :: Role
testObject_Role_team_4 = RoleOwner
testObject_Role_team_5 :: Role
testObject_Role_team_5 = RoleMember
testObject_Role_team_6 :: Role
testObject_Role_team_6 = RoleOwner
testObject_Role_team_7 :: Role
testObject_Role_team_7 = RoleAdmin
testObject_Role_team_8 :: Role
testObject_Role_team_8 = RoleMember
testObject_Role_team_9 :: Role
testObject_Role_team_9 = RoleMember
testObject_Role_team_10 :: Role
testObject_Role_team_10 = RoleOwner
testObject_Role_team_11 :: Role
testObject_Role_team_11 = RoleExternalPartner
testObject_Role_team_12 :: Role
testObject_Role_team_12 = RoleAdmin
testObject_Role_team_13 :: Role
testObject_Role_team_13 = RoleAdmin
testObject_Role_team_14 :: Role
testObject_Role_team_14 = RoleExternalPartner
testObject_Role_team_15 :: Role
testObject_Role_team_15 = RoleOwner
testObject_Role_team_16 :: Role
testObject_Role_team_16 = RoleExternalPartner
testObject_Role_team_17 :: Role
testObject_Role_team_17 = RoleExternalPartner
testObject_Role_team_18 :: Role
testObject_Role_team_18 = RoleExternalPartner
testObject_Role_team_19 :: Role
testObject_Role_team_19 = RoleMember
testObject_Role_team_20 :: Role
testObject_Role_team_20 = RoleOwner
