{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LegalHoldServiceRemove_team where

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
testObject_LegalHoldServiceRemove_team_1 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_1 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000057-0000-0008-0000-00490000003b"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000061-0000-003c-0000-00270000000e")))}
testObject_LegalHoldServiceRemove_team_2 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_2 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000000d-0000-0051-0000-000400000071"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000006f-0000-0049-0000-000300000035")))}
testObject_LegalHoldServiceRemove_team_3 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_3 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000005e-0000-003a-0000-003400000037"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000064-0000-0038-0000-001300000078")))}
testObject_LegalHoldServiceRemove_team_4 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_4 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000049-0000-000f-0000-00060000004d"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000029-0000-0001-0000-001d0000001b")))}
testObject_LegalHoldServiceRemove_team_5 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_5 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000073-0000-004b-0000-006000000021"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000003d-0000-006c-0000-007e0000003c")))}
