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
testObject_LegalHoldServiceRemove_team_1 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000045-0000-004f-0000-003a0000000e"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000076-0000-0016-0000-00570000007f")))}
testObject_LegalHoldServiceRemove_team_2 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_2 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000009-0000-0065-0000-002800000060"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000030-0000-0002-0000-00580000006a")))}
testObject_LegalHoldServiceRemove_team_3 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_3 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000006-0000-0048-0000-007a00000011"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000075-0000-0006-0000-000200000051")))}
testObject_LegalHoldServiceRemove_team_4 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_4 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000018-0000-0074-0000-003c0000006a"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000004-0000-005b-0000-002800000064")))}
testObject_LegalHoldServiceRemove_team_5 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_5 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000051-0000-004c-0000-002600000051"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000051-0000-0048-0000-007d0000003e")))}
