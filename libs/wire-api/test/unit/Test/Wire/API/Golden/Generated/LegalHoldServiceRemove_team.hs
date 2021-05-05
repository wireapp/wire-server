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
testObject_LegalHoldServiceRemove_1 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_1 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000053-0000-0001-0000-001600000067"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000003b-0000-0057-0000-007300000042")))}
testObject_LegalHoldServiceRemove_2 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_2 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-003700000001"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000077-0000-0034-0000-003b0000002e")))}
testObject_LegalHoldServiceRemove_3 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_3 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000012-0000-0053-0000-005e0000006c"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000056-0000-0056-0000-003000000000")))}
testObject_LegalHoldServiceRemove_4 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_4 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000028-0000-001e-0000-004b00000006"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000006f-0000-0007-0000-006600000032")))}
testObject_LegalHoldServiceRemove_5 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_5 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000003c-0000-0047-0000-000700000017"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000007a-0000-004b-0000-003d0000005f")))}
