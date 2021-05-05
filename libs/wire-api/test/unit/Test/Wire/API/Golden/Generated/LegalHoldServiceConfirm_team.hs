{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LegalHoldServiceConfirm_team where

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
testObject_LegalHoldServiceConfirm_1 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_1 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "15"}, lhcUserId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000600000003"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000300000003"))), lhcRefreshToken = "bg\SOH"}
testObject_LegalHoldServiceConfirm_2 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_2 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "9"}, lhcUserId = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000100000006"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000400000004"))), lhcRefreshToken = "C"}
testObject_LegalHoldServiceConfirm_3 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_3 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "9"}, lhcUserId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000008"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000800000001"))), lhcRefreshToken = "5to\145490\ETBOY"}
testObject_LegalHoldServiceConfirm_4 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_4 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "20"}, lhcUserId = (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000100000000"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000200000008"))), lhcRefreshToken = "\64143b"}
testObject_LegalHoldServiceConfirm_5 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_5 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "a"}, lhcUserId = (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000700000007"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000006"))), lhcRefreshToken = "dh\DC3g)3\143038"}
