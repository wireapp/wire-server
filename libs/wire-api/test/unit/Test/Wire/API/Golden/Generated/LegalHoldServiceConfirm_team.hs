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
testObject_LegalHoldServiceConfirm_team_1 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_1 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "1a"}, lhcUserId = (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000100000007"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000500000006"))), lhcRefreshToken = "l\1104573\1029954\8603"}
testObject_LegalHoldServiceConfirm_team_2 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_2 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "11"}, lhcUserId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000600000003"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000100000002"))), lhcRefreshToken = "\1094742"}
testObject_LegalHoldServiceConfirm_team_3 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_3 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "1c"}, lhcUserId = (Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000200000007"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), lhcRefreshToken = "jP\ESC"}
testObject_LegalHoldServiceConfirm_team_4 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_4 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "d"}, lhcUserId = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000600000004"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000500000006"))), lhcRefreshToken = "mf\"\1030237={"}
testObject_LegalHoldServiceConfirm_team_5 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_5 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "17"}, lhcUserId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000800000006"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000500000004"))), lhcRefreshToken = "\SOH)["}
testObject_LegalHoldServiceConfirm_team_6 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_6 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "6"}, lhcUserId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000006"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000500000006"))), lhcRefreshToken = "\NAK,g\t\1078736\991502"}
testObject_LegalHoldServiceConfirm_team_7 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_7 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "9"}, lhcUserId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000500000004"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000500000007"))), lhcRefreshToken = "\US-\1024888',\1099319"}
testObject_LegalHoldServiceConfirm_team_8 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_8 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "b"}, lhcUserId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000200000000"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000700000000"))), lhcRefreshToken = ""}
testObject_LegalHoldServiceConfirm_team_9 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_9 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "15"}, lhcUserId = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000001"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000800000000"))), lhcRefreshToken = "[YWv"}
testObject_LegalHoldServiceConfirm_team_10 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_10 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "b"}, lhcUserId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000400000000"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000800000008"))), lhcRefreshToken = "U:tYe"}
testObject_LegalHoldServiceConfirm_team_11 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_11 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "e"}, lhcUserId = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000600000002"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000200000005"))), lhcRefreshToken = "\992866\ENQ\DC1\119264\1026576\DLE"}
testObject_LegalHoldServiceConfirm_team_12 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_12 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "4"}, lhcUserId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000400000008"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000005"))), lhcRefreshToken = "\1066336\f\DC1\v"}
testObject_LegalHoldServiceConfirm_team_13 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_13 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "1e"}, lhcUserId = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000100000005"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000004"))), lhcRefreshToken = ""}
testObject_LegalHoldServiceConfirm_team_14 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_14 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "1a"}, lhcUserId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000600000004"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000400000001"))), lhcRefreshToken = "\NAKh>\989975s"}
testObject_LegalHoldServiceConfirm_team_15 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_15 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "18"}, lhcUserId = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000800000000"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000600000008"))), lhcRefreshToken = "(\RS.o"}
testObject_LegalHoldServiceConfirm_team_16 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_16 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "18"}, lhcUserId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000700000008"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000800000001"))), lhcRefreshToken = "}+\185350\1023011\DEL\1034960"}
testObject_LegalHoldServiceConfirm_team_17 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_17 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "1b"}, lhcUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000200000006"))), lhcRefreshToken = "\DLES\STX"}
testObject_LegalHoldServiceConfirm_team_18 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_18 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "18"}, lhcUserId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000500000006"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000800000001"))), lhcRefreshToken = ""}
testObject_LegalHoldServiceConfirm_team_19 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_19 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "16"}, lhcUserId = (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000800000007"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000004"))), lhcRefreshToken = ")"}
testObject_LegalHoldServiceConfirm_team_20 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_20 = LegalHoldServiceConfirm {lhcClientId = ClientId {client = "4"}, lhcUserId = (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000300000005"))), lhcTeamId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000500000000"))), lhcRefreshToken = ""}
