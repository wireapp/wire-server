{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RequestNewLegalHoldClient_team where

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
testObject_RequestNewLegalHoldClient_team_1 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_1 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000021-0000-0010-0000-002d0000001d")))) ((Id (fromJust (UUID.fromString "0000005f-0000-007b-0000-007100000036")))))
testObject_RequestNewLegalHoldClient_team_2 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_2 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000014-0000-002a-0000-005f0000000e")))) ((Id (fromJust (UUID.fromString "0000000b-0000-006a-0000-006000000016")))))
testObject_RequestNewLegalHoldClient_team_3 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_3 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000001-0000-0030-0000-003e00000048")))) ((Id (fromJust (UUID.fromString "00000035-0000-0002-0000-00290000001e")))))
testObject_RequestNewLegalHoldClient_team_4 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_4 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000007e-0000-0019-0000-00440000000c")))) ((Id (fromJust (UUID.fromString "00000052-0000-002d-0000-001f00000056")))))
testObject_RequestNewLegalHoldClient_team_5 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_5 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000059-0000-0011-0000-000500000078")))) ((Id (fromJust (UUID.fromString "0000007b-0000-0014-0000-00400000002b")))))
