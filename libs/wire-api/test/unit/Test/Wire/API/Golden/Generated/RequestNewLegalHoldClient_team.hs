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
testObject_RequestNewLegalHoldClient_team_1 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000016-0000-004d-0000-007700000011")))) ((Id (fromJust (UUID.fromString "00000068-0000-003b-0000-002000000027")))))
testObject_RequestNewLegalHoldClient_team_2 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_2 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000026-0000-0027-0000-001b00000058")))) ((Id (fromJust (UUID.fromString "00000023-0000-0072-0000-005b00000018")))))
testObject_RequestNewLegalHoldClient_team_3 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_3 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000007a-0000-004d-0000-00320000004c")))) ((Id (fromJust (UUID.fromString "00000009-0000-0064-0000-003b00000048")))))
testObject_RequestNewLegalHoldClient_team_4 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_4 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000008-0000-0071-0000-003700000051")))) ((Id (fromJust (UUID.fromString "0000005e-0000-001e-0000-007a00000079")))))
testObject_RequestNewLegalHoldClient_team_5 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_5 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000004c-0000-0055-0000-006800000065")))) ((Id (fromJust (UUID.fromString "00000034-0000-005f-0000-006d0000007a")))))
