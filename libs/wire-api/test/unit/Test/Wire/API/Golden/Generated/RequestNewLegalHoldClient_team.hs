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
testObject_RequestNewLegalHoldClient_1 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_1 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000023-0000-000d-0000-005600000079")))) ((Id (fromJust (UUID.fromString "00000002-0000-0010-0000-00140000004c")))))
testObject_RequestNewLegalHoldClient_2 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_2 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000063-0000-0028-0000-007e0000000c")))) ((Id (fromJust (UUID.fromString "00000055-0000-001f-0000-006900000080")))))
testObject_RequestNewLegalHoldClient_3 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_3 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000004d-0000-0032-0000-005d00000006")))) ((Id (fromJust (UUID.fromString "00000038-0000-0079-0000-001600000048")))))
testObject_RequestNewLegalHoldClient_4 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_4 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000004f-0000-002a-0000-003a00000034")))) ((Id (fromJust (UUID.fromString "0000005a-0000-002c-0000-002a00000000")))))
testObject_RequestNewLegalHoldClient_5 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_5 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000007f-0000-004d-0000-005e00000023")))) ((Id (fromJust (UUID.fromString "00000048-0000-0058-0000-005800000080")))))
