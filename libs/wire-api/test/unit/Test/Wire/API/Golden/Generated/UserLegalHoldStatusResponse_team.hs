{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserLegalHoldStatusResponse_team where

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
testObject_UserLegalHoldStatusResponse_team_1 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_1 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\1081324\1097654YG")), ulhsrClientId = Just (ClientId {client = "75"})}
testObject_UserLegalHoldStatusResponse_team_2 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_2 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "f1"})}
testObject_UserLegalHoldStatusResponse_team_3 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_3 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_4 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_4 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\ACK\\u")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_5 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_5 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\135984")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_6 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_6 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("Z")), ulhsrClientId = Just (ClientId {client = "b0"})}
testObject_UserLegalHoldStatusResponse_team_7 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_7 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_8 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_8 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("=")), ulhsrClientId = Just (ClientId {client = "27"})}
testObject_UserLegalHoldStatusResponse_team_9 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_9 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "4f"})}
testObject_UserLegalHoldStatusResponse_team_10 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_10 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("o\v")), ulhsrClientId = Just (ClientId {client = "e6"})}
testObject_UserLegalHoldStatusResponse_team_11 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_11 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("r")), ulhsrClientId = Just (ClientId {client = "ec"})}
testObject_UserLegalHoldStatusResponse_team_12 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_12 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("\ACK5")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_13 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_13 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\25516K\SYN-JBe\NAKCF")), ulhsrClientId = Just (ClientId {client = "32"})}
testObject_UserLegalHoldStatusResponse_team_14 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_14 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("b/\a\1033850LE\1061067!y")), ulhsrClientId = Just (ClientId {client = "25"})}
testObject_UserLegalHoldStatusResponse_team_15 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_15 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "bc"})}
testObject_UserLegalHoldStatusResponse_team_16 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_16 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\SUB,\1053775+")), ulhsrClientId = Just (ClientId {client = "df"})}
testObject_UserLegalHoldStatusResponse_team_17 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_17 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("&")), ulhsrClientId = Just (ClientId {client = "6e"})}
testObject_UserLegalHoldStatusResponse_team_18 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_18 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\vJ\60760/o\1076749\DC4+T9")), ulhsrClientId = Just (ClientId {client = "52"})}
testObject_UserLegalHoldStatusResponse_team_19 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_19 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("2\1075370Xy")), ulhsrClientId = Just (ClientId {client = "4e"})}
testObject_UserLegalHoldStatusResponse_team_20 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_20 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("0o{W*\ACKC]\132137E")), ulhsrClientId = Nothing}
