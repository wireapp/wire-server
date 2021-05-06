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
testObject_UserLegalHoldStatusResponse_team_1 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\138263\163176")), ulhsrClientId = Just (ClientId {client = "fa"})}
testObject_UserLegalHoldStatusResponse_team_2 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_2 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("XKu\155398\74634\169923")), ulhsrClientId = Just (ClientId {client = "b4"})}
testObject_UserLegalHoldStatusResponse_team_3 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_3 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("'\SI\GS\135220\1019249\&2\60327&")), ulhsrClientId = Just (ClientId {client = "1d"})}
testObject_UserLegalHoldStatusResponse_team_4 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_4 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\DC2GJYL\ACK")), ulhsrClientId = Just (ClientId {client = "65"})}
testObject_UserLegalHoldStatusResponse_team_5 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_5 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "df"})}
testObject_UserLegalHoldStatusResponse_team_6 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_6 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("8sm7\1030517")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_7 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_7 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\1095882k~\48703X\78239K\RSH")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_8 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_8 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_9 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_9 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("/Z\t\83387")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_10 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_10 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("D\160307\&2\CAN\aK")), ulhsrClientId = Just (ClientId {client = "9b"})}
testObject_UserLegalHoldStatusResponse_team_11 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_11 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("<v,Y\EOTL")), ulhsrClientId = Just (ClientId {client = "c0"})}
testObject_UserLegalHoldStatusResponse_team_12 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_12 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("7HQX\DLE\EOT")), ulhsrClientId = Just (ClientId {client = "46"})}
testObject_UserLegalHoldStatusResponse_team_13 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_13 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\ACK")), ulhsrClientId = Just (ClientId {client = "ea"})}
testObject_UserLegalHoldStatusResponse_team_14 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_14 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("E\1026515q\1102467DMJ<")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_15 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_15 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "77"})}
testObject_UserLegalHoldStatusResponse_team_16 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_16 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "56"})}
testObject_UserLegalHoldStatusResponse_team_17 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_17 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\149290")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_18 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_18 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("\SUB\DLE\SI\174384b/8\SI\1090389")), ulhsrClientId = Just (ClientId {client = "81"})}
testObject_UserLegalHoldStatusResponse_team_19 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_19 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("$+\151917")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_20 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_20 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("+")), ulhsrClientId = Just (ClientId {client = "27"})}
