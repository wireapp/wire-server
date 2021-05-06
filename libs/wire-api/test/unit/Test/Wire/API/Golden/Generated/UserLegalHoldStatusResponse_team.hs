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
testObject_UserLegalHoldStatusResponse_team_1 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\128697D\195057\1012232R\174054")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_2 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_2 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "ff"})}
testObject_UserLegalHoldStatusResponse_team_3 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_3 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("1\SOH*4nq\159874\1091211\173702\&8")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_4 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_4 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("% z\DLE\33122E")), ulhsrClientId = Just (ClientId {client = "91"})}
testObject_UserLegalHoldStatusResponse_team_5 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_5 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\152689D")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_6 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_6 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_7 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_7 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("SP\1053875")), ulhsrClientId = Just (ClientId {client = "2b"})}
testObject_UserLegalHoldStatusResponse_team_8 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_8 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "34"})}
testObject_UserLegalHoldStatusResponse_team_9 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_9 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("|")), ulhsrClientId = Just (ClientId {client = "b5"})}
testObject_UserLegalHoldStatusResponse_team_10 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_10 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("ye\17719\&2\44985")), ulhsrClientId = Just (ClientId {client = "95"})}
testObject_UserLegalHoldStatusResponse_team_11 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_11 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\SYNds|G\1035361\32376\\A")), ulhsrClientId = Just (ClientId {client = "a3"})}
testObject_UserLegalHoldStatusResponse_team_12 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_12 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("c")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_13 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_13 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("&X\f\FS")), ulhsrClientId = Just (ClientId {client = "fa"})}
testObject_UserLegalHoldStatusResponse_team_14 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_14 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "49"})}
testObject_UserLegalHoldStatusResponse_team_15 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_15 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("\52594\DC26DF")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_16 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_16 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("Q\1075769\20799\EOT{")), ulhsrClientId = Just (ClientId {client = "f7"})}
testObject_UserLegalHoldStatusResponse_team_17 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_17 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("=\DEL\ETB\1043860")), ulhsrClientId = Just (ClientId {client = "e7"})}
testObject_UserLegalHoldStatusResponse_team_18 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_18 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "f5"})}
testObject_UserLegalHoldStatusResponse_team_19 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_19 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\74802`+\SI\DC3\NUL\1022213Us\f")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_20 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_20 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\159432i")), ulhsrClientId = Just (ClientId {client = "12"})}
