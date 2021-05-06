{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EventType_team where

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
testObject_EventType_team_1 :: EventType
testObject_EventType_team_1 = MemberLeave
testObject_EventType_team_2 :: EventType
testObject_EventType_team_2 = MemberUpdate
testObject_EventType_team_3 :: EventType
testObject_EventType_team_3 = MemberLeave
testObject_EventType_team_4 :: EventType
testObject_EventType_team_4 = TeamCreate
testObject_EventType_team_5 :: EventType
testObject_EventType_team_5 = ConvDelete
testObject_EventType_team_6 :: EventType
testObject_EventType_team_6 = MemberLeave
testObject_EventType_team_7 :: EventType
testObject_EventType_team_7 = TeamDelete
testObject_EventType_team_8 :: EventType
testObject_EventType_team_8 = TeamDelete
testObject_EventType_team_9 :: EventType
testObject_EventType_team_9 = TeamDelete
testObject_EventType_team_10 :: EventType
testObject_EventType_team_10 = TeamCreate
testObject_EventType_team_11 :: EventType
testObject_EventType_team_11 = TeamDelete
testObject_EventType_team_12 :: EventType
testObject_EventType_team_12 = MemberUpdate
testObject_EventType_team_13 :: EventType
testObject_EventType_team_13 = MemberUpdate
testObject_EventType_team_14 :: EventType
testObject_EventType_team_14 = TeamCreate
testObject_EventType_team_15 :: EventType
testObject_EventType_team_15 = MemberUpdate
testObject_EventType_team_16 :: EventType
testObject_EventType_team_16 = TeamUpdate
testObject_EventType_team_17 :: EventType
testObject_EventType_team_17 = TeamUpdate
testObject_EventType_team_18 :: EventType
testObject_EventType_team_18 = MemberLeave
testObject_EventType_team_19 :: EventType
testObject_EventType_team_19 = ConvCreate
testObject_EventType_team_20 :: EventType
testObject_EventType_team_20 = TeamUpdate
