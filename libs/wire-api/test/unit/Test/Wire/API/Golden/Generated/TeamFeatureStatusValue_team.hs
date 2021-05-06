{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamFeatureStatusValue_team where

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
testObject_TeamFeatureStatusValue_team_1 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_1 = TeamFeatureEnabled
testObject_TeamFeatureStatusValue_team_2 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_2 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_3 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_3 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_4 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_4 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_5 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_5 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_6 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_6 = TeamFeatureEnabled
testObject_TeamFeatureStatusValue_team_7 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_7 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_8 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_8 = TeamFeatureEnabled
testObject_TeamFeatureStatusValue_team_9 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_9 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_10 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_10 = TeamFeatureEnabled
testObject_TeamFeatureStatusValue_team_11 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_11 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_12 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_12 = TeamFeatureEnabled
testObject_TeamFeatureStatusValue_team_13 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_13 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_14 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_14 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_15 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_15 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_16 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_16 = TeamFeatureEnabled
testObject_TeamFeatureStatusValue_team_17 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_17 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_18 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_18 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_19 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_19 = TeamFeatureDisabled
testObject_TeamFeatureStatusValue_team_20 :: TeamFeatureStatusValue
testObject_TeamFeatureStatusValue_team_20 = TeamFeatureDisabled
