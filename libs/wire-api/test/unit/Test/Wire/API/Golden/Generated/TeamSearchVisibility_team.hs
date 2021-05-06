{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamSearchVisibility_team where

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
testObject_TeamSearchVisibility_team_1 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_1 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_2 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_2 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_3 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_3 = SearchVisibilityStandard
testObject_TeamSearchVisibility_team_4 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_4 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_5 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_5 = SearchVisibilityStandard
testObject_TeamSearchVisibility_team_6 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_6 = SearchVisibilityStandard
testObject_TeamSearchVisibility_team_7 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_7 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_8 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_8 = SearchVisibilityStandard
testObject_TeamSearchVisibility_team_9 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_9 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_10 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_10 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_11 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_11 = SearchVisibilityStandard
testObject_TeamSearchVisibility_team_12 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_12 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_13 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_13 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_14 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_14 = SearchVisibilityStandard
testObject_TeamSearchVisibility_team_15 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_15 = SearchVisibilityStandard
testObject_TeamSearchVisibility_team_16 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_16 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_17 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_17 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_18 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_18 = SearchVisibilityNoNameOutsideTeam
testObject_TeamSearchVisibility_team_19 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_19 = SearchVisibilityStandard
testObject_TeamSearchVisibility_team_20 :: TeamSearchVisibility
testObject_TeamSearchVisibility_team_20 = SearchVisibilityStandard
