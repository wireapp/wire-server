{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team where

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
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_1 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_1 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 13}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_2 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_2 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 10}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_3 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_3 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureEnabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -63}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_4 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_4 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 74}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_5 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_5 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -42}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_6 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_6 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -3}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_7 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_7 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureEnabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -10}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_8 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_8 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 7}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_9 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_9 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 71}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_10 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_10 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -121}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_11 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_11 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureEnabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -82}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_12 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_12 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureEnabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = 100}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_13 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_13 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -40}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_14 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_14 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureEnabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -49}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_15 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_15 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = 116}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_16 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_16 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 65}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_17 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_17 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureEnabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -50}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_18 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_18 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureEnabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -21}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_19 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_19 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -98}}
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_20 :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_20 = TeamFeatureStatusWithConfig {tfwcStatus = TeamFeatureDisabled, tfwcConfig = TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -77}}
