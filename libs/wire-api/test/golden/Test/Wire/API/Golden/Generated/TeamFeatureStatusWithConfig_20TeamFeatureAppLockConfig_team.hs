{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.
module Test.Wire.API.Golden.Generated.TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team where

import Imports (Bool (False, True))
import Wire.API.Team.Feature
  ( EnforceAppLock (EnforceAppLock),
    TeamFeatureAppLockConfig (..),
    TeamFeatureStatusValue (TeamFeatureDisabled, TeamFeatureEnabled),
    TeamFeatureStatusWithConfig (..),
  )

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_1 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_1 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -98}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_2 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_2 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = 14}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_3 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_3 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 92}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_4 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_4 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = 45}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_5 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_5 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 119}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_6 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_6 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -50}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_7 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_7 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -50}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_8 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_8 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -76}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_9 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_9 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = 96}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_10 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_10 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = 120}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_11 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_11 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 62}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_12 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_12 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -50}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_13 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_13 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -99}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_14 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_14 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -96}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_15 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_15 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -12}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_16 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_16 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = -60}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_17 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_17 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 100}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_18 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_18 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 74}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_19 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_19 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureDisabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock False, applockInactivityTimeoutSecs = -125}
    }

testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_20 ::
  TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
testObject_TeamFeatureStatusWithConfig_20TeamFeatureAppLockConfig_team_20 =
  TeamFeatureStatusWithConfig
    { tfwcStatus = TeamFeatureEnabled,
      tfwcConfig =
        TeamFeatureAppLockConfig {applockEnforceAppLock = EnforceAppLock True, applockInactivityTimeoutSecs = 69}
    }
