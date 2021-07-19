module Test.Wire.API.Golden.Manual.FeatureConfigEvent where

import Imports
import Wire.API.Event.FeatureConfig
import Wire.API.Team.Feature

testObject_FeatureConfigEvent_1 :: Event
testObject_FeatureConfigEvent_1 = Event Update TeamFeatureFileSharing (EdFeatureWithoutConfigChanged (TeamFeatureStatusNoConfig TeamFeatureEnabled))

testObject_FeatureConfigEvent_2 :: Event
testObject_FeatureConfigEvent_2 = Event Update TeamFeatureSSO (EdFeatureWithoutConfigChanged (TeamFeatureStatusNoConfig TeamFeatureDisabled))

testObject_FeatureConfigEvent_3 :: Event
testObject_FeatureConfigEvent_3 =
  Event
    Update
    TeamFeatureAppLock
    ( EdFeatureApplockChanged
        ( TeamFeatureStatusWithConfig
            TeamFeatureDisabled
            (TeamFeatureAppLockConfig (EnforceAppLock True) 300)
        )
    )
