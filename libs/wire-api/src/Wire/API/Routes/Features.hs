module Wire.API.Routes.Features where

import Wire.API.Conversation.Role
import Wire.API.Error.Galley
import Wire.API.Team.Feature

type family FeatureErrors cfg where
  FeatureErrors LegalholdConfig =
    '[ 'ActionDenied 'RemoveConversationMember,
       'CannotEnableLegalHoldServiceLargeTeam,
       'LegalHoldNotEnabled,
       'LegalHoldDisableUnimplemented,
       'LegalHoldServiceNotRegistered,
       'UserLegalHoldIllegalOperation,
       'LegalHoldCouldNotBlockConnections
     ]
  FeatureErrors _ = '[]

type family FeatureAPIDesc cfg where
  FeatureAPIDesc EnforceFileDownloadLocationConfig =
    "<p><b>Custom feature: only supported on some dedicated on-prem systems.</b></p>"
  FeatureAPIDesc _ = ""
