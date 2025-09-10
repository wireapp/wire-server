{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamSubsystem where

import Data.Id
import Data.Range
import Imports
import Polysemy
import Wire.API.Team.Member

data TeamSubsystem m a where
  InternalGetTeamMember :: UserId -> TeamId -> TeamSubsystem m (Maybe TeamMember)
  InternalGetTeamMembers :: TeamId -> Maybe (Range 1 HardTruncationLimit Int32) -> TeamSubsystem m TeamMemberList
  InternalGetTeamMembersByIds :: TeamId -> [UserId] -> TeamSubsystem m TeamMemberList
  InternalGetTeamAdmins :: TeamId -> TeamSubsystem m TeamMemberList

makeSem ''TeamSubsystem
