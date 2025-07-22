{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamSubsystem where

import Data.Id
import Imports
import Polysemy
import Wire.API.Team.Member

data TeamSubsystem m a where
  InternalGetTeamMember :: UserId -> TeamId -> TeamSubsystem m (Maybe TeamMember)
  InternalGetTeamMembers :: TeamId -> TeamSubsystem m TeamMemberList

makeSem ''TeamSubsystem
