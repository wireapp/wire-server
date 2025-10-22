{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamSubsystem where

import Data.Id
import Data.Range
import Imports
import Polysemy
import Wire.API.Team.Member
import Wire.API.Team.Member.Info (TeamMemberInfoList)

data TeamSubsystem m a where
  InternalGetTeamMember :: UserId -> TeamId -> TeamSubsystem m (Maybe TeamMember)
  InternalGetTeamMembers :: TeamId -> Maybe (Range 1 HardTruncationLimit Int32) -> TeamSubsystem m TeamMemberList
  InternalSelectTeamMemberInfos :: TeamId -> [UserId] -> TeamSubsystem m TeamMemberInfoList
  InternalGetTeamAdmins :: TeamId -> TeamSubsystem m TeamMemberList

makeSem ''TeamSubsystem
