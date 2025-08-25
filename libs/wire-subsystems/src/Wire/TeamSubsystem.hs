{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamSubsystem where

import Control.Error
import Data.Id
import Data.Range
import Imports
import Polysemy
import Wire.API.Team.Member

data TeamSubsystem m a where
  InternalGetTeamMember :: UserId -> TeamId -> TeamSubsystem m (Maybe TeamMember)
  InternalGetTeamMembers :: TeamId -> Maybe (Range 1 HardTruncationLimit Int32) -> TeamSubsystem m TeamMemberList
  InternalGetTeamAdmins :: TeamId -> TeamSubsystem m TeamMemberList
  InternalGetUserTeam :: UserId -> TeamSubsystem m (Maybe TeamId)

makeSem ''TeamSubsystem

internalGetUserTeamMember :: (Member TeamSubsystem r) => UserId -> Sem r (Maybe TeamMember)
internalGetUserTeamMember uid = runMaybeT $ do
  tid <- MaybeT $ internalGetUserTeam uid
  MaybeT $ internalGetTeamMember uid tid
