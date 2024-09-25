{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamInvitationSubsystem where

import Data.Id
import Data.Qualified
import Polysemy
import Wire.API.Team.Invitation
import Wire.API.User (InvitationCode)
import Wire.API.User.EmailAddress

data TeamInvitationSubsystem m a where
  InviteUser :: Local UserId -> TeamId -> InvitationRequest -> TeamInvitationSubsystem m (Invitation, InvitationLocation)
  AcceptInvitation :: UserId -> InvitationId -> InvitationCode -> TeamInvitationSubsystem m ()
  RevokeInvitation :: TeamId -> InvitationId -> TeamInvitationSubsystem m ()
  GetInvitationByCode :: InvitationCode -> TeamInvitationSubsystem m Invitation
  GetInvitationByEmail :: EmailAddress -> TeamInvitationSubsystem m Invitation
  CheckInvitationsByEmail :: EmailAddress -> TeamInvitationSubsystem m HeadInvitationByEmailResult
  DeleteAllInvitationsFor :: TeamId -> TeamInvitationSubsystem m ()

makeSem ''TeamInvitationSubsystem
