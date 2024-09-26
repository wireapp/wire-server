{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamInvitationSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Team.Invitation
import Wire.API.Team.Role
import Wire.API.User (InvitationCode)
import Wire.API.User.EmailAddress

data TeamInvitationSubsystem m a where
  InviteUser :: Local UserId -> TeamId -> InvitationRequest -> TeamInvitationSubsystem m (Invitation, InvitationLocation)
  -- | This function exists to support migration in this susbystem, after the
  -- migration this would just be an internal detail of the subsystem
  InternalCreateInvitation :: TeamId -> Maybe InvitationId -> Role -> Local (Maybe UserId) -> EmailAddress -> InvitationRequest -> TeamInvitationSubsystem m (Invitation, InvitationCode)

makeSem ''TeamInvitationSubsystem
