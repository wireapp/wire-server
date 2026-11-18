{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.FanInNotificationSubsystem.Interpreter (interpretFanInNotificationSubsystem) where

import Data.Aeson qualified as A
import Data.Domain (Domain)
import Data.List.NonEmpty qualified as NE
import Data.Qualified (Qualified (..))
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Notification.FanIn
import Wire.FanInEpochMembershipStore
import Wire.FanInNotificationStore
import Wire.FanInNotificationSubsystem
import Wire.Sem.Now qualified as Now

interpretFanInNotificationSubsystem :: forall r. (Member Now.Now r, Member FanInEpochMembershipStore r, Member FanInNotificationStore r, Member (Error FanInNotificationSubsystemError) r) => Maybe Domain -> InterpreterFor FanInNotificationSubsystem r
interpretFanInNotificationSubsystem localDomain = interpret $ \case
  PublishFanInNotifications pushes -> publish pushes
  UpdateFanInMembership update -> do
    now <- Now.get
    replaceFanInCurrentEpochMembers update.groupId update.epoch now [FanInEpochMember m.userId m.clientId | m <- update.members]
  DeliverFanInNotifications {} -> throw FanInOperationNotImplemented
  InitializeFanInClient {} -> throw FanInOperationNotImplemented
  InitializeFanInConnection {} -> throw FanInOperationNotImplemented
  InitializeFanInTeamMember {} -> throw FanInOperationNotImplemented
  ResetFanInAcknowledgements {} -> throw FanInOperationNotImplemented
  where
    stored :: FanInPush -> FanInNotification
    stored p = FanInNotification p.notificationId (A.toJSON [A.Object p.json]) p.origin
    publish :: [FanInPush] -> Sem r ()
    publish pushes = do
      -- Validate every push before the first store operation. A mixed request
      -- containing an unsupported/invalid entry must not be partially accepted.
      for_ pushes $ \p -> do
        when p.transient $ throw FanInTransientUnsupported
        either (const (throw FanInInvalidNotificationId)) (const (pure ())) (fanInNotificationCreatedAt (stored p))
        for_ p.targets $ \case
          FanInTargetConnections {} -> when (isNothing localDomain) $ throw FanInLocalDomainMissing
          _ -> pure ()
      let entries = [(t, stored p) | p <- pushes, t <- NE.toList p.targets]
      for_ (NE.nonEmpty [(u, n) | (FanInTargetUser u, n) <- entries]) appendFanInUserNotifications
      for_ (NE.nonEmpty [((u, c), n) | (FanInTargetUserClients u cs, n) <- entries, c <- NE.toList cs]) appendFanInClientNotifications
      for_ (NE.nonEmpty [(t, n) | (FanInTargetTeam t, n) <- entries]) appendFanInTeamNotifications
      for_ (NE.nonEmpty [((g, e), n) | (FanInTargetEpoch g e, n) <- entries]) appendFanInEpochNotifications
      for_ (NE.nonEmpty [(u, n) | (FanInTargetConnections (Qualified u d), n) <- entries, Just d == localDomain]) appendFanInLocalConnectionNotifications
      for_ (NE.nonEmpty [((d, u), n) | (FanInTargetConnections (Qualified u d), n) <- entries, Just d /= localDomain]) appendFanInRemoteConnectionNotifications
