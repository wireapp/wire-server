{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH
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

module Wire.TeamJournal where

import Control.Lens
import Data.Currency qualified as Currency
import Data.Id
import Data.Proto.Id
import Data.ProtoLens (defMessage)
import Data.Text (pack)
import Data.Time.Clock.POSIX
import Imports hiding (head)
import Numeric.Natural
import Polysemy
import Proto.TeamEvents (TeamEvent, TeamEvent'EventData, TeamEvent'EventType (..))
import Proto.TeamEvents_Fields qualified as T
import Wire.API.Team (TeamCreationTime (..))
import Wire.Sem.Now
import Wire.Sem.Now qualified as Now
import Wire.TeamStore

data TeamJournal m a where
  EnqueueTeamEvent :: TeamEvent -> TeamJournal m ()

makeSem ''TeamJournal

-- Note [journaling]
-- ~~~~~~~~~~~~~~~~~
-- Team journal operations to SQS are a no-op when the service
-- is started without journaling arguments

teamActivate ::
  ( Member Now r,
    Member TeamStore r,
    Member TeamJournal r
  ) =>
  TeamId ->
  Natural ->
  Maybe Currency.Alpha ->
  Maybe TeamCreationTime ->
  Sem r ()
teamActivate tid teamSize cur time = do
  owners <- getBillingTeamMembers tid
  journalEvent TeamEvent'TEAM_ACTIVATE tid (Just $ evData teamSize owners cur) time

teamUpdate ::
  ( Member Now r,
    Member TeamJournal r
  ) =>
  TeamId ->
  Natural ->
  [UserId] ->
  Sem r ()
teamUpdate tid teamSize billingUserIds =
  journalEvent TeamEvent'TEAM_UPDATE tid (Just $ evData teamSize billingUserIds Nothing) Nothing

teamDelete ::
  ( Member Now r,
    Member TeamJournal r
  ) =>
  TeamId ->
  Sem r ()
teamDelete tid = journalEvent TeamEvent'TEAM_DELETE tid Nothing Nothing

teamSuspend ::
  ( Member Now r,
    Member TeamJournal r
  ) =>
  TeamId ->
  Sem r ()
teamSuspend tid = journalEvent TeamEvent'TEAM_SUSPEND tid Nothing Nothing

journalEvent ::
  ( Member Now r,
    Member TeamJournal r
  ) =>
  TeamEvent'EventType ->
  TeamId ->
  Maybe TeamEvent'EventData ->
  Maybe TeamCreationTime ->
  Sem r ()
journalEvent typ tid dat tim = do
  -- writetime is in microseconds in cassandra 3.11
  now <- round . utcTimeToPOSIXSeconds <$> Now.get
  let ts = maybe now ((`div` 1000000) . _tcTime) tim
      ev =
        defMessage
          & T.eventType .~ typ
          & T.teamId .~ toBytes tid
          & T.utcTime .~ ts
          & T.maybe'eventData .~ dat
  enqueueTeamEvent ev

----------------------------------------------------------------------------
-- utils

evData :: Natural -> [UserId] -> Maybe Currency.Alpha -> TeamEvent'EventData
evData memberCount billingUserIds cur =
  defMessage
    & T.memberCount .~ fromIntegral memberCount
    & T.billingUser .~ (toBytes <$> billingUserIds)
    & T.maybe'currency .~ (pack . show <$> cur)
