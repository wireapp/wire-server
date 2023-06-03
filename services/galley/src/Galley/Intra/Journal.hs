-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Intra.Journal
  ( teamActivate,
    teamUpdate,
    teamDelete,
    teamSuspend,
    evData,
    getBillingUserIds,
  )
where

import Control.Lens
import Data.ByteString.Conversion
import qualified Data.Currency as Currency
import Data.Id
import Data.Proto.Id
import Data.ProtoLens (defMessage)
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Galley.API.Util
import Galley.Effects.TeamStore
import qualified Galley.Options as Opts
import Galley.Types.Teams
import Imports hiding (head)
import Numeric.Natural
import Polysemy
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import Proto.TeamEvents (TeamEvent'EventData, TeamEvent'EventType (..))
import qualified Proto.TeamEvents_Fields as T
import System.Logger (field, msg, val)
import Wire.API.Team.Member
import Wire.API.Team.Permission

-- [Note: journaling]
-- Team journal operations to SQS are a no-op when the service
-- is started without journaling arguments

teamActivate ::
  ( Member (Input Opts.Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  TeamId ->
  Natural ->
  Maybe Currency.Alpha ->
  Maybe TeamCreationTime ->
  Sem r ()
teamActivate tid teamSize cur time = do
  billingUserIds <- getBillingUserIds tid Nothing
  journalEvent TeamEvent'TEAM_ACTIVATE tid (Just $ evData teamSize billingUserIds cur) time

teamUpdate ::
  ( Member TeamStore r,
    Member (Input UTCTime) r
  ) =>
  TeamId ->
  Natural ->
  [UserId] ->
  Sem r ()
teamUpdate tid teamSize billingUserIds =
  journalEvent TeamEvent'TEAM_UPDATE tid (Just $ evData teamSize billingUserIds Nothing) Nothing

teamDelete ::
  ( Member TeamStore r,
    Member (Input UTCTime) r
  ) =>
  TeamId ->
  Sem r ()
teamDelete tid = journalEvent TeamEvent'TEAM_DELETE tid Nothing Nothing

teamSuspend ::
  ( Member TeamStore r,
    Member (Input UTCTime) r
  ) =>
  TeamId ->
  Sem r ()
teamSuspend tid = journalEvent TeamEvent'TEAM_SUSPEND tid Nothing Nothing

journalEvent ::
  ( Member TeamStore r,
    Member (Input UTCTime) r
  ) =>
  TeamEvent'EventType ->
  TeamId ->
  Maybe TeamEvent'EventData ->
  Maybe TeamCreationTime ->
  Sem r ()
journalEvent typ tid dat tim = do
  -- writetime is in microseconds in cassandra 3.11
  now <- round . utcTimeToPOSIXSeconds <$> input
  let ts = maybe now ((`div` 1000000) . view tcTime) tim
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

-- FUTUREWORK: Remove this function and always get billing users ids using
-- 'getBillingTeamMembers'. This is required only until data is backfilled in the
-- 'billing_team_user' table.
getBillingUserIds ::
  ( Member (Input Opts.Opts) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  TeamId ->
  Maybe TeamMemberList ->
  Sem r [UserId]
getBillingUserIds tid maybeMemberList = do
  opts <- input @Opts.Opts
  let enableIndexedBillingTeamMembers =
        fromMaybe False $ opts.settings.enableIndexedBillingTeamMembers
  case maybeMemberList of
    Nothing ->
      if enableIndexedBillingTeamMembers
        then fetchFromDB
        else do
          mems <- getTeamMembersForFanout tid
          handleList enableIndexedBillingTeamMembers mems
    Just list -> handleList enableIndexedBillingTeamMembers list
  where
    fetchFromDB :: Member TeamStore r => Sem r [UserId]
    fetchFromDB = getBillingTeamMembers tid

    filterFromMembers :: TeamMemberList -> Sem r [UserId]
    filterFromMembers list =
      pure $ map (view userId) $ filter (`hasPermission` SetBilling) (list ^. teamMembers)

    handleList ::
      ( Member TeamStore r,
        Member P.TinyLog r
      ) =>
      Bool ->
      TeamMemberList ->
      Sem r [UserId]
    handleList enableIndexedBillingTeamMembers list =
      case list ^. teamMemberListType of
        ListTruncated ->
          if enableIndexedBillingTeamMembers
            then fetchFromDB
            else do
              P.warn $
                field "team" (toByteString tid)
                  . msg (val "TeamMemberList is incomplete, you may not see all the admin users in team. Please enable the indexedBillingTeamMembers feature.")
              filterFromMembers list
        ListComplete -> filterFromMembers list
