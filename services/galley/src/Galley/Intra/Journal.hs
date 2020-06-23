-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Proto
import Data.Proto.Id
import Data.ProtoLens (defMessage)
import Data.Text (pack)
import Galley.App
import qualified Galley.Aws as Aws
import qualified Galley.Data as Data
import qualified Galley.Options as Opts
import Galley.Types.Teams
import Imports hiding (head)
import Numeric.Natural
import Proto.TeamEvents (TeamEvent'EventData, TeamEvent'EventType (..))
import qualified Proto.TeamEvents_Fields as T
import System.Logger (field, msg, val)
import qualified System.Logger.Class as Log

-- [Note: journaling]
-- Team journal operations to SQS are a no-op when the service
-- is started without journaling arguments

teamActivate :: TeamId -> Natural -> Maybe Currency.Alpha -> Maybe TeamCreationTime -> Galley ()
teamActivate tid teamSize cur time = do
  billingUserIds <- getBillingUserIds tid Nothing
  journalEvent TeamEvent'TEAM_ACTIVATE tid (Just $ evData teamSize billingUserIds cur) time

teamUpdate :: TeamId -> Natural -> [UserId] -> Galley ()
teamUpdate tid teamSize billingUserIds =
  journalEvent TeamEvent'TEAM_UPDATE tid (Just $ evData teamSize billingUserIds Nothing) Nothing

teamDelete :: TeamId -> Galley ()
teamDelete tid = journalEvent TeamEvent'TEAM_DELETE tid Nothing Nothing

teamSuspend :: TeamId -> Galley ()
teamSuspend tid = journalEvent TeamEvent'TEAM_SUSPEND tid Nothing Nothing

journalEvent :: TeamEvent'EventType -> TeamId -> Maybe TeamEvent'EventData -> Maybe TeamCreationTime -> Galley ()
journalEvent typ tid dat tim =
  view aEnv >>= \mEnv -> for_ mEnv $ \e -> do
    -- writetime is in microseconds in cassandra 3.11
    ts <- maybe now (return . (`div` 1000000) . view tcTime) tim
    let ev =
          defMessage
            & T.eventType .~ typ
            & T.teamId .~ toBytes tid
            & T.utcTime .~ ts
            & T.maybe'eventData .~ dat
    Aws.execute e (Aws.enqueue ev)

----------------------------------------------------------------------------
-- utils

evData :: Natural -> [UserId] -> Maybe Currency.Alpha -> TeamEvent'EventData
evData memberCount billingUserIds cur =
  defMessage
    & T.memberCount .~ fromIntegral memberCount
    & T.billingUser .~ (toBytes <$> billingUserIds)
    & T.maybe'currency .~ (pack . show <$> cur)

-- FUTUREWORK: Remove this function and always get billing users ids using
-- 'Data.listBillingTeamMembers'. This is required only until data is backfilled in the
-- 'billing_team_user' table.
getBillingUserIds :: TeamId -> Maybe TeamMemberList -> Galley [UserId]
getBillingUserIds tid maybeMemberList = do
  enableIndexedBillingTeamMembers <- view (options . Opts.optSettings . Opts.setEnableIndexedBillingTeamMembers . to (fromMaybe False))
  case maybeMemberList of
    Nothing ->
      if enableIndexedBillingTeamMembers
        then fetchFromDB
        else handleList enableIndexedBillingTeamMembers =<< Data.teamMembersForFanout tid
    Just list -> handleList enableIndexedBillingTeamMembers list
  where
    fetchFromDB :: Galley [UserId]
    fetchFromDB = Data.listBillingTeamMembers tid

    filterFromMembers :: TeamMemberList -> Galley [UserId]
    filterFromMembers list =
      pure $ map (view userId) $ filter (`hasPermission` SetBilling) (list ^. teamMembers)

    handleList :: Bool -> TeamMemberList -> Galley [UserId]
    handleList enableIndexedBillingTeamMembers list =
      case list ^. teamMemberListType of
        ListTruncated ->
          if enableIndexedBillingTeamMembers
            then fetchFromDB
            else do
              Log.warn $
                field "team" (toByteString tid)
                  . msg (val "TeamMemberList is incomplete, you may not see all the admin users in team. Please enable the indexedBillingTeamMembers feature.")
              filterFromMembers list
        ListComplete -> filterFromMembers list
