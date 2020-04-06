module Galley.Intra.Journal
  ( teamActivate,
    teamUpdate,
    teamDelete,
    teamSuspend,
    evData,
  )
where

import Control.Lens
import qualified Data.Currency as Currency
import Data.Id
import Data.Proto
import Data.Proto.Id
import Data.ProtoLens (defMessage)
import Data.Text (pack)
import Galley.App
import qualified Galley.Aws as Aws
import Galley.Types.Teams
import Imports hiding (head)
import Numeric.Natural
import Proto.TeamEvents (TeamEvent'EventData, TeamEvent'EventType (..))
import qualified Proto.TeamEvents_Fields as T

-- [Note: journaling]
-- Team journal operations to SQS are a no-op when the service
-- is started without journaling arguments

teamActivate :: TeamId -> Natural -> [TeamMember] -> Maybe Currency.Alpha -> Maybe TeamCreationTime -> Galley ()
teamActivate tid teamSize mems cur time = journalEvent TeamEvent'TEAM_ACTIVATE tid (Just $ evData teamSize mems cur) time

teamUpdate :: TeamId -> Natural -> [TeamMember] -> Galley ()
teamUpdate tid teamSize mems = journalEvent TeamEvent'TEAM_UPDATE tid (Just $ evData teamSize mems Nothing) Nothing

teamDelete :: TeamId -> Galley ()
teamDelete tid = journalEvent TeamEvent'TEAM_DELETE tid Nothing Nothing

teamSuspend :: TeamId -> Galley ()
teamSuspend tid = journalEvent TeamEvent'TEAM_SUSPEND tid Nothing Nothing

journalEvent :: TeamEvent'EventType -> TeamId -> Maybe TeamEvent'EventData -> Maybe TeamCreationTime -> Galley ()
journalEvent typ tid dat tim = view aEnv >>= \mEnv -> for_ mEnv $ \e -> do
  -- writetime is in microseconds in cassandra 3.11
  ts <- maybe now (return . (`div` 1000000) . view tcTime) tim
  let ev =
        defMessage
          & T.eventType .~ typ
          & T.teamId .~ (toBytes tid)
          & T.utcTime .~ ts
          & T.maybe'eventData .~ dat
  Aws.execute e (Aws.enqueue ev)

----------------------------------------------------------------------------
-- utils

evData :: Natural -> [TeamMember] -> Maybe Currency.Alpha -> TeamEvent'EventData
evData memberCount mems cur =
  defMessage
    & T.memberCount .~ fromIntegral memberCount
    & T.billingUser .~ (toBytes <$> uids)
    & T.maybe'currency .~ (pack . show <$> cur)
  where
    uids = view userId <$> filter (`hasPermission` SetBilling) mems
