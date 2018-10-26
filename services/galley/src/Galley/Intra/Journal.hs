{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Intra.Journal
    ( teamActivate
    , teamUpdate
    , teamDelete
    , teamSuspend
    , evData
    ) where

import Control.Lens
import Data.Foldable (for_)
import Data.Id
import Data.Text (pack)
import Galley.Types.Teams
import Data.Proto
import Data.Proto.Id
import Galley.App
import Prelude hiding (head, mapM)
import Proto.TeamEvents

import qualified Data.Currency as Currency
import qualified Galley.Aws as Aws

-- [Note: journaling]
-- Team journal operations to SQS are a no-op when the service
-- is started without journaling arguments

teamActivate :: TeamId -> [TeamMember] -> Maybe Currency.Alpha -> Maybe TeamCreationTime -> Galley ()
teamActivate tid mems cur time = journalEvent TeamEvent'TEAM_ACTIVATE tid (Just $ evData mems cur) time

teamUpdate :: TeamId -> [TeamMember] -> Galley ()
teamUpdate tid mems = journalEvent TeamEvent'TEAM_UPDATE tid (Just $ evData mems Nothing) Nothing

teamDelete :: TeamId -> Galley ()
teamDelete tid = journalEvent TeamEvent'TEAM_DELETE tid Nothing Nothing

teamSuspend :: TeamId -> Galley ()
teamSuspend tid = journalEvent TeamEvent'TEAM_SUSPEND tid Nothing Nothing

journalEvent :: TeamEvent'EventType -> TeamId -> Maybe TeamEvent'EventData -> Maybe TeamCreationTime -> Galley ()
journalEvent typ tid dat tim = view aEnv >>= \mEnv -> for_ mEnv $ \e -> do
    -- writetime is in microseconds in cassandra 3.11
    ts <- maybe now (return . (`div` 1000000) . view tcTime) tim
    let ev = TeamEvent typ (toBytes tid) ts dat []
    Aws.execute e (Aws.enqueue ev)

----------------------------------------------------------------------------
-- utils

evData :: [TeamMember] -> Maybe Currency.Alpha -> TeamEvent'EventData
evData mems cur = TeamEvent'EventData count (toBytes <$> uids) (pack . show <$> cur) []
  where
    uids  = view userId <$> filter (`hasPermission` SetBilling) mems
    count = fromIntegral $ length mems
