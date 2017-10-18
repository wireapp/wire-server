{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Intra.Journal
    ( teamActivate
    , teamUpdate
    , teamDelete
    , teamSuspend
    , bytes
    , evData
    , nowInt
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Int
import Data.Foldable (for_)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Id
import Galley.Types.Teams
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX
import Galley.App
import Prelude hiding (head, mapM)
import Proto.TeamEvents

import qualified Data.UUID as UUID
import qualified Galley.Aws as Aws

-- [Note: journaling]
-- Team journal operations to SQS are a no-op when the service
-- is started without journaling arguments

teamActivate :: TeamId -> [TeamMember] -> Maybe TeamCreationTime -> Galley ()
teamActivate tid mems time = journalEvent TeamEvent'TEAM_ACTIVATE tid (Just $ evData mems) time

teamUpdate :: TeamId -> [TeamMember] -> Galley ()
teamUpdate tid mems = journalEvent TeamEvent'TEAM_UPDATE tid (Just $ evData mems) Nothing

teamDelete :: TeamId -> Galley ()
teamDelete tid = journalEvent TeamEvent'TEAM_DELETE tid Nothing Nothing

teamSuspend :: TeamId -> Galley ()
teamSuspend tid = journalEvent TeamEvent'TEAM_SUSPEND tid Nothing Nothing

journalEvent :: TeamEvent'EventType -> TeamId -> Maybe TeamEvent'EventData -> Maybe TeamCreationTime -> Galley ()
journalEvent typ tid dat tim = view aEnv >>= \mEnv -> for_ mEnv $ \e -> do
    -- writetime is in microseconds in cassandra 3.11
    ts <- maybe nowInt (return . (`div` 1000000) . view tcTime) tim
    let ev = TeamEvent typ (bytes tid) ts dat
    Aws.execute e (Aws.enqueue ev)

----------------------------------------------------------------------------
-- utils

bytes :: Id a -> ByteString
bytes = toStrict . UUID.toByteString . toUUID

evData :: [TeamMember] -> TeamEvent'EventData
evData mems = TeamEvent'EventData count (bytes <$> uids)
  where
    uids  = view userId <$> filter (`hasPermission` SetBilling) mems
    count = fromIntegral $ length mems

nowInt :: MonadIO m => m Int64
nowInt = liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
