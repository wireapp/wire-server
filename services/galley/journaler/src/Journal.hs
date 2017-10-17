{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Journal where

import Cassandra as C
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Lens
import Control.Monad.Except
import Data.Id
import Data.ByteString.Conversion
import Data.Monoid ((<>))
import Data.Int
import Data.Maybe (fromMaybe)
import Proto.TeamEvents
import Galley.Types.Teams.Intra
import System.Logger (Logger)

import qualified System.Logger        as Log
import qualified Galley.Data          as Data
import qualified Galley.Intra.Journal as Journal
import qualified Galley.Aws           as Aws


runCommand :: Logger -> Aws.Env -> ClientState -> Maybe TeamId -> IO ()
runCommand l env c start = void $ C.runClient c $ do
    page <- case start of
        Just st  -> retry x5 $ paginate teamSelectFrom (paramsP Quorum (Identity st) 100)
        Nothing -> retry x5 $ paginate teamSelect' (paramsP Quorum () 100)
    scan 0 page

  where
    scan :: Int -> Page Row -> C.Client ()
    scan acc page = do
        let boundTeams = filterBinding (result page)
        void $ mapConcurrently journal boundTeams
        let count = acc + Prelude.length boundTeams
        Log.info l . Log.msg $ Log.val ("Processed " <> toByteString' count <> " bound teams so far")
        when (hasMore page) $
            retry x5 (liftClient (nextPage page)) >>= scan count

    journal :: Row -> C.Client ()
    journal (tid, _, s, time) = C.runClient c $ case s of
          Just Active        -> journalTeamActivate tid time
          Just PendingDelete -> journalTeamDelete tid
          Just Deleted       -> journalTeamDelete tid
          Just Suspended     -> journalTeamSuspend tid
          Just PendingActive -> return ()
          -- Nothing addresses teams that are "pre status"
          Nothing            -> journalTeamActivate tid time

    journalTeamDelete :: TeamId -> C.Client ()
    journalTeamDelete tid = publish tid TeamEvent'TEAM_DELETE Nothing Nothing

    journalTeamSuspend :: TeamId -> C.Client ()
    journalTeamSuspend tid = publish tid TeamEvent'TEAM_SUSPEND Nothing Nothing

    journalTeamActivate :: TeamId -> Maybe Int64 -> C.Client ()
    journalTeamActivate tid time = do
        mems <- Data.teamMembers tid
        let dat = Journal.evData mems
        publish tid TeamEvent'TEAM_ACTIVATE time (Just dat)

    publish :: TeamId -> TeamEvent'EventType -> Maybe Int64 -> Maybe TeamEvent'EventData -> C.Client ()
    publish tid typ time dat = do
        now <- Journal.nowInt
        -- writetime is in microseconds in cassandra 2.1 and 3.7
        let creationTimeSeconds = maybe now (`div` 1000000) time
        let event = TeamEvent typ (Journal.bytes tid) creationTimeSeconds dat
        Aws.execute env (Aws.enqueue event)

-- CQL queries
teamSelect' :: PrepQuery R () Row
teamSelect' = "SELECT team, binding, status, writetime(binding) FROM team"

teamSelectFrom :: PrepQuery R (Identity TeamId) Row
teamSelectFrom = "SELECT team, binding, status, writetime(binding) FROM team WHERE token(team) > token(?)"

-- Utils

type Row = (TeamId, Maybe Bool, Maybe TeamStatus, Maybe Int64)

filterBinding :: [Row] -> [Row]
filterBinding = filter (\(_, b, _, _) -> fromMaybe False b)
