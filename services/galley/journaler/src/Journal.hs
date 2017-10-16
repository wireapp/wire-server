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
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import System.Logger (Logger)

import qualified System.Logger          as Log
import qualified Galley.Data            as Data
import qualified Galley.Intra.Journal   as Journal
import qualified Galley.Aws             as Aws


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
          Just Active        -> journalTeamCreate tid time
          Just PendingDelete -> journalTeamDelete tid
          Just Deleted       -> journalTeamDelete tid
          Just Suspended     -> journalTeamCreate tid time
          Just PendingActive -> return ()
          Nothing            -> journalTeamCreate tid time

    journalTeamDelete :: TeamId -> C.Client ()
    journalTeamDelete tid = do
        now <- Journal.nowInt
        let event = TeamEvent TeamEvent'TEAM_DELETE (Journal.bytes tid) now Nothing
        Aws.execute env (Aws.enqueue event)

    journalTeamCreate :: TeamId -> Maybe Int64 -> C.Client ()
    journalTeamCreate tid time = do
        now <- Journal.nowInt
        mems <- Data.teamMembers tid
        let creationTimeSeconds = maybe now (`div` 1000000) time  -- writetime is in microseconds in cassandra 2.1
        let bUsers = view userId <$> filter (`hasPermission` SetBilling) mems
        let eData = Journal.evData (fromIntegral $ length mems) bUsers
        let event = TeamEvent TeamEvent'TEAM_CREATE (Journal.bytes tid) creationTimeSeconds (Just eData)
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
