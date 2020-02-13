module Journal where

import Cassandra as C hiding (Row)
import Control.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Proto
import Data.Proto.Id as Proto
import Data.ProtoLens (defMessage)
import qualified Galley.Aws as Aws
import qualified Galley.Data as Data
import qualified Galley.Intra.Journal as Journal
import Galley.Types.Teams (TeamCreationTime (..), tcTime)
import Galley.Types.Teams.Intra
import Imports
import Proto.TeamEvents (TeamEvent'EventData, TeamEvent'EventType (..))
import qualified Proto.TeamEvents_Fields as T
import qualified System.Logger as Log
import System.Logger.Class (Logger)
import UnliftIO (mapConcurrently)

runCommand :: Logger -> Aws.Env -> ClientState -> Maybe TeamId -> IO ()
runCommand l env c start = void $ C.runClient c $ do
  page <- case start of
    Just st -> retry x5 $ paginate teamSelectFrom (paramsP Quorum (Identity st) 100)
    Nothing -> retry x5 $ paginate teamSelect' (paramsP Quorum () 100)
  scan 0 page
  where
    scan :: Int -> Page Row -> C.Client ()
    scan acc page = do
      let boundTeams = filterBinding (result page)
      void $ mapConcurrently journal boundTeams
      let count = acc + length boundTeams
      Log.info l . Log.msg $ Log.val ("Processed " <> toByteString' count <> " bound teams so far")
      when (hasMore page) $
        retry x5 (liftClient (nextPage page)) >>= scan count
    journal :: (TeamId, Maybe TeamStatus, Maybe TeamCreationTime) -> C.Client ()
    journal (tid, s, time) = C.runClient c $ case s of
      Just Active -> journalTeamActivate tid time
      Just PendingDelete -> journalTeamDelete tid
      Just Deleted -> journalTeamDelete tid
      Just Suspended -> journalTeamSuspend tid
      Just PendingActive -> return ()
      -- Nothing addresses teams that are "pre status"
      Nothing -> journalTeamActivate tid time
    journalTeamDelete :: TeamId -> C.Client ()
    journalTeamDelete tid = publish tid TeamEvent'TEAM_DELETE Nothing Nothing
    journalTeamSuspend :: TeamId -> C.Client ()
    journalTeamSuspend tid = publish tid TeamEvent'TEAM_SUSPEND Nothing Nothing
    journalTeamActivate :: TeamId -> Maybe TeamCreationTime -> C.Client ()
    journalTeamActivate tid time = do
      mems <- Data.teamMembers tid
      let dat = Journal.evData mems Nothing
      publish tid TeamEvent'TEAM_ACTIVATE time (Just dat)
    publish :: TeamId -> TeamEvent'EventType -> Maybe TeamCreationTime -> Maybe TeamEvent'EventData -> C.Client ()
    publish tid typ time dat = do
      -- writetime is in microseconds in cassandra 3.11
      creationTimeSeconds <- maybe now (return . (`div` 1000000) . view tcTime) time
      let event =
            defMessage
              & T.eventType .~ typ
              & T.teamId .~ (Proto.toBytes tid)
              & T.utcTime .~ creationTimeSeconds
              & T.maybe'eventData .~ dat
      Aws.execute env (Aws.enqueue event)

-- CQL queries
teamSelect' :: PrepQuery R () Row
teamSelect' = "SELECT team, binding, status, writetime(binding) FROM team"

teamSelectFrom :: PrepQuery R (Identity TeamId) Row
teamSelectFrom = "SELECT team, binding, status, writetime(binding) FROM team WHERE token(team) > token(?)"

-- Utils

type Row = (TeamId, Maybe Bool, Maybe TeamStatus, Maybe Int64)

filterBinding :: [Row] -> [(TeamId, Maybe TeamStatus, Maybe TeamCreationTime)]
filterBinding = map (\(t, _, st, tim) -> (t, st, TeamCreationTime <$> tim)) . filter (\(_, b, _, _) -> fromMaybe False b)
