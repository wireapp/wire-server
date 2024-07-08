{-# LANGUAGE RecordWildCards #-}

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

module Work where

import Cassandra hiding (Set)
import Cassandra qualified as Cas
import Cassandra.Settings qualified as Cas
import Conduit
import Control.Lens
import Control.Monad.Except
import Data.Conduit.Combinators qualified as C
import Data.Handle (Handle)
import Data.Id
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Imports
import Options
import Options.Applicative hiding (action)
import System.Logger qualified as Log
import Types

-- | The table user_handle grouped by user
type HandleMap = Map UserId [Handle]

readHandleMap :: Env -> IO HandleMap
readHandleMap Env {..} =
  runConduit $
    transPipe (runClient envBrig) (paginateC selectUserHandle (paramsP LocalQuorum () envPageSize) x1)
      .| (C.foldM insertAndLog (Map.empty, 0) <&> fst)
  where
    selectUserHandle :: PrepQuery R () (Maybe UserId, Maybe Handle)
    selectUserHandle = "SELECT user, handle FROM user_handle"

    insertAndLog :: (HandleMap, Int) -> [(Maybe UserId, Maybe Handle)] -> IO (HandleMap, Int)
    insertAndLog (hmap, nTotal) pairs = do
      let n = length pairs
      Log.info envLogger $ Log.msg @Text $ "handles loaded: " <> (T.pack . show $ nTotal + n)
      pure (foldl' insert hmap pairs, nTotal + n)

    insert :: HandleMap -> (Maybe UserId, Maybe Handle) -> HandleMap
    insert hmap (Just uid, maybeToList -> handles) =
      Map.insertWith (++) uid handles hmap
    insert hmap (Nothing, _) = hmap

data Action
  = -- | Set the handle for user (column "handle" in table user).
    -- Precondition:
    -- The handle is the sole handle owned by the user (according to table user_handle)
    SetHandle
      UserId
      Handle
  | -- | Change the handle for user (column "handle" in table user)
    -- and delete the other handle.
    -- Precondition:
    -- Both handles are already owned by the user (according to table user_handle)
    -- and the user doesnt own any more handles.
    -- syntax: ResetHandle uid handle handleToBeRemoved
    ResetHandle
      UserId
      Handle
      Handle
  | NoActionRequired UserId
  deriving (Show)

data ActionError = ActionError UserId Text [Handle]
  deriving (Show)

type ActionResult = Either ActionError Action

decideAction ::
  UserId ->
  -- | "handle" column in table "brig.user"
  Maybe Handle ->
  -- | All handles owned by user in table "brig.user_handle"
  [Handle] ->
  ActionResult
decideAction uid Nothing [handle] = pure $ SetHandle uid handle
decideAction uid Nothing [] = pure $ NoActionRequired uid
decideAction uid Nothing handles = throwError $ ActionError uid "No handle set, but multiple handles owned" handles
decideAction uid (Just currentHandle) handles =
  case filter (/= currentHandle) handles of
    [] -> pure $ NoActionRequired uid
    [otherHandle] -> pure $ ResetHandle uid otherHandle currentHandle
    handles' -> throwError $ ActionError uid "Handle is set, but multiple handles owned" handles'

sourceActions :: Env -> HandleMap -> ConduitM () ActionResult IO ()
sourceActions Env {..} hmap =
  transPipe
    (runClient envGalley)
    ( paginateC selectTeam (paramsP LocalQuorum (pure envTeam) envPageSize) x5
        .| C.map (fmap runIdentity)
    )
    .| C.mapM readUsersPage
    .| C.concat
    .| C.map
      ( \(uid, mbHandle) ->
          decideAction uid mbHandle (fromMaybe [] $ Map.lookup uid hmap)
      )
  where
    selectTeam :: PrepQuery R (Identity TeamId) (Identity UserId)
    selectTeam = "SELECT user FROM team_member WHERE team = ?"

    readUsersPage :: [UserId] -> IO [(UserId, Maybe Handle)]
    readUsersPage uids =
      runClient envBrig $
        query selectUsers (params LocalQuorum (pure uids))

    selectUsers :: PrepQuery R (Identity [UserId]) (UserId, Maybe Handle)
    selectUsers = "SELECT id, handle FROM user WHERE id in ?"

executeAction :: Env -> Action -> IO ()
executeAction env = \case
  (NoActionRequired _) -> pure ()
  (SetHandle uid handle) ->
    setUserHandle env uid handle
  (ResetHandle uid handle handleRemove) -> do
    setUserHandle env uid handle
    removeHandle env handleRemove
  where
    setUserHandle :: Env -> UserId -> Handle -> IO ()
    setUserHandle Env {..} uid handle =
      runClient envBrig $
        Cas.write updateHandle $
          params LocalQuorum (handle, uid)
      where
        updateHandle :: PrepQuery W (Handle, UserId) ()
        updateHandle = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET handle = ? WHERE id = ?"

    removeHandle :: Env -> Handle -> IO ()
    removeHandle Env {..} handle =
      runClient envBrig $
        Cas.write deleteHandle $
          params LocalQuorum (pure handle)
      where
        deleteHandle :: PrepQuery W (Identity Handle) ()
        deleteHandle = "DELETE FROM user_handle WHERE handle = ?"

runCommand :: Env -> IO ()
runCommand env@Env {..} = do
  Log.info envLogger (Log.msg @Text "Loading all handles...")
  hmap <- readHandleMap env
  Log.info envLogger $ Log.msg @Text ("Processing team" <> (if envSettings ^. setDryRun then " (dry-run) " else " ") <> "...")
  runConduit $
    sourceActions env hmap
      .| C.iterM (handleAction env)
      .| chunkify 100
      .| C.mapM_ (logSummary env)
  Log.info envLogger $ Log.msg @Text "Done."
  where
    handleAction :: Env -> ActionResult -> IO ()
    handleAction _env (Left err) = Log.err envLogger (Log.msg . show $ err)
    handleAction _env (Right action) = do
      Log.debug envLogger $ Log.msg (show $ action)
      unless (envSettings ^. setDryRun) $
        executeAction env action

    logSummary :: Env -> [ActionResult] -> IO ()
    logSummary _env results = do
      Log.info envLogger $
        Log.msg $
          ("Action summary for batch" <> (if envSettings ^. setDryRun then " (dry-run) " else " ") <> ":\n")
            <> T.intercalate
              "\n"
              ( let (nErrs, nReset, nSet, nNoOp) = foldl' tally (0, 0, 0, 0) results
                 in catMaybes
                      [ mark "   error(!)" nErrs,
                        mark "   reset original handle" nReset,
                        mark "   set missing handle" nSet,
                        mark "   do nothing" nNoOp
                      ]
              )
      where
        mark :: Text -> Int -> Maybe Text
        mark msg n
          | n > 0 = Just $ msg <> ": " <> T.pack (show n)
          | otherwise = Nothing

        tally :: (Int, Int, Int, Int) -> ActionResult -> (Int, Int, Int, Int)
        tally (nErrs, nReset, nSet, nNoOp) (Right ResetHandle {}) = (nErrs, nReset + 1, nSet, nNoOp)
        tally (nErrs, nReset, nSet, nNoOp) (Right SetHandle {}) = (nErrs, nReset, nSet + 1, nNoOp)
        tally (nErrs, nReset, nSet, nNoOp) (Right NoActionRequired {}) = (nErrs, nReset, nSet, nNoOp + 1)
        tally (nErrs, nReset, nSet, nNoOp) (Left _) = (nErrs + 1, nReset, nSet, nNoOp)

    chunkify :: (Monad m) => Int -> ConduitT i [i] m ()
    chunkify n = void (C.map (: [])) .| C.chunksOfE n

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  lgr <- initLogger (if s ^. setDebug then Log.Debug else Log.Info)

  brig <- initCas (s ^. setCasBrig) lgr
  galley <- initCas (s ^. setCasGalley) lgr

  let env =
        Env
          { envBrig = brig,
            envGalley = galley,
            envPageSize = s ^. setPageSize,
            envTeam = s ^. setTeamId,
            envSettings = s,
            envLogger = lgr
          }
  runCommand env
  where
    desc =
      header "repair-handles"
        <> progDesc "Fix dangling and missing handles"
        <> fullDesc
    initLogger level =
      Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.setLogLevel level Log.defSettings
    initCas cas l =
      Cas.init
        . Cas.setLogger (Cas.mkLogger l)
        . Cas.setContacts (cas ^. cHosts) []
        . Cas.setPortNumber (fromIntegral $ cas ^. cPort)
        . Cas.setKeyspace (cas ^. cKeyspace)
        . Cas.setProtocolVersion Cas.V4
        $ Cas.defSettings
