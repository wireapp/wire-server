{-# LANGUAGE RecordWildCards #-}

module Wire.MockInterpreters.UserGroupStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.UserGroup
import Wire.MockInterpreters.Random (runRandomPure)
import Wire.Sem.Random qualified as Random
import Wire.UserGroupStore

runInMemoryUserGroupStore :: InterpreterFor UserGroupStore r
runInMemoryUserGroupStore =
  runRandomPure
    . evalState mempty
    . inMemoryUserGroupStoreInterpreter
    . raiseUnder
    . raiseUnder

inMemoryUserGroupStoreInterpreter :: (Member (State (Map TeamId [UserGroup])) r, Member Random.Random r) => InterpreterFor UserGroupStore r
inMemoryUserGroupStoreInterpreter = interpret $ \case
  CreateUserGroup team NewUserGroup {..} managedBy -> do
    id_ <- Id <$> Random.uuid
    modify (Map.insertWith (<>) team [UserGroup {..}])
    pure id_
  GetUserGroup tid gid ->
    gets $ \teamGroups -> do
      groups <- Map.lookup tid teamGroups
      find (\g -> g.id_ == gid) groups
