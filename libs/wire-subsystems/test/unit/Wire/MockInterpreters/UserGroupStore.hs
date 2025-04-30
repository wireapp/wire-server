{-# LANGUAGE RecordWildCards #-}

module Wire.MockInterpreters.UserGroupStore where

import Data.Id
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
    . evalState []
    . inMemoryUserGroupStoreInterpreter
    . raiseUnder
    . raiseUnder

inMemoryUserGroupStoreInterpreter :: (Member (State [UserGroup]) r, Member Random.Random r) => InterpreterFor UserGroupStore r
inMemoryUserGroupStoreInterpreter = interpret $ \case
  CreateUserGroup team NewUserGroup {..} managedBy -> do
    id_ <- Id <$> Random.uuid
    modify (UserGroup {..} :)
    pure id_
  GetUserGroup tid gid ->
    gets $ find \g -> g.id_ == gid && g.team == tid
