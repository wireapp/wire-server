{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- TODO: move this next to Postgres interpreter; write integration tests that run random,
-- valid command sequences against both and compare.

module Wire.MockInterpreters.UserGroupStore where

import Data.Default
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import GHC.Stack
import Imports
import Polysemy
import Polysemy.Internal (Append)
import Polysemy.State
import System.Random (StdGen, mkStdGen)
import Wire.API.User
import Wire.API.UserGroup
import Wire.MockInterpreters.Random
import Wire.Sem.Random qualified as Rnd
import Wire.UserGroupStore

-- TODO: this is not a mock state, but maybe an InMemState
data MockState = MockState
  { userGroups :: Map (TeamId, UserGroupId) UserGroup,
    now :: UTCTimeMillis
  }
  deriving (Eq, Show)

instance Default MockState where
  def = MockState mempty (fromJust (readUTCTimeMillis "2021-05-12T10:52:02Z"))

type EffectConstraints r =
  ( Member (State MockState) r,
    Member Rnd.Random r,
    HasCallStack
  )

type EffectStack =
  '[ UserGroupStore,
     State MockState,
     Rnd.Random,
     State StdGen
   ]

runInMemoryUserGroupStore :: MockState -> Sem (EffectStack `Append` r) a -> Sem r a
runInMemoryUserGroupStore mockState =
  evalState (mkStdGen 3)
    . randomToStatefulStdGen
    . evalState mockState
    . userGroupStoreTestInterpreter

userGroupStoreTestInterpreter :: (EffectConstraints r) => InterpreterFor UserGroupStore r
userGroupStoreTestInterpreter =
  interpret \case
    CreateUserGroup tid ng mb -> createUserGroupImpl tid ng mb
    GetUserGroup tid gid -> getUserGroupImpl tid gid

createUserGroupImpl :: (EffectConstraints r) => TeamId -> NewUserGroup -> ManagedBy -> Sem r UserGroup
createUserGroupImpl tid nug managedBy = do
  now <- (.now) <$> get
  gid <- Id <$> Rnd.uuid
  let ug =
        UserGroup
          { id_ = gid,
            name = nug.name,
            members = nug.members,
            managedBy = managedBy,
            createdAt = now
          }

  modifyUserGroups (Map.insert (tid, gid) ug)
  pure ug

getUserGroupImpl :: (EffectConstraints r) => TeamId -> UserGroupId -> Sem r (Maybe UserGroup)
getUserGroupImpl tid gid = (Map.lookup (tid, gid) . (.userGroups)) <$> get

modifyUserGroups ::
  forall r m.
  (EffectConstraints r, m ~ Map (TeamId, UserGroupId) UserGroup) =>
  (m -> m) ->
  Sem r ()
modifyUserGroups u = modify (\ms -> ms {userGroups = u ms.userGroups})
