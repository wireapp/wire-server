{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- FUTUREWORK: move this next to Postgres interpreter; write integration tests that run random,
-- valid command sequences against both and compare.

module Wire.MockInterpreters.UserGroupStore where

import Data.Default
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Time
import GHC.Stack
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.Internal (Append)
import Polysemy.State
import System.Random (StdGen, mkStdGen)
import Wire.API.User
import Wire.API.UserGroup
import Wire.MockInterpreters.Random
import Wire.Sem.Now qualified as Now
import Wire.Sem.Now.Input qualified as Now
import Wire.Sem.Random qualified as Rnd
import Wire.UserGroupStore

data UserGroupInMemState = UserGroupInMemState
  { userGroups :: Map (TeamId, UserGroupId) UserGroup
  }
  deriving (Eq, Show)

instance Default UserGroupInMemState where
  def = UserGroupInMemState mempty

type EffectConstraints r =
  ( Member Now.Now r,
    Member (State UserGroupInMemState) r,
    Member Rnd.Random r,
    HasCallStack
  )

type EffectStack =
  '[ UserGroupStore,
     Now.Now,
     Input UTCTime,
     State UserGroupInMemState,
     Rnd.Random,
     State StdGen
   ]

runInMemoryUserGroupStore :: UserGroupInMemState -> Sem (EffectStack `Append` r) a -> Sem r a
runInMemoryUserGroupStore state =
  evalState (mkStdGen 3)
    . randomToStatefulStdGen
    . evalState state
    . runInputConst (fromJust (parseTimeM True defaultTimeLocale "%FT%T%QZ" "2021-05-12T10:52:02Z"))
    . Now.nowToInput
    . userGroupStoreTestInterpreter

userGroupStoreTestInterpreter :: (EffectConstraints r) => InterpreterFor UserGroupStore r
userGroupStoreTestInterpreter =
  interpret \case
    CreateUserGroup tid ng mb -> createUserGroupImpl tid ng mb
    GetUserGroup tid gid -> getUserGroupImpl tid gid
    AddUserGroupsToChannel _ _ -> error "AddUserGroupsToChannel not implemented in mock interpreter"

createUserGroupImpl :: (EffectConstraints r) => TeamId -> NewUserGroup -> ManagedBy -> Sem r UserGroup
createUserGroupImpl tid nug managedBy = do
  now <- Now.get
  gid <- Id <$> Rnd.uuid
  let ug =
        UserGroup
          { id_ = gid,
            name = nug.name,
            members = nug.members,
            managedBy = managedBy,
            createdAt = toUTCTimeMillis now
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
