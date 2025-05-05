{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- TODO: move this next to Postgres interpreter; write integration tests that run random,
-- valid command sequences against both and compare.

module Wire.MockInterpreters.UserGroupStore where

import Data.Default
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Vector (fromList)
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
    GetUserGroups tid limit lastKey -> getGroupsImpl tid limit lastKey
    UpdateUserGroup tid gid gup -> updateUserGroupImpl tid gid gup
    DeleteUserGroup tid gid -> deleteUserGroupImpl tid gid
    AddUser tid gid uid -> addUserImpl tid gid uid
    RemoveUser tid gid uid -> removeUserImpl tid gid uid

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

getGroupsImpl :: (EffectConstraints r) => TeamId -> Maybe Int -> Maybe UserGroupId -> Sem r UserGroupPage
getGroupsImpl tid (fromMaybe 100 -> limit) mbLastKey = do
  allGroups :: [(UserGroupId, UserGroup)] <- do
    let f :: ((TeamId, a), b) -> [(a, b)] -> [(a, b)]
        f ((tidx, gid), grp) acc = if tidx == tid then (gid, grp) : acc else acc
    (foldr f [] . Map.toList . (.userGroups)) <$> get

  let cutLowerBound :: forall a. (a ~ [(UserGroupId, UserGroup)]) => a -> a
      cutLowerBound = maybe id (\lastKey -> filter ((> lastKey) . fst)) mbLastKey

      relevant :: [UserGroup]
      relevant = map snd . cutLowerBound $ allGroups

      truncated :: [UserGroup]
      truncated = Imports.take limit $ relevant

  pure $ UserGroupPage truncated (length truncated /= length relevant)

updateUserGroupImpl :: (EffectConstraints r) => TeamId -> UserGroupId -> UserGroupUpdate -> Sem r (Maybe UserGroup)
updateUserGroupImpl tid gid (UserGroupUpdate newName) = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {name = newName} :: UserGroup)

  modifyUserGroups (Map.alter f (tid, gid))
  getUserGroupImpl tid gid

deleteUserGroupImpl :: (EffectConstraints r) => TeamId -> UserGroupId -> Sem r ()
deleteUserGroupImpl tid gid = do
  modifyUserGroups (Map.delete (tid, gid))

addUserImpl :: (EffectConstraints r) => TeamId -> UserGroupId -> UserId -> Sem r ()
addUserImpl tid gid uid = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = fromList . nub $ uid : toList g.members} :: UserGroup)

  modifyUserGroups (Map.alter f (tid, gid))

removeUserImpl :: (EffectConstraints r) => TeamId -> UserGroupId -> UserId -> Sem r ()
removeUserImpl tid gid uid = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = fromList $ toList g.members \\ [uid]} :: UserGroup)

  modifyUserGroups (Map.alter f (tid, gid))

modifyUserGroups ::
  forall r m.
  (EffectConstraints r, m ~ Map (TeamId, UserGroupId) UserGroup) =>
  (m -> m) ->
  Sem r ()
modifyUserGroups u = modify (\ms -> ms {userGroups = u ms.userGroups})
