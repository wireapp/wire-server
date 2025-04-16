{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.MockInterpreters.UserGroupSubsystem
  ( runInMemoryUserGroupSubsystem,
    userGroupSubsystemTestInterpreter,
  )
where

import Data.Id
import Data.Map qualified as Map
import Data.Time
import Data.UUID
import GHC.Stack
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.State
import System.Random (StdGen, mkStdGen)
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.MockInterpreters.Random
import Wire.Sem.Random qualified as Rnd
import Wire.UserGroupSubsystem

type MockState = Map UserGroupId UserGroup

type EffectStack r =
  ( Member (State MockState) r,
    Member (Input UTCTime) r,
    Member Rnd.Random r,
    HasCallStack
  )

runInMemoryUserGroupSubsystem ::
  UTCTime ->
  Sem
    '[ UserGroupSubsystem,
       State MockState,
       Input UTCTime,
       Rnd.Random,
       State StdGen
     ]
    a ->
  a
runInMemoryUserGroupSubsystem now =
  run
    . evalState (mkStdGen 3)
    . randomToStatefulStdGen
    . runInputConst now
    . evalState (mempty :: MockState)
    . userGroupSubsystemTestInterpreter

userGroupSubsystemTestInterpreter :: (EffectStack r) => InterpreterFor UserGroupSubsystem r
userGroupSubsystemTestInterpreter =
  interpret \case
    CreateGroup ng -> createGroupImpl ng
    GetGroup gid -> getGroupImpl gid
    GetGroups limit lastKey -> getGroupsImpl limit lastKey
    UpdateGroup gid gup -> updateGroupImpl gid gup
    DeleteGroup gid -> deleteGroupImpl gid
    AddUser gid uid -> addUserImpl gid uid
    RemoveUser gid uid -> removeUserImpl gid uid

createGroupImpl :: (EffectStack r) => NewUserGroup -> Sem r UserGroup
createGroupImpl nug = do
  now <- input
  gid <- Id <$> Rnd.uuid
  let ug =
        UserGroup
          { id_ = gid,
            name = nug.name,
            members = nug.members,
            managedBy = ManagedByWire,
            createdAt = now
          }

  modify (Map.insert gid ug)
  pure ug

getGroupImpl :: (EffectStack r) => UserGroupId -> Sem r (Maybe UserGroup)
getGroupImpl gid = Map.lookup gid <$> get

getGroupsImpl :: (EffectStack r) => Maybe Int -> Maybe UUID -> Sem r UserGroupPage
getGroupsImpl (fromMaybe 100 -> limit) mbLastKey = do
  allGroups <- get
  let cutLowerBound = maybe id (\lastKey -> filter ((> Id lastKey) . fst)) mbLastKey
      relevant = map snd . cutLowerBound . Map.toList $ allGroups
      truncated = Imports.take limit $ relevant
  pure $ UserGroupPage truncated (length truncated /= length relevant)

updateGroupImpl :: (EffectStack r) => UserGroupId -> UserGroupUpdate -> Sem r (Maybe UserGroup)
updateGroupImpl gid (UserGroupUpdate newName) = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {name = newName} :: UserGroup)

  modify (Map.alter f gid)
  getGroupImpl gid

deleteGroupImpl :: (EffectStack r) => UserGroupId -> Sem r ()
deleteGroupImpl _ = do
  _ <- input
  _ <- get
  undefined

addUserImpl :: (EffectStack r) => UserGroupId -> UserId -> Sem r ()
addUserImpl _ _ = do
  _ <- input
  _ <- get
  undefined

removeUserImpl :: (EffectStack r) => UserGroupId -> UserId -> Sem r ()
removeUserImpl _ _ = do
  _ <- input
  _ <- get
  undefined
