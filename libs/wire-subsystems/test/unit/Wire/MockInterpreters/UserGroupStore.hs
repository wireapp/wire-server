{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- FUTUREWORK: move this next to Postgres interpreter; write integration tests that run random,
-- valid command sequences against both and compare.

module Wire.MockInterpreters.UserGroupStore where

import Data.Default
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Range
import Data.Text qualified as T
import Data.Time.Clock
import Data.Vector (fromList)
import GHC.Stack
import Imports
import Polysemy
import Polysemy.Internal (Append)
import Polysemy.State
import System.Random (StdGen, mkStdGen)
import Wire.API.User
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.MockInterpreters.Random
import Wire.Sem.Random qualified as Rnd
import Wire.UserGroupStore

data UserGroupInMemState = UserGroupInMemState
  { userGroups :: Map (TeamId, UserGroupId) UserGroup,
    now :: UTCTimeMillis -- (we could use `Now` from polysemy-wire-zoo, but that doesn't allow moving the clock deliberately.)
  }
  deriving (Eq, Show)

moveClock :: (Member (State UserGroupInMemState) r) => NominalDiffTime -> Sem r ()
moveClock diff = modify (\s -> s {now = toUTCTimeMillis (addUTCTime diff (fromUTCTimeMillis s.now))})

instance Default UserGroupInMemState where
  def = UserGroupInMemState mempty (fromJust (readUTCTimeMillis "2021-05-12T10:52:02Z"))

type EffectConstraints r =
  ( Member (State UserGroupInMemState) r,
    Member Rnd.Random r,
    HasCallStack
  )

type EffectStack =
  '[ UserGroupStore,
     State UserGroupInMemState,
     Rnd.Random,
     State StdGen
   ]

runInMemoryUserGroupStore :: UserGroupInMemState -> Sem (EffectStack `Append` r) a -> Sem r a
runInMemoryUserGroupStore state =
  evalState (mkStdGen 3)
    . randomToStatefulStdGen
    . evalState state
    . userGroupStoreTestInterpreter

userGroupStoreTestInterpreter :: (EffectConstraints r) => InterpreterFor UserGroupStore r
userGroupStoreTestInterpreter =
  interpret \case
    CreateUserGroup tid ng mb -> createUserGroupImpl tid ng mb
    GetUserGroup tid gid -> getUserGroupImpl tid gid
    GetUserGroups tid listUserGroupsQuery -> getUserGroupsImpl tid listUserGroupsQuery
    UpdateUserGroup tid gid gup -> updateUserGroupImpl tid gid gup
    DeleteUserGroup tid gid -> deleteUserGroupImpl tid gid
    AddUser gid uid -> addUserImpl gid uid
    RemoveUser gid uid -> removeUserImpl gid uid

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

getUserGroupsImpl :: (EffectConstraints r) => TeamId -> PaginationState -> Sem r [UserGroup]
getUserGroupsImpl tid pstate = do
  ((snd <$>) . sieve . Map.toList . (.userGroups)) <$> get
  where
    sieve,
      dropAfterPageSize,
      dropBeforeStart,
      orderByKeys,
      narrowToSearchString,
      narrowToTeam ::
        [((TeamId, UserGroupId), UserGroup)] -> [((TeamId, UserGroupId), UserGroup)]

    sieve =
      dropAfterPageSize
        . dropBeforeStart
        . orderByKeys
        . narrowToSearchString
        . narrowToTeam

    narrowToTeam = filter (\((thisTid, _), _) -> thisTid == tid)

    narrowToSearchString =
      filter (\(_, ug) -> maybe True (`T.isInfixOf` userGroupNameToText ug.name) pstate.searchString)

    orderByKeys = Imports.sortBy c
      where
        c (_, ug) (_, ug') =
          compare
            (mostSignificantSmaller, leastSignificantSmaller)
            (mostSignificantGreater, leastSignificantGreater)
          where
            showName = fromRange . unUserGroupName

            mostSignificantSmaller = case (pstate.sortBy, pstate.sortOrderName, pstate.sortOrderCreatedAt) of
              (SortByName, Asc, _) -> showName ug.name
              (SortByName, Desc, _) -> showName ug'.name
              (SortByCreatedAt, _, Asc) -> showUTCTimeMillis ug.createdAt
              (SortByCreatedAt, _, Desc) -> showUTCTimeMillis ug'.createdAt

            leastSignificantSmaller = case (pstate.sortBy, pstate.sortOrderName, pstate.sortOrderCreatedAt) of
              (SortByName, _, Asc) -> showUTCTimeMillis ug.createdAt
              (SortByName, _, Desc) -> showUTCTimeMillis ug'.createdAt
              (SortByCreatedAt, Asc, _) -> showName ug.name
              (SortByCreatedAt, Desc, _) -> showName ug'.name

            mostSignificantGreater = case (pstate.sortBy, pstate.sortOrderName, pstate.sortOrderCreatedAt) of
              (SortByName, Asc, _) -> showName ug'.name
              (SortByName, Desc, _) -> showName ug.name
              (SortByCreatedAt, _, Asc) -> showUTCTimeMillis ug'.createdAt
              (SortByCreatedAt, _, Desc) -> showUTCTimeMillis ug.createdAt

            leastSignificantGreater = case (pstate.sortBy, pstate.sortOrderName, pstate.sortOrderCreatedAt) of
              (SortByName, _, Asc) -> showUTCTimeMillis ug'.createdAt
              (SortByName, _, Desc) -> showUTCTimeMillis ug.createdAt
              (SortByCreatedAt, Asc, _) -> showName ug'.name
              (SortByCreatedAt, Desc, _) -> showName ug.name

    dropBeforeStart = case pstate.offset of
      Nothing -> const []
      (Just off) -> drop (fromIntegral off)

    dropAfterPageSize = take (pageSizeToInt pstate.pageSize)

updateUserGroupImpl :: (EffectConstraints r) => TeamId -> UserGroupId -> UserGroupUpdate -> Sem r (Maybe ())
updateUserGroupImpl tid gid (UserGroupUpdate newName) = do
  exists <- getUserGroupImpl tid gid
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {name = newName} :: UserGroup)

  modifyUserGroups (Map.alter f (tid, gid))
  pure $ exists $> ()

deleteUserGroupImpl :: (EffectConstraints r) => TeamId -> UserGroupId -> Sem r (Maybe ())
deleteUserGroupImpl tid gid = do
  exists <- getUserGroupImpl tid gid
  modifyUserGroups (Map.delete (tid, gid))
  pure $ exists $> ()

addUserImpl :: (EffectConstraints r) => UserGroupId -> UserId -> Sem r ()
addUserImpl gid uid = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = fromList . nub $ uid : toList g.members} :: UserGroup)

  modifyUserGroupsGidOnly gid (Map.alter f)

removeUserImpl :: (EffectConstraints r) => UserGroupId -> UserId -> Sem r ()
removeUserImpl gid uid = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = fromList $ toList g.members \\ [uid]} :: UserGroup)

  modifyUserGroupsGidOnly gid (Map.alter f)

----------------------------------------------------------------------

modifyUserGroups ::
  forall r m.
  (EffectConstraints r, m ~ Map (TeamId, UserGroupId) UserGroup) =>
  (m -> m) ->
  Sem r ()
modifyUserGroups u = modify (\ms -> ms {userGroups = u ms.userGroups})

modifyUserGroupsGidOnly ::
  forall r m.
  (EffectConstraints r, m ~ Map (TeamId, UserGroupId) UserGroup) =>
  UserGroupId ->
  ((TeamId, UserGroupId) -> m -> m) ->
  Sem r ()
modifyUserGroupsGidOnly gid u = do
  modify $ \ms -> case filter (\(_, gid') -> gid' == gid) (Map.keys ms.userGroups) of
    [] -> ms
    [fullKey] -> ms {userGroups = u fullKey ms.userGroups}
    bad -> error $ "uuid clash: " <> show bad
