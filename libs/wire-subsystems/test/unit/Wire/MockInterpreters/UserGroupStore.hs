{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- FUTUREWORK: move this next to Postgres interpreter; write integration tests that run random,
-- valid command sequences against both and compare.

module Wire.MockInterpreters.UserGroupStore where

import Data.Default
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Qualified
import Data.Text qualified as T
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector (fromList)
import GHC.Stack
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal (Append)
import Polysemy.State
import System.Random (StdGen, mkStdGen)
import Wire.API.User
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.GalleyAPIAccess
import Wire.MockInterpreters.Random
import Wire.NotificationSubsystem
import Wire.Sem.Random qualified as Rnd
import Wire.UserGroupStore
import Wire.UserGroupSubsystem.Interpreter (UserGroupSubsystemError)
import Wire.UserSubsystem

data UserGroupInMemState = UserGroupInMemState
  { userGroups :: Map (TeamId, UserGroupId) UserGroup,
    -- | current time.  we could use `Now` from polysemy-wire-zoo, but that doesn't allow
    -- moving the clock deliberately.
    now :: UTCTimeMillis,
    -- | time passing before every action.  default is 0, so you can control time by setClock,
    -- moveClock (see below).
    clockStep :: NominalDiffTime
  }
  deriving (Eq, Show)

instance Default UserGroupInMemState where
  def = UserGroupInMemState mempty (toUTCTimeMillis $ posixSecondsToUTCTime 0) 0

setClock :: (Member (State UserGroupInMemState) r) => UTCTime -> Sem r ()
setClock time = modify (\s -> s {now = toUTCTimeMillis time})

moveClock :: (Member (State UserGroupInMemState) r) => NominalDiffTime -> Sem r ()
moveClock diff = modify (\s -> s {now = toUTCTimeMillis (addUTCTime diff (fromUTCTimeMillis s.now))})

moveClockOneStep :: (Member (State UserGroupInMemState) r) => Sem r ()
moveClockOneStep = do
  st <- get
  moveClock st.clockStep

type UserGroupStoreInMemEffectConstraints r =
  ( Member (State UserGroupInMemState) r,
    Member Rnd.Random r,
    HasCallStack
  )

type UserGroupStoreInMemEffectStack =
  '[ UserGroupStore,
     State UserGroupInMemState,
     Rnd.Random,
     State StdGen
   ]

type UserGroupStoreInMemEffectStackTest =
  '[ UserSubsystem,
     GalleyAPIAccess
   ]
    `Append` UserGroupStoreInMemEffectStack
    `Append` '[ Input (Local ()),
                NotificationSubsystem,
                State [Push],
                Error UserGroupSubsystemError
              ]

runInMemoryUserGroupStore :: UserGroupInMemState -> Sem (UserGroupStoreInMemEffectStack `Append` r) a -> Sem r a
runInMemoryUserGroupStore state =
  evalState (mkStdGen 3)
    . randomToStatefulStdGen
    . evalState state
    . userGroupStoreTestInterpreter

userGroupStoreTestInterpreter :: (UserGroupStoreInMemEffectConstraints r) => InterpreterFor UserGroupStore r
userGroupStoreTestInterpreter =
  interpret $
    (moveClockOneStep >>) . \case
      CreateUserGroup tid ng mb -> createUserGroupImpl tid ng mb
      GetUserGroup tid gid -> getUserGroupImpl tid gid
      GetUserGroups tid listUserGroupsQuery -> getUserGroupsImpl tid listUserGroupsQuery
      UpdateUserGroup tid gid gup -> updateUserGroupImpl tid gid gup
      DeleteUserGroup tid gid -> deleteUserGroupImpl tid gid
      AddUser gid uid -> addUserImpl gid uid
      RemoveUser gid uid -> removeUserImpl gid uid

createUserGroupImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> NewUserGroup -> ManagedBy -> Sem r UserGroup
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

getUserGroupImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> UserGroupId -> Sem r (Maybe UserGroup)
getUserGroupImpl tid gid = (Map.lookup (tid, gid) . (.userGroups)) <$> get

getUserGroupsImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> PaginationState -> Sem r [UserGroup]
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

    orderByKeys = Imports.sortBy cmp
      where
        cmp (_, ug) (_, ug') = case (pstate.sortBy, pstate.sortOrder) of
          (SortByName, Asc) -> (n, i) `compare` (n', i')
          (SortByName, Desc) -> (n', i') `compare` (n, i)
          (SortByCreatedAt, Asc) -> (c, i) `compare` (c', i')
          (SortByCreatedAt, Desc) -> (c', i') `compare` (c, i)
          where
            n = ug.name
            n' = ug'.name
            i = ug.id_
            i' = ug'.id_
            c = ug.createdAt
            c' = ug'.createdAt

    dropBeforeStart = do
      dropWhile sqlConds
      where
        sqlConds :: ((TeamId, UserGroupId), UserGroup) -> Bool
        sqlConds ((_, _), row) =
          case (pstate.lastSeen, pstate.sortOrder, pstate.sortBy) of
            (Just (LastSeen (Just name) _ tieBreaker), Asc, SortByName) -> (name, tieBreaker) >= (row.name, row.id_)
            (Just (LastSeen (Just name) _ tieBreaker), Desc, SortByName) -> (name, tieBreaker) <= (row.name, row.id_)
            (Just (LastSeen _ (Just ts) tieBreaker), Asc, SortByCreatedAt) -> (ts, tieBreaker) >= (row.createdAt, row.id_)
            (Just (LastSeen _ (Just ts) tieBreaker), Desc, SortByCreatedAt) -> (ts, tieBreaker) <= (row.createdAt, row.id_)
            (Nothing, _, _) -> False
            _ -> error $ "unexpected lastSeen: " <> show (pstate.lastSeen, pstate.sortOrder, pstate.sortBy)

    dropAfterPageSize = take (pageSizeToInt pstate.pageSize)

updateUserGroupImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> UserGroupId -> UserGroupUpdate -> Sem r (Maybe ())
updateUserGroupImpl tid gid (UserGroupUpdate newName) = do
  exists <- getUserGroupImpl tid gid
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {name = newName} :: UserGroup)

  modifyUserGroups (Map.alter f (tid, gid))
  pure $ exists $> ()

deleteUserGroupImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> UserGroupId -> Sem r (Maybe ())
deleteUserGroupImpl tid gid = do
  exists <- getUserGroupImpl tid gid
  modifyUserGroups (Map.delete (tid, gid))
  pure $ exists $> ()

addUserImpl :: (UserGroupStoreInMemEffectConstraints r) => UserGroupId -> UserId -> Sem r ()
addUserImpl gid uid = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = fromList . nub $ uid : toList g.members} :: UserGroup)

  modifyUserGroupsGidOnly gid (Map.alter f)

removeUserImpl :: (UserGroupStoreInMemEffectConstraints r) => UserGroupId -> UserId -> Sem r ()
removeUserImpl gid uid = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = fromList $ toList g.members \\ [uid]} :: UserGroup)

  modifyUserGroupsGidOnly gid (Map.alter f)

----------------------------------------------------------------------

modifyUserGroups ::
  forall r m.
  (UserGroupStoreInMemEffectConstraints r, m ~ Map (TeamId, UserGroupId) UserGroup) =>
  (m -> m) ->
  Sem r ()
modifyUserGroups u = modify (\ms -> ms {userGroups = u ms.userGroups})

modifyUserGroupsGidOnly ::
  forall r m.
  (UserGroupStoreInMemEffectConstraints r, m ~ Map (TeamId, UserGroupId) UserGroup) =>
  UserGroupId ->
  ((TeamId, UserGroupId) -> m -> m) ->
  Sem r ()
modifyUserGroupsGidOnly gid u = do
  modify $ \ms -> case filter (\(_, gid') -> gid' == gid) (Map.keys ms.userGroups) of
    [] -> ms
    [fullKey] -> ms {userGroups = u fullKey ms.userGroups}
    bad -> error $ "uuid clash: " <> show bad
