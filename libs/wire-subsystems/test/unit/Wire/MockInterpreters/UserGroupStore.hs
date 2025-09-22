{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- FUTUREWORK: move this next to Postgres interpreter; write integration tests that run random,
-- valid command sequences against both and compare.

module Wire.MockInterpreters.UserGroupStore where

import Control.Lens ((%~), _2)
import Data.Domain (Domain (Domain))
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Qualified (Qualified (Qualified))
import Data.Text qualified as T
import Data.Time.Clock
import Data.Vector (Vector, fromList)
import GHC.Stack
import Imports
import Polysemy
import Polysemy.Internal (Append)
import Polysemy.State
import System.Random (StdGen, mkStdGen)
import Wire.API.Pagination
import Wire.API.User
import Wire.API.UserGroup hiding (UpdateUserGroupChannels)
import Wire.API.UserGroup.Pagination
import Wire.MockInterpreters.Now
import Wire.MockInterpreters.Random
import Wire.Sem.Random qualified as Rnd
import Wire.UserGroupStore

type UserGroupInMemState = Map (TeamId, UserGroupId) UserGroup

type UserGroupStoreInMemEffectConstraints r =
  ( Member (State UserGroupInMemState) r,
    Member Rnd.Random r,
    Member MockNow r,
    HasCallStack
  )

type UserGroupStoreInMemEffectStack =
  '[ UserGroupStore,
     State UserGroupInMemState,
     Rnd.Random,
     State StdGen
   ]

runInMemoryUserGroupStore :: (Member MockNow r) => UserGroupInMemState -> Sem (UserGroupStoreInMemEffectStack `Append` r) a -> Sem r a
runInMemoryUserGroupStore state =
  evalState (mkStdGen 3)
    . randomToStatefulStdGen
    . evalState state
    . userGroupStoreTestInterpreter

userGroupStoreTestInterpreter :: (UserGroupStoreInMemEffectConstraints r) => InterpreterFor UserGroupStore r
userGroupStoreTestInterpreter =
  interpret $ \case
    CreateUserGroup tid ng mb -> createUserGroupImpl tid ng mb
    GetUserGroup tid gid -> getUserGroupImpl tid gid
    GetUserGroups req -> getUserGroupsImpl req
    UpdateUserGroup tid gid gup -> updateUserGroupImpl tid gid gup
    DeleteUserGroup tid gid -> deleteUserGroupImpl tid gid
    AddUser gid uid -> addUserImpl gid uid
    UpdateUsers gid uids -> updateUsersImpl gid uids
    RemoveUser gid uid -> removeUserImpl gid uid
    UpdateUserGroupChannels _ gid convIds -> updateUserGroupChannelsImpl gid convIds

updateUsersImpl :: (UserGroupStoreInMemEffectConstraints r) => UserGroupId -> Vector UserId -> Sem r ()
updateUsersImpl gid uids = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = Identity . fromList . nub $ toList uids} :: UserGroup)

  modifyUserGroupsGidOnly gid (Map.alter f)

createUserGroupImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> NewUserGroup -> ManagedBy -> Sem r UserGroup
createUserGroupImpl tid nug managedBy = do
  now <- get @UTCTime
  gid <- Id <$> Rnd.uuid
  let ug =
        UserGroup_
          { id_ = gid,
            name = nug.name,
            members = Identity nug.members,
            channels = mempty,
            membersCount = Nothing,
            channelsCount = Nothing,
            managedBy = managedBy,
            createdAt = toUTCTimeMillis now
          }

  modify (Map.insert (tid, gid) ug)
  pure ug

getUserGroupImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> UserGroupId -> Sem r (Maybe UserGroup)
getUserGroupImpl tid gid = (Map.lookup (tid, gid)) <$> get @UserGroupInMemState

getUserGroupsImpl :: (UserGroupStoreInMemEffectConstraints r) => UserGroupPageRequest -> Sem r UserGroupPage
getUserGroupsImpl UserGroupPageRequest {..} = do
  meta <- ((snd <$>) . sieve . fmap (_2 %~ userGroupToMeta) . Map.toList) <$> get @UserGroupInMemState
  pure $ UserGroupPage meta (length meta)
  where
    sieve,
      dropAfterPageSize,
      dropBeforeStart,
      orderByKeys,
      narrowToSearchString,
      narrowToTeam ::
        [((TeamId, UserGroupId), UserGroupMeta)] -> [((TeamId, UserGroupId), UserGroupMeta)]

    sieve =
      dropAfterPageSize
        . dropBeforeStart
        . orderByKeys
        . narrowToSearchString
        . narrowToTeam

    narrowToTeam = filter (\((thisTid, _), _) -> thisTid == team)

    narrowToSearchString =
      filter (\(_, ug) -> maybe True (`T.isInfixOf` userGroupNameToText ug.name) searchString)

    orderByKeys = Imports.sortBy cmp
      where
        cmp (_, ug) (_, ug') = case (paginationState, sortOrder) of
          (PaginationSortByName _, Asc) -> (n, i) `compare` (n', i')
          (PaginationSortByName _, Desc) -> (n', i') `compare` (n, i)
          (PaginationSortByCreatedAt _, Asc) -> (c, i) `compare` (c', i')
          (PaginationSortByCreatedAt _, Desc) -> (c', i') `compare` (c, i)
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
        sqlConds :: ((TeamId, UserGroupId), UserGroupMeta) -> Bool
        sqlConds ((_, _), row) =
          case (paginationState, sortOrder) of
            (PaginationSortByName (Just (name, tieBreaker)), Asc) -> (name, tieBreaker) >= (row.name, row.id_)
            (PaginationSortByName (Just (name, tieBreaker)), Desc) -> (name, tieBreaker) <= (row.name, row.id_)
            (PaginationSortByCreatedAt (Just (ts, tieBreaker)), Asc) -> (ts, tieBreaker) >= (row.createdAt, row.id_)
            (PaginationSortByCreatedAt (Just (ts, tieBreaker)), Desc) -> (ts, tieBreaker) <= (row.createdAt, row.id_)
            (_, _) -> False

    dropAfterPageSize = take (pageSizeToInt pageSize)

updateUserGroupImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> UserGroupId -> UserGroupUpdate -> Sem r (Maybe ())
updateUserGroupImpl tid gid (UserGroupUpdate newName) = do
  exists <- getUserGroupImpl tid gid
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {name = newName} :: UserGroup)

  modify (Map.alter f (tid, gid))
  pure $ exists $> ()

deleteUserGroupImpl :: (UserGroupStoreInMemEffectConstraints r) => TeamId -> UserGroupId -> Sem r (Maybe ())
deleteUserGroupImpl tid gid = do
  exists <- getUserGroupImpl tid gid
  modify (Map.delete (tid, gid))
  pure $ exists $> ()

addUserImpl :: (UserGroupStoreInMemEffectConstraints r) => UserGroupId -> UserId -> Sem r ()
addUserImpl gid uid = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = Identity . fromList . nub $ uid : toList (runIdentity g.members)} :: UserGroup)

  modifyUserGroupsGidOnly gid (Map.alter f)

removeUserImpl :: (UserGroupStoreInMemEffectConstraints r) => UserGroupId -> UserId -> Sem r ()
removeUserImpl gid uid = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) = Just (g {members = Identity . fromList $ toList (runIdentity g.members) \\ [uid]} :: UserGroup)

  modifyUserGroupsGidOnly gid (Map.alter f)

updateUserGroupChannelsImpl ::
  (UserGroupStoreInMemEffectConstraints r) =>
  UserGroupId ->
  Vector ConvId ->
  Sem r ()
updateUserGroupChannelsImpl gid convIds = do
  let f :: Maybe UserGroup -> Maybe UserGroup
      f Nothing = Nothing
      f (Just g) =
        Just
          ( g
              { channels = Identity $ Just $ flip Qualified (Domain "<local>") <$> convIds,
                channelsCount = Just $ length convIds
              } ::
              UserGroup
          )

  modifyUserGroupsGidOnly gid (Map.alter f)

----------------------------------------------------------------------

modifyUserGroupsGidOnly ::
  forall r m.
  (UserGroupStoreInMemEffectConstraints r, m ~ Map (TeamId, UserGroupId) UserGroup) =>
  UserGroupId ->
  ((TeamId, UserGroupId) -> m -> m) ->
  Sem r ()
modifyUserGroupsGidOnly gid u = do
  modify $ \ms -> case filter (\(_, gid') -> gid' == gid) (Map.keys ms) of
    [] -> ms
    [fullKey] -> u fullKey ms
    bad -> error $ "uuid clash: " <> show bad
