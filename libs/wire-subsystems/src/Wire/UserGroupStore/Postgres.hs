{-# LANGUAGE RecordWildCards #-}

module Wire.UserGroupStore.Postgres where

import Control.Error (MaybeT (..))
import Data.Bifunctor (second)
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Profunctor
import Data.Qualified (Local, QualifiedWithTag (tUntagged), inputQualifyLocal, qualifyAs)
import Data.Text qualified as T
import Data.Time
import Data.UUID as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Decoders qualified as HD
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxSessions
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input
import Wire.API.Pagination
import Wire.API.User.Profile
import Wire.API.UserGroup hiding (UpdateUserGroupChannels)
import Wire.API.UserGroup.Pagination
import Wire.PaginationState
import Wire.Postgres
import Wire.UserGroupStore (UserGroupPageRequest (..), UserGroupStore (..))

type UserGroupStorePostgresEffectConstraints r =
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  )

interpretUserGroupStoreToPostgres ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r, Member (Input (Local ())) r) =>
  InterpreterFor UserGroupStore r
interpretUserGroupStoreToPostgres =
  interpret $ \case
    CreateUserGroup team newUserGroup managedBy -> createUserGroup team newUserGroup managedBy
    GetUserGroup team userGroupId includeChannels -> getUserGroup team userGroupId includeChannels
    GetUserGroups req -> getUserGroups req
    UpdateUserGroup tid gid gup -> updateGroup tid gid gup
    DeleteUserGroup tid gid -> deleteGroup tid gid
    AddUser gid uid -> addUser gid uid
    UpdateUsers gid uids -> updateUsers gid uids
    RemoveUser gid uid -> removeUser gid uid
    AddUserGroupChannels gid convIds -> updateUserGroupChannels True gid convIds
    UpdateUserGroupChannels gid convIds -> updateUserGroupChannels False gid convIds
    GetUserGroupIdsForUsers uids -> getUserGroupIdsForUsers uids

getUserGroupIdsForUsers :: (UserGroupStorePostgresEffectConstraints r) => [UserId] -> Sem r (Map UserId [UserGroupId])
getUserGroupIdsForUsers uidsList = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session (Map UserId [UserGroupId])
    session = do
      rows <- statement (V.fromList (toUUID <$> uidsList)) stmt
      pure $ Map.fromListWith (<>) [(Id u, [Id g]) | (u, g) <- V.toList rows]

    stmt :: Statement (Vector UUID) (Vector (UUID, UUID))
    stmt =
      [vectorStatement|
            select (ugm.user_id :: uuid), (ugm.user_group_id :: uuid)
              from user_group_member as ugm
              where ugm.user_id = ANY (($1 :: uuid[]))
          |]

updateUsers :: (UserGroupStorePostgresEffectConstraints r) => UserGroupId -> Vector UserId -> Sem r ()
updateUsers gid uids = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = TxSessions.transaction TxSessions.Serializable TxSessions.Write do
      Tx.statement gid deleteAllUsersStatement
      Tx.statement (toUUID gid, uids) insertGroupMembersStatement

    deleteAllUsersStatement :: Statement UserGroupId ()
    deleteAllUsersStatement =
      dimap (.toUUID) (const ()) $
        [resultlessStatement|
          delete from user_group_member where user_group_id = ($1 :: uuid)
          |]

getUserGroup ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r, Member (Input (Local ())) r) =>
  TeamId ->
  UserGroupId ->
  Bool ->
  Sem r (Maybe UserGroup)
getUserGroup team id_ includeChannels = do
  pool <- input
  loc <- inputQualifyLocal ()
  eitherUserGroup <- liftIO $ use pool (if includeChannels then sessionWithChannels loc else session)
  either throw pure eitherUserGroup
  where
    session :: Session (Maybe UserGroup)
    session = runMaybeT do
      (name, managedBy, createdAt) <- MaybeT $ statement (id_, team) getGroupMetadataStatement
      members <- lift $ Identity <$> statement id_ getGroupMembersStatement
      let membersCount = Just . V.length $ runIdentity members
          channelsCount = Nothing
          channels = mempty
      pure $ UserGroup_ {..}

    sessionWithChannels :: Local a -> Session (Maybe UserGroup)
    sessionWithChannels loc = runMaybeT do
      (name, managedBy, createdAt, memberIds, channelIds) <- MaybeT $ statement (id_, team) getGroupWithMembersAndChannelsStatement
      let members = Identity (fmap Id memberIds)
          membersCount = Just $ V.length memberIds
          channels = Just (fmap (tUntagged . qualifyAs loc . Id) channelIds)
          channelsCount = Just $ V.length channelIds
      pure $ UserGroup_ {..}

    decodeMetadataRow :: (Text, Int32, UTCTime) -> Either Text (UserGroupName, ManagedBy, UTCTimeMillis)
    decodeMetadataRow (name, managedByInt, utcTime) =
      (,,toUTCTimeMillis utcTime)
        <$> userGroupNameFromText name
        <*> managedByFromInt32 managedByInt

    decodeWithArrays :: (Text, Int32, UTCTime, Vector UUID, Vector UUID) -> Either Text (UserGroupName, ManagedBy, UTCTimeMillis, Vector UUID, Vector UUID)
    decodeWithArrays (name, managedByInt, utcTime, membs, chans) = do
      n <- userGroupNameFromText name
      m <- managedByFromInt32 managedByInt
      pure (n, m, toUTCTimeMillis utcTime, membs, chans)

    getGroupMetadataStatement :: Statement (UserGroupId, TeamId) (Maybe (UserGroupName, ManagedBy, UTCTimeMillis))
    getGroupMetadataStatement =
      lmap (\(gid, tid) -> (gid.toUUID, tid.toUUID))
        . refineResult (mapM decodeMetadataRow)
        $ [maybeStatement|
         select (name :: text), (managed_by :: int), (created_at :: timestamptz)
           from user_group where id = ($1 :: uuid) AND team_id = ($2 :: uuid)
         |]

    getGroupMembersStatement :: Statement UserGroupId (Vector UserId)
    getGroupMembersStatement =
      dimap (.toUUID) (fmap Id) $
        [vectorStatement|
          select (user_id :: uuid) from user_group_member where user_group_id = ($1 :: uuid)
          |]

    getGroupWithMembersAndChannelsStatement :: Statement (UserGroupId, TeamId) (Maybe (UserGroupName, ManagedBy, UTCTimeMillis, Vector UUID, Vector UUID))
    getGroupWithMembersAndChannelsStatement =
      lmap (\(gid, tid) -> (gid.toUUID, tid.toUUID))
        . refineResult (mapM decodeWithArrays)
        $ [maybeStatement|
            select
              (name :: text),
              (managed_by :: int),
              (created_at :: timestamptz),
              coalesce((select array_agg(ugm.user_id) from user_group_member ugm where ugm.user_group_id = ug.id), array[]::uuid[]) :: uuid[],
              coalesce((select array_agg(ugc.conv_id) from user_group_channel ugc where ugc.user_group_id = ug.id), array[]::uuid[]) :: uuid[]
            from user_group ug
            where ug.id = ($1 :: uuid) and ug.team_id = ($2 :: uuid)
          |]

getUserGroups ::
  forall r.
  ( UserGroupStorePostgresEffectConstraints r,
    Member (Input (Local ())) r
  ) =>
  UserGroupPageRequest ->
  Sem r UserGroupPage
getUserGroups req@(UserGroupPageRequest {..}) = do
  loc <- inputQualifyLocal ()
  runTransaction TxSessions.ReadCommitted TxSessions.Read $
    UserGroupPage <$> getUserGroupsSession loc <*> getCountSession
  where
    getUserGroupsSession :: Local () -> Tx.Transaction [UserGroupMeta]
    getUserGroupsSession loc =
      Tx.statement () $
        refineResult (mapM (parseRow loc)) $
          buildStatement
            ( mconcat $
                [ literal "select",
                  literal selectors,
                  literal "from user_group as ug",
                  where_
                    ( [clause1 "team_id" "=" req.team]
                        <> [ clause (sortOrderOperator sortOrder) c
                             | c <- toList (paginationClause paginationState)
                           ]
                        <> toList (like "name" <$> searchString)
                    )
                ]
                  <> [ orderBy
                         [ (sortColumn, sortOrder),
                           ("id", sortOrder)
                         ],
                       limit (pageSizeToInt32 req.pageSize)
                     ]
            )
            decodeRow

    getCountSession :: Tx.Transaction Int
    getCountSession =
      Tx.statement () $
        refineResult parseCount $
          buildStatement
            ( literal "select count(*) from user_group"
                <> where_
                  ( [clause1 "team_id" "=" req.team]
                      <> toList (like "name" <$> searchString)
                  )
            )
            (HD.singleRow (HD.column (HD.nonNullable HD.int8)))

    decodeRow :: HD.Result [(UUID, Text, Int32, UTCTime, Maybe Int32, Int32, Maybe (Vector UUID))]
    decodeRow =
      HD.rowList
        ( (,,,,,,)
            <$> HD.column (HD.nonNullable HD.uuid)
            <*> HD.column (HD.nonNullable HD.text)
            <*> HD.column (HD.nonNullable HD.int4)
            <*> HD.column (HD.nonNullable HD.timestamptz)
            <*> (if req.includeMemberCount then Just <$> HD.column (HD.nonNullable HD.int4) else pure Nothing)
            <*> HD.column (HD.nonNullable HD.int4)
            <*> ( if req.includeChannels
                    then
                      Just
                        <$> HD.column
                          ( HD.nonNullable
                              ( HD.array
                                  ( HD.dimension
                                      V.replicateM
                                      (HD.element (HD.nonNullable HD.uuid))
                                  )
                              )
                          )
                    else pure Nothing
                )
        )

    parseRow :: Local a -> (UUID, Text, Int32, UTCTime, Maybe Int32, Int32, Maybe (Vector UUID)) -> Either Text UserGroupMeta
    parseRow loc (Id -> id_, namePre, managedByPre, toUTCTimeMillis -> createdAt, membersCountRaw, channelsCountRaw, maybeChannels) = do
      managedBy <- case managedByPre of
        0 -> pure ManagedByWire
        1 -> pure ManagedByScim
        bad -> Left $ "Could not parse managedBy value: " <> T.pack (show bad)
      name <- userGroupNameFromText namePre
      let members = Const ()
          membersCount = fromIntegral <$> membersCountRaw
          channelsCount = Just (fromIntegral channelsCountRaw)
          channels = fmap (fmap (tUntagged . qualifyAs loc . Id)) maybeChannels
      pure $ UserGroup_ {..}

    sortColumn :: Text
    sortColumn = case paginationState of
      PaginationSortByName _ -> "name"
      PaginationSortByCreatedAt _ -> "created_at"

    selectors :: Text
    selectors =
      T.intercalate ", " $
        filter (not . T.null) $
          ["id", "name", "managed_by", "created_at"]
            <> ["(select count(*) from user_group_member as ugm where ugm.user_group_id = ug.id) as members" | includeMemberCount]
            <> ["(select count(*) from user_group_channel as ugc where ugc.user_group_id = ug.id) as channels"]
            <> ["coalesce((select array_agg(ugc.conv_id) from user_group_channel as ugc where ugc.user_group_id = ug.id), array[]::uuid[]) as channel_ids" | includeChannels]

createUserGroup ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  NewUserGroup ->
  ManagedBy ->
  Sem r UserGroup
createUserGroup team newUserGroup managedBy = do
  pool <- input
  eitherUuid <- liftIO $ use pool session
  either throw pure eitherUuid
  where
    session :: Session UserGroup
    session = TxSessions.transaction TxSessions.Serializable TxSessions.Write do
      (id_, name, managedBy_, createdAt) <- Tx.statement (newUserGroup.name, team, managedBy) insertGroupStatement
      Tx.statement (toUUID id_, newUserGroup.members) insertGroupMembersStatement
      pure
        UserGroup_
          { membersCount = Nothing,
            members = Identity newUserGroup.members,
            channels = mempty,
            managedBy = managedBy_,
            channelsCount = Nothing,
            id_,
            name,
            createdAt
          }

    decodeMetadataRow :: (UUID, Text, Int32, UTCTime) -> Either Text (UserGroupId, UserGroupName, ManagedBy, UTCTimeMillis)
    decodeMetadataRow (groupId, name, managedByInt, utcTime) =
      (Id groupId,,,toUTCTimeMillis utcTime)
        <$> userGroupNameFromText name
        <*> managedByFromInt32 managedByInt

    insertGroupStatement :: Statement (UserGroupName, TeamId, ManagedBy) (UserGroupId, UserGroupName, ManagedBy, UTCTimeMillis)
    insertGroupStatement =
      lmap (\(n, t, m) -> (userGroupNameToText n, t.toUUID, managedByToInt32 m))
        . refineResult decodeMetadataRow
        $ [singletonStatement|
            insert into user_group (name, team_id, managed_by)
              values ($1 :: text, $2 :: uuid, $3 :: int)
              returning id :: uuid, name :: text, managed_by :: int, created_at :: timestamptz
            |]

-- This can perhaps be simplified using rel8
insertGroupMembersStatement :: Statement (UUID, Vector UserId) ()
insertGroupMembersStatement =
  lmap (second (fmap (.toUUID)) . uncurry toRelationTable) $
    [resultlessStatement|
      insert into user_group_member (user_group_id, user_id) select * from unnest ($1 :: uuid[], $2 :: uuid[])
      |]

updateGroup ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  UserGroupId ->
  UserGroupUpdate ->
  Sem r (Maybe ())
updateGroup tid gid gup = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session (Maybe ())
    session = TxSessions.transaction TxSessions.Serializable TxSessions.Write do
      found <- isJust <$> Tx.statement (tid, gid, gup.name) updateGroupStatement
      pure $ if found then Just () else Nothing

    updateGroupStatement :: Statement (TeamId, UserGroupId, UserGroupName) (Maybe Bool)
    updateGroupStatement =
      lmap (\(t, g, n) -> (t.toUUID, g.toUUID, userGroupNameToText n)) $
        [maybeStatement|
          update user_group set name = ($3 :: text)
            where team_id = ($1 :: uuid) and id = ($2 :: uuid)
            returning (true :: bool)
          |]

deleteGroup ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  UserGroupId ->
  Sem r (Maybe ())
deleteGroup tid gid = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session (Maybe ())
    session = TxSessions.transaction TxSessions.Serializable TxSessions.Write do
      found <- isJust <$> Tx.statement (tid, gid) deleteGroupStatement
      pure $ if found then Just () else Nothing

    deleteGroupStatement :: Statement (TeamId, UserGroupId) (Maybe Bool)
    deleteGroupStatement =
      lmap (\(t, g) -> (t.toUUID, g.toUUID)) $
        [maybeStatement|
          delete from user_group
            where team_id = ($1 :: uuid) and id = ($2 :: uuid)
            returning (true :: bool)
          |]

addUser ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  UserGroupId ->
  UserId ->
  Sem r ()
addUser =
  crudUser
    [resultlessStatement|
      insert into user_group_member (user_group_id, user_id) values (($1 :: uuid), ($2 :: uuid))
      |]

removeUser ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  UserGroupId ->
  UserId ->
  Sem r ()
removeUser =
  crudUser
    [resultlessStatement|
      delete from user_group_member where user_group_id = ($1 :: uuid) and user_id = ($2 :: uuid)
      |]

updateUserGroupChannels ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  Bool ->
  UserGroupId ->
  Vector ConvId ->
  Sem r ()
updateUserGroupChannels appendOnly gid convIds = do
  pool <- input
  eitherErrorOrUnit <- liftIO $ use pool session
  either throw pure eitherErrorOrUnit
  where
    session :: Session ()
    session = TxSessions.transaction TxSessions.Serializable TxSessions.Write $ do
      unless appendOnly $
        Tx.statement (gid, convIds) deleteStatement
      Tx.statement (gid, convIds) insertStatement

    deleteStatement :: Statement (UserGroupId, Vector ConvId) ()
    deleteStatement =
      lmap
        (bimap toUUID (fmap toUUID))
        $ [resultlessStatement|
          delete from user_group_channel where user_group_id = ($1 :: uuid) and conv_id not in (SELECT unnest($2 :: uuid[]))
          |]

    insertStatement :: Statement (UserGroupId, Vector ConvId) ()
    insertStatement =
      lmap (bimap (fmap (.toUUID)) (fmap (.toUUID)) . uncurry toRelationTable) $
        [resultlessStatement|
          insert into user_group_channel (user_group_id, conv_id)  select * from unnest ($1 :: uuid[], $2 :: uuid[])
          on conflict (user_group_id, conv_id) do nothing
          |]

crudUser ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  Statement (UUID, UUID) () ->
  UserGroupId ->
  UserId ->
  Sem r ()
crudUser op gid uid = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = TxSessions.transaction TxSessions.Serializable TxSessions.Write do
      Tx.statement
        (gid, uid)
        (lmap (\(gid_, uid_) -> (gid_.toUUID, uid_.toUUID)) op)

toRelationTable :: a -> Vector b -> (Vector a, Vector b)
toRelationTable a bs = (a <$ bs, bs)
