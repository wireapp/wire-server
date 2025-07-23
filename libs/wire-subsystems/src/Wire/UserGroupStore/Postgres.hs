{-# LANGUAGE RecordWildCards #-}

module Wire.UserGroupStore.Postgres where

import Control.Error (MaybeT (..))
import Control.Lens (view, _1, _2, _3)
import Data.Bifunctor (second)
import Data.Functor.Contravariant ((>$<))
import Data.Id
import Data.Json.Util
import Data.Profunctor
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.UUID as UUID
import Data.Vector (Vector)
import Hasql.Decoders qualified as HD
import Hasql.Encoders qualified as HE
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions qualified as Transaction
import Hasql.Transaction.Sessions qualified as TransactionSession
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.UserGroupStore

type UserGroupStorePostgresEffectConstraints r =
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  )

interpretUserGroupStoreToPostgres ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  InterpreterFor UserGroupStore r
interpretUserGroupStoreToPostgres =
  interpret $ \case
    CreateUserGroup team newUserGroup managedBy -> createUserGroupImpl team newUserGroup managedBy
    GetUserGroup team userGroupId -> getUserGroupImpl team userGroupId
    GetUserGroups team listUserGroupsQuery -> getUserGroupsImpl team listUserGroupsQuery
    UpdateUserGroup tid gid gup -> updateGroupImpl tid gid gup
    DeleteUserGroup tid gid -> deleteGroupImpl tid gid
    AddUser gid uid -> addUserImpl gid uid
    RemoveUser gid uid -> removeUserImpl gid uid

getUserGroupImpl ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  UserGroupId ->
  Sem r (Maybe UserGroup)
getUserGroupImpl team id_ = do
  pool <- input
  eitherUserGroup <- liftIO $ use pool session
  either throw pure eitherUserGroup
  where
    session :: Session (Maybe UserGroup)
    session = runMaybeT do
      (name, managedBy, createdAt) <- MaybeT $ statement (id_, team) getGroupMetadataStatement
      members <- lift $ statement id_ getGroupMembersStatement
      pure $ UserGroup {..}

    decodeMetadataRow :: (Text, Int32, UTCTime) -> Either Text (UserGroupName, ManagedBy, UTCTimeMillis)
    decodeMetadataRow (name, managedByInt, utcTime) =
      (,,toUTCTimeMillis utcTime)
        <$> userGroupNameFromText name
        <*> managedByFromInt32 managedByInt

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

getUserGroupsImpl ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  PaginationState ->
  Sem r [UserGroup]
getUserGroupsImpl tid pstate = do
  pool <- input
  eitherResult <- liftIO $ use pool session
  either throw pure eitherResult
  where
    session :: Session [UserGroup]
    session = sessionFromParams
      where
        (encodeUtf8 -> sqlBS, sqlParams) = paginationStateToSqlQuery tid pstate

        -- fill just the right number of holes in the statement using matching encoders
        sessionFromParams = case sqlParams of
          [] -> mkSession (statement ()) HE.noParams
          [a] -> mkSession (statement a) encode1
          [a, b] -> mkSession (statement (a, b)) encode2
          bad -> error $ "internal error in paginationStateToSqlQuery: " <> show bad

        encode1 = HE.param (HE.nonNullable HE.text)
        encode2 = (view _1 >$< encode1) <> (view _2 >$< encode1)

        -- shared code from all cases in run above
        mkSession :: (Statement params [UserGroup] -> Session [UserGroup]) -> HE.Params params -> Session [UserGroup]
        mkSession mkStatement encodeParams =
          mkStatement . refineResult (mapM parseRow) $
            Statement sqlBS encodeParams decodeRow True

    decodeRow :: HD.Result [(UUID, Text, Int32, UTCTime)]
    decodeRow =
      HD.rowList
        ( (,,,)
            <$> HD.column (HD.nonNullable HD.uuid)
            <*> HD.column (HD.nonNullable HD.text)
            <*> HD.column (HD.nonNullable HD.int4)
            <*> HD.column (HD.nonNullable HD.timestamptz)
        )

    parseRow :: (UUID, Text, Int32, UTCTime) -> Either Text UserGroup
    parseRow (Id -> id_, namePre, managedByPre, toUTCTimeMillis -> createdAt) = do
      managedBy <- case managedByPre of
        0 -> pure ManagedByWire
        1 -> pure ManagedByScim
        bad -> Left $ "Could not parse managedBy value: " <> T.pack (show bad)
      name <- userGroupNameFromText namePre
      let members = mempty -- TODO: do we want `data UserGroup (m :: * -> *) = UserGroup { members :: m (Vector ...), ... }`?
      pure $ UserGroup {..}

-- | Compile a pagination state into select query to return the next page.  Result is the
-- query string and the search string (which needs escaping).
paginationStateToSqlQuery :: TeamId -> PaginationState -> (Text, [Text])
paginationStateToSqlQuery (Id (UUID.toText -> tid)) pstate =
  ( T.unwords $ filter (not . T.null) [sel, whr, cstr, like, orderBy, limit],
    cstrParams <> searchParams
  )
  where
    sel = "select id, name, managed_by, created_at from user_group"
    whr = "where team_id='" <> tid <> "'"
    orderBy = T.unwords ["order by", pstate.sortBy.toText, pstate.sortOrder.toText <> ", id", pstate.sortOrder.toText]
    limit = T.unwords ["limit", T.pack $ show $ pageSizeToInt pstate.pageSize]

    (cstr, cstrParams) = maybe ("", []) mkConstraints pstate.lastSeen
    (like, searchParams) =
      maybe
        ("", [])
        (\st -> ("and name ilike ($" <> T.pack (show (length cstrParams + 1)) <> " :: text)", ["%" <> st <> "%"]))
        pstate.searchString

    mkConstraints :: (HasCallStack) => LastSeen -> (Text, [Text])
    mkConstraints (LastSeen mName mCreatedAt lastId) =
      (T.unwords ["and", lhs, pstate.sortOrder.op, rhs], params)
      where
        lhs = "(" <> pstate.sortBy.toText <> ", id)"
        (rhs, params) = case (mName, mCreatedAt) of
          (Just ugn, Nothing) ->
            ( "($1 :: text, '" <> UUID.toText (toUUID lastId) <> "')",
              [userGroupNameToText ugn]
            )
          (Nothing, Just timeStamp) ->
            ( "('" <> showUTCTimeMillis timeStamp <> "', '" <> UUID.toText (toUUID lastId) <> "')",
              []
            )
          _ -> error "paginationStateToSqlQuery: LastSeen must have either name or createdAt"

createUserGroupImpl ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  NewUserGroup ->
  ManagedBy ->
  Sem r UserGroup
createUserGroupImpl team newUserGroup managedBy = do
  pool <- input
  eitherUuid <- liftIO $ use pool session
  either throw pure eitherUuid
  where
    session :: Session UserGroup
    session = TransactionSession.transaction Transaction.Serializable TransactionSession.Write do
      (id_, name, managedBy_, createdAt) <- Transaction.statement (newUserGroup.name, team, managedBy) insertGroupStatement
      Transaction.statement (toUUID id_, newUserGroup.members) insertGroupMembersStatement
      pure UserGroup {members = newUserGroup.members, managedBy = managedBy_, ..}

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

updateGroupImpl ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  UserGroupId ->
  UserGroupUpdate ->
  Sem r (Maybe ())
updateGroupImpl tid gid gup = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session (Maybe ())
    session = TransactionSession.transaction Transaction.Serializable TransactionSession.Write do
      found <- isJust <$> Transaction.statement (tid, gid, gup.name) updateGroupStatement
      pure $ if found then Just () else Nothing

    updateGroupStatement :: Statement (TeamId, UserGroupId, UserGroupName) (Maybe Bool)
    updateGroupStatement =
      lmap (\(t, g, n) -> (t.toUUID, g.toUUID, userGroupNameToText n)) $
        [maybeStatement|
          update user_group set name = ($3 :: text)
            where team_id = ($1 :: uuid) and id = ($2 :: uuid)
            returning (true :: bool)
          |]

deleteGroupImpl ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  UserGroupId ->
  Sem r (Maybe ())
deleteGroupImpl tid gid = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session (Maybe ())
    session = TransactionSession.transaction Transaction.Serializable TransactionSession.Write do
      found <- isJust <$> Transaction.statement (tid, gid) deleteGroupStatement
      pure $ if found then Just () else Nothing

    deleteGroupStatement :: Statement (TeamId, UserGroupId) (Maybe Bool)
    deleteGroupStatement =
      lmap (\(t, g) -> (t.toUUID, g.toUUID)) $
        [maybeStatement|
          delete from user_group
            where team_id = ($1 :: uuid) and id = ($2 :: uuid)
            returning (true :: bool)
          |]

addUserImpl ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  UserGroupId ->
  UserId ->
  Sem r ()
addUserImpl =
  crudUser
    [resultlessStatement|
      insert into user_group_member (user_group_id, user_id) values (($1 :: uuid), ($2 :: uuid))
      |]

removeUserImpl ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  UserGroupId ->
  UserId ->
  Sem r ()
removeUserImpl =
  crudUser
    [resultlessStatement|
      delete from user_group_member where user_group_id = ($1 :: uuid) and user_id = ($2 :: uuid)
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
    session = TransactionSession.transaction Transaction.Serializable TransactionSession.Write do
      Transaction.statement
        (gid, uid)
        (lmap (\(gid_, uid_) -> (gid_.toUUID, uid_.toUUID)) op)

toRelationTable :: a -> Vector b -> (Vector a, Vector b)
toRelationTable a bs = (a <$ bs, bs)
