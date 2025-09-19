{-# LANGUAGE RecordWildCards #-}

module Wire.UserGroupStore.Postgres where

import Control.Error (MaybeT (..))
import Data.Aeson (ToJSON (toJSON))
import Data.Bifunctor (second)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible
import Data.Id
import Data.Json.Util
import Data.Profunctor
import Data.Qualified (Qualified)
import Data.Range
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.UUID as UUID
import Data.Vector (Vector)
import Hasql.Decoders qualified as HD
import Hasql.Encoders qualified as HE
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
import Wire.UserGroupStore (PaginationState (..), UserGroupPageRequest (..), UserGroupStore (..), toSortBy)

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
    CreateUserGroup team newUserGroup managedBy -> createUserGroup team newUserGroup managedBy
    GetUserGroup team userGroupId -> getUserGroup team userGroupId
    GetUserGroups req -> getUserGroups req
    UpdateUserGroup tid gid gup -> updateGroup tid gid gup
    DeleteUserGroup tid gid -> deleteGroup tid gid
    AddUser gid uid -> addUser gid uid
    UpdateUsers gid uids -> updateUsers gid uids
    RemoveUser gid uid -> removeUser gid uid
    UpdateUserGroupChannels _ gid convIds -> updateUserGroupChannels gid convIds

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
  (UserGroupStorePostgresEffectConstraints r) =>
  TeamId ->
  UserGroupId ->
  Sem r (Maybe UserGroup)
getUserGroup team id_ = do
  pool <- input
  eitherUserGroup <- liftIO $ use pool session
  either throw pure eitherUserGroup
  where
    session :: Session (Maybe UserGroup)
    session = runMaybeT do
      (name, managedBy, createdAt) <- MaybeT $ statement (id_, team) getGroupMetadataStatement
      members <- lift $ Identity <$> statement id_ getGroupMembersStatement
      let membersCount = Nothing
          channelsCount = Nothing
          channels = mempty
      pure $ UserGroup_ {..}

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

divide3 :: (Divisible f) => (p -> (a, b, c)) -> f a -> f b -> f c -> f p
divide3 f a b c = divide (\p -> let (x, y, z) = f p in (x, (y, z))) a (divide id b c)

divide4 :: (Divisible f) => (p -> (a, b, c, d)) -> f a -> f b -> f c -> f d -> f p
divide4 f a b c d = divide (\p -> let (w, x, y, z) = f p in (w, (x, y, z))) a (divide3 id b c d)

divide5 :: (Divisible f) => (p -> (a, b, c, d, e)) -> f a -> f b -> f c -> f d -> f e -> f p
divide5 f a b c d e = divide (\p -> let (v, w, x, y, z) = f p in (v, (w, x, y, z))) a (divide4 id b c d e)

getUserGroups ::
  forall r.
  (UserGroupStorePostgresEffectConstraints r) =>
  UserGroupPageRequest ->
  Sem r UserGroupPage
getUserGroups req@(UserGroupPageRequest {..}) = do
  pool <- input
  eitherResult <- liftIO $ use pool do
    TxSessions.transaction TxSessions.ReadCommitted TxSessions.Read do
      UserGroupPage <$> getUserGroupsSession <*> getCountSession
  either throw pure eitherResult
  where
    getUserGroupsSession :: Tx.Transaction [UserGroupMeta]
    getUserGroupsSession = case (req.searchString, req.paginationState) of
      (Nothing, PaginationSortByName Nothing) -> do
        let encoder = divide id encodeId encodeInt
            stmt = refineResult (mapM parseRow) $ Statement queryBS encoder decodeRow True
        Tx.statement (req.team, pageSizeInt) stmt
      (Nothing, PaginationSortByCreatedAt Nothing) -> do
        let encoder = divide id encodeId encodeInt
            stmt = refineResult (mapM parseRow) $ Statement queryBS encoder decodeRow True
        Tx.statement (req.team, pageSizeInt) stmt
      (Nothing, PaginationSortByName (Just (name, gid))) -> do
        let encoder = divide4 id encodeId encodeGroupName encodeId encodeInt
            stmt = refineResult (mapM parseRow) $ Statement queryBS encoder decodeRow True
        Tx.statement (req.team, name, gid, pageSizeInt) stmt
      (Nothing, PaginationSortByCreatedAt (Just (timestamp, gid))) -> do
        let encoder = divide4 id encodeId encodeTime encodeId encodeInt
            stmt = refineResult (mapM parseRow) $ Statement queryBS encoder decodeRow True
        Tx.statement (req.team, timestamp, gid, pageSizeInt) stmt
      (Just st, PaginationSortByName Nothing) -> do
        let encoder = divide3 id encodeId encodeText encodeInt
            stmt = refineResult (mapM parseRow) $ Statement queryBS encoder decodeRow True
        Tx.statement (req.team, fuzzy st, pageSizeInt) stmt
      (Just st, PaginationSortByCreatedAt Nothing) -> do
        let encoder = divide3 id encodeId encodeText encodeInt
            stmt = refineResult (mapM parseRow) $ Statement queryBS encoder decodeRow True
        Tx.statement (req.team, fuzzy st, pageSizeInt) stmt
      (Just st, PaginationSortByName (Just (name, gid))) -> do
        let encoder = divide5 id encodeId encodeGroupName encodeId encodeText encodeInt
            stmt = refineResult (mapM parseRow) $ Statement queryBS encoder decodeRow True
        Tx.statement (req.team, name, gid, fuzzy st, pageSizeInt) stmt
      (Just st, PaginationSortByCreatedAt (Just (timestamp, gid))) -> do
        let encoder = divide5 id encodeId encodeTime encodeId encodeText encodeInt
            stmt = refineResult (mapM parseRow) $ Statement queryBS encoder decodeRow True
        Tx.statement (req.team, timestamp, gid, fuzzy st, pageSizeInt) stmt

    getCountSession :: Tx.Transaction Int
    getCountSession = case searchString of
      Just st -> do
        let stmt = refineResult parseCount $ Statement countQuery (divide id encodeId encodeText) decodeCount True
        Tx.statement (req.team, fuzzy st) stmt
      Nothing -> do
        let stmt = refineResult parseCount $ Statement countQuery encodeId decodeCount True
        Tx.statement (req.team) stmt
      where
        decodeCount :: HD.Result Int64
        decodeCount = HD.singleRow $ HD.column (HD.nonNullable HD.int8)

        parseCount :: Int64 -> Either Text Int
        parseCount = \case
          n | n < 0 -> Left "Negative count from database"
          n | n > fromIntegral (maxBound :: Int) -> Left "Count from database too large"
          n -> Right $ fromIntegral n

        countQuery :: ByteString
        countQuery =
          "SELECT count(*) FROM user_group WHERE team_id = ($1 :: uuid)"
            <> maybe "" (const " AND name ILIKE ($2 :: text)") searchString

    encodeId :: HE.Params (Id a)
    encodeId = contramap toUUID $ HE.param (HE.nonNullable HE.uuid)

    queryBS :: ByteString
    queryBS = TE.encodeUtf8 userGroupsQuery

    pageSizeInt :: Int32
    pageSizeInt = pageSizeToInt32 req.pageSize

    fuzzy :: Text -> Text
    fuzzy x = "%" <> x <> "%"

    encodeText :: HE.Params Text
    encodeText = HE.param $ HE.nonNullable HE.text

    encodeInt :: HE.Params Int32
    encodeInt = HE.param $ HE.nonNullable HE.int4

    encodeGroupName :: HE.Params UserGroupName
    encodeGroupName = contramap (fromRange . unUserGroupName) encodeText

    encodeTime :: HE.Params UTCTimeMillis
    encodeTime = contramap fromUTCTimeMillis $ HE.param $ HE.nonNullable HE.timestamptz

    decodeRow :: HD.Result [(UUID, Text, Int32, UTCTime, Maybe Int32)]
    decodeRow =
      HD.rowList
        ( (,,,,)
            <$> HD.column (HD.nonNullable HD.uuid)
            <*> HD.column (HD.nonNullable HD.text)
            <*> HD.column (HD.nonNullable HD.int4)
            <*> HD.column (HD.nonNullable HD.timestamptz)
            <*> (if req.includeMemberCount then Just <$> HD.column (HD.nonNullable HD.int4) else pure Nothing)
        )

    parseRow :: (UUID, Text, Int32, UTCTime, Maybe Int32) -> Either Text UserGroupMeta
    parseRow (Id -> id_, namePre, managedByPre, toUTCTimeMillis -> createdAt, membersCountRaw) = do
      managedBy <- case managedByPre of
        0 -> pure ManagedByWire
        1 -> pure ManagedByScim
        bad -> Left $ "Could not parse managedBy value: " <> T.pack (show bad)
      name <- userGroupNameFromText namePre
      let members = Const ()
          membersCount = fromIntegral <$> membersCountRaw
          channelsCount = Nothing
          channels = mempty
      pure $ UserGroup_ {..}

    -- \| Compile a pagination state into select query to return the next page.  Result is the
    -- query string and the search string (which needs escaping).
    userGroupsQuery :: Text
    userGroupsQuery =
      (T.unwords $ filter (not . T.null) [selFrom, whr, constraintClause, like, orderBy, limit])
      where
        selFrom = "select " <> sel <> " from user_group as ug"
        sel =
          T.intercalate ", " $
            filter (not . T.null) $
              ["id", "name", "managed_by", "created_at"]
                <> ["(select count(*) from user_group_member as ugm where ugm.user_group_id = ug.id) as members" | includeMemberCount]
        whr = "where team_id = ($1 :: uuid)"
        sortColumn = toSortBy paginationState
        orderBy = T.unwords ["order by", sortColumnName sortColumn, sortOrderClause sortOrder <> ", id", sortOrderClause sortOrder]
        mConstraintClause = mkConstraints
        constraintClause = fromMaybe "" mConstraintClause
        nameComparisonParamIndex :: Int = maybe 2 (const 4) mConstraintClause
        like =
          maybe
            ""
            (const $ "and name ilike ($" <> T.pack (show nameComparisonParamIndex) <> " :: text)")
            searchString
        limitParamIndex :: Int = case searchString of
          Just _ -> nameComparisonParamIndex + 1
          Nothing -> nameComparisonParamIndex
        limit = "limit ($" <> T.pack (show limitParamIndex) <> " :: int)"
        mkConstraints :: Maybe Text
        mkConstraints =
          case paginationState of
            PaginationSortByName Nothing -> Nothing
            PaginationSortByCreatedAt Nothing -> Nothing
            PaginationSortByName (Just _) ->
              Just $ mkQuery "($2 :: text, $3 :: uuid)"
            PaginationSortByCreatedAt (Just _) ->
              Just $ mkQuery "($2 :: timestamptz, $3 :: uuid)"
          where
            lhs = "(" <> sortColumnName sortColumn <> ", id)"
            mkQuery rhs = T.unwords ["and", lhs, sortOrderOperator sortOrder, rhs]

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
  UserGroupId ->
  Vector (Qualified ConvId) ->
  Sem r ()
updateUserGroupChannels gid convIds = do
  pool <- input
  eitherErrorOrUnit <- liftIO $ use pool session
  either throw pure eitherErrorOrUnit
  where
    session :: Session ()
    session = statement (gid, convIds) upsertStatement

    upsertStatement :: Statement (UserGroupId, Vector (Qualified ConvId)) ()
    upsertStatement =
      lmap
        (bimap toUUID (fmap toJSON))
        $ [resultlessStatement|
          insert into user_group_channels (user_group_id, channel_ids) values(($1 :: uuid), ($2 :: json[]))
          on conflict (user_group_id) do update set channel_ids = ($2 :: json[])
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
