{-# LANGUAGE RecordWildCards #-}

module Wire.UserGroupStore.Postgres where

import Control.Error (MaybeT (..))
import Data.Bifunctor (second)
import Data.Id
import Data.Json.Util
import Data.Profunctor
import Data.Time
import Data.UUID
import Data.Vector (Vector)
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions qualified as Transaction
import Hasql.Transaction.Sessions qualified as TransactionSession
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.UserGroupStore

interpretUserGroupStoreToPostgres :: (Member (Embed IO) r, Member (Input Pool) r) => InterpreterFor UserGroupStore r
interpretUserGroupStoreToPostgres =
  interpret $ \case
    CreateUserGroup team newUserGroup managedBy -> createUserGroupImpl team newUserGroup managedBy
    GetUserGroup team userGroupId -> getUserGroupImpl team userGroupId

getUserGroupImpl :: (Member (Embed IO) r, Member (Input Pool) r) => TeamId -> UserGroupId -> Sem r (Maybe UserGroup)
getUserGroupImpl team id_ = do
  pool <- input
  eitherUserGroup <- liftIO $ use pool session
  case eitherUserGroup of
    Left err -> error $ show err
    Right g -> pure g
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
           from user_group where id = ($1 :: uuid) AND team = ($2 :: uuid)
         |]

    getGroupMembersStatement :: Statement UserGroupId (Vector UserId)
    getGroupMembersStatement =
      dimap (.toUUID) (fmap Id) $
        [vectorStatement|
          select (user_id :: uuid) from user_group_member where user_group_id = ($1 :: uuid)
          |]

createUserGroupImpl :: (Member (Embed IO) r, Member (Input Pool) r) => TeamId -> NewUserGroup -> ManagedBy -> Sem r UserGroup
createUserGroupImpl team newUserGroup managedBy = do
  pool <- input
  eitherUuid <- liftIO $ use pool session
  case eitherUuid of
    -- TODO: Deal with this better
    Left err -> error $ show err
    Right grp -> pure grp
  where
    session :: Session UserGroup
    session = TransactionSession.transaction Transaction.Serializable TransactionSession.Write do
      (id_, name, managedBy_, createdAt) <-
        Transaction.statement
          (newUserGroup.name, team, managedBy)
          insertGroupStatement
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

    toRelationTable :: a -> Vector b -> (Vector a, Vector b)
    toRelationTable a bs = (a <$ bs, bs)

    -- This can perhaps be simplified using rel8
    insertGroupMembersStatement :: Statement (UUID, Vector UserId) ()
    insertGroupMembersStatement =
      lmap (second (fmap (.toUUID)) . uncurry toRelationTable) $
        [singletonStatement|
          insert into user_group_member (user_group_id, user_id) select * from unnest ($1 :: uuid[], $2 :: uuid[])
          |]
