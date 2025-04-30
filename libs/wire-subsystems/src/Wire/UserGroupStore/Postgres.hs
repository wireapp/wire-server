{-# LANGUAGE RecordWildCards #-}

module Wire.UserGroupStore.Postgres where

import Control.Error (MaybeT (..))
import Data.Bifunctor (second)
import Data.Id
import Data.Profunctor
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
    GetUserGroup userGroupId -> getUserGroupImpl userGroupId

getUserGroupImpl :: (Member (Embed IO) r, Member (Input Pool) r) => UserGroupId -> Sem r (Maybe UserGroup)
getUserGroupImpl id_ = do
  pool <- input
  eitherUserGroup <- liftIO $ use pool session
  case eitherUserGroup of
    Left err -> error $ show err
    Right g -> pure g
  where
    session :: Session (Maybe UserGroup)
    session = runMaybeT do
      (name, team, managedBy) <- MaybeT $ statement id_ getGroupMetadataStatement
      members <- lift $ statement id_ getGroupMembersStatement
      pure $ UserGroup {..}

    decodeMetadataRow :: (Text, UUID, Int32) -> Either Text (Text, TeamId, ManagedBy)
    decodeMetadataRow (name, teamUUID, managedByInt) = (name,Id teamUUID,) <$> managedByFromInt32 managedByInt

    getGroupMetadataStatement :: Statement UserGroupId (Maybe (Text, TeamId, ManagedBy))
    getGroupMetadataStatement =
      lmap (.toUUID)
        . refineResult (mapM decodeMetadataRow)
        $ [maybeStatement|
         select (name :: text), (team_id :: uuid), (managed_by :: int) from user_group where id = ($1 :: uuid)
         |]

    getGroupMembersStatement :: Statement UserGroupId (Vector UserId)
    getGroupMembersStatement =
      dimap (.toUUID) (fmap Id) $
        [vectorStatement|
          select (user_id :: uuid) from user_group_member where user_group_id = ($1 :: uuid)
          |]

createUserGroupImpl :: (Member (Embed IO) r, Member (Input Pool) r) => TeamId -> NewUserGroup -> ManagedBy -> Sem r UserGroupId
createUserGroupImpl team newUserGroup managedBy = do
  pool <- input
  eitherUuid <- liftIO $ use pool session
  case eitherUuid of
    -- TODO: Deal with this better
    Left err -> error $ show err
    Right uuid -> pure $ Id uuid
  where
    session :: Session UUID
    session = TransactionSession.transaction Transaction.Serializable TransactionSession.Write do
      groupId <- Transaction.statement (newUserGroup.name, team, managedBy) insertGroupStatement
      Transaction.statement (groupId, newUserGroup.members) insertGroupMembersStatement
      pure groupId

    insertGroupStatement :: Statement (Text, TeamId, ManagedBy) UUID
    insertGroupStatement =
      lmap (\(n, t, m) -> (n, t.toUUID, managedByToInt32 m)) $
        [singletonStatement|
         insert into user_group (name, team_id, managed_by) values ($1 :: text, $2 :: uuid, $3 :: int) returning id :: uuid
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
