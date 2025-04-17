{-# LANGUAGE RecordWildCards #-}

module Wire.UserGroupStore.Postgres where

import Data.Id
import Data.UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.UserGroup
import Wire.UserGroupStore

interpretUserGroupStoreToPostgres :: (Member (Embed IO) r, Member (Input Pool) r) => InterpreterFor UserGroupStore r
interpretUserGroupStoreToPostgres =
  interpret $ \case
    CreateUserGroup newUserGroup -> createUserGroupImpl newUserGroup
    GetUserGroup userGroupId -> getUserGroupImpl userGroupId

getUserGroupImpl :: (Member (Embed IO) r, Member (Input Pool) r) => UserGroupId -> Sem r (Maybe UserGroup)
getUserGroupImpl groupId = do
  pool <- input
  eitherUserGroup <- liftIO $ use pool session
  case eitherUserGroup of
    Left err -> error $ show err
    Right g -> pure g
  where
    session :: Session (Maybe UserGroup)
    session = do
      name <- statement groupId.toUUID getGroupNameStatement
      members <- Id <$$> statement groupId.toUUID getGroupMembersStatement
      pure $ UserGroup {..}

    getGroupNameStatement :: Statement UUID (Maybe Text)
    getGroupNameStatement =
      [maybeStatement|
         select (name :: text) from user_group where id = ($1 :: uuid)
         |]
    getGroupMembersStatement :: Statement UUID (Vector UUID)
    getGroupMembersStatement =
      [vectorStatement|
          select (user_id :: uuid) from user_group_member where user_group_id = ($1 :: uuid)
          |]

-- getUserGroupImpl :: UserGroupId -> Sem r (Maybe UserGroup)
-- getUserGroupImpl groupId = do
--   pool <- input
--   eitherUuid <- liftIO $ use pool session
--   case eitherUuid of
--     Left err -> error $ show err
--     Right uuid -> pure $ Id uuid
--   where
--     session :: Session UUID
--     session = statement groupId.toUUID insertStatement

--     insertStatement :: Statement UUID Text
--     insertStatement =
--       [singletonStatement|
--          insert into user_group (name) values ($1 :: text) returning id :: uuid
--          |]

createUserGroupImpl :: (Member (Embed IO) r, Member (Input Pool) r) => NewUserGroup -> Sem r UserGroupId
createUserGroupImpl newUserGroup = do
  pool <- input
  eitherUuid <- liftIO $ use pool session
  case eitherUuid of
    Left err -> error $ show err
    Right uuid -> pure $ Id uuid
  where
    session :: Session UUID
    session = do
      groupId <- statement newUserGroup.name insertGroupStatement
      let pairs = Vector.fromList $ (groupId,) . toUUID <$> newUserGroup.members
      statement (fst <$> pairs, snd <$> pairs) insertGroupMembersStatement
      pure groupId

    insertGroupStatement :: Statement Text UUID
    insertGroupStatement =
      [singletonStatement|
         insert into user_group (name) values ($1 :: text) returning id :: uuid
         |]
    insertGroupMembersStatement :: Statement (Vector UUID, Vector UUID) ()
    insertGroupMembersStatement =
      [singletonStatement|
          insert into user_group_member (user_group_id, user_id) select * from unnest ($1 :: uuid[], $2 :: uuid[])
          |]
