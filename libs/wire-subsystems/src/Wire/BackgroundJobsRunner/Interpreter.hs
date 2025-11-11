{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundJobsRunner.Interpreter where

import Data.ByteString.Conversion (toByteString)
import Data.Default
import Data.Id
import Data.List.NonEmpty (nonEmpty)
import Data.Qualified
import Data.Set qualified as Set
import Data.Singletons
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message (field, msg, val)
import Wire.API.BackgroundJobs
import Wire.API.Conversation (ConversationJoin (..))
import Wire.API.Conversation.Action.Tag
import Wire.API.Conversation.Role (roleNameWireMember)
import Wire.API.Event.Conversation
import Wire.API.Team.HardTruncationLimit (hardTruncationLimit)
import Wire.API.UserGroup
import Wire.BackgroundJobsPublisher
import Wire.BackgroundJobsRunner (BackgroundJobsRunner (..))
import Wire.ConversationStore (ConversationStore, getConversation, upsertMembers)
import Wire.ConversationSubsystem
import Wire.Sem.Random
import Wire.StoredConversation
import Wire.UserGroupStore (UserGroupStore, getUserGroup, getUserGroupChannels)
import Wire.UserList (toUserList)

interpretBackgroundJobsRunner ::
  ( Member UserGroupStore r,
    Member BackgroundJobsPublisher r,
    Member (Input (Local ())) r,
    Member ConversationStore r,
    Member ConversationSubsystem r,
    Member Random r,
    Member TinyLog r
  ) =>
  InterpreterFor BackgroundJobsRunner r
interpretBackgroundJobsRunner = interpret $ \case
  RunJob job -> runJob job

runJob ::
  ( Member UserGroupStore r,
    Member BackgroundJobsPublisher r,
    Member (Input (Local ())) r,
    Member ConversationStore r,
    Member ConversationSubsystem r,
    Member Random r,
    Member TinyLog r
  ) =>
  Job ->
  Sem r ()
runJob job = case job.payload of
  JobSyncUserGroupAndChannel payload -> runSyncUserGroupAndChannel payload
  JobSyncUserGroup payload -> runSyncUserGroup payload

runSyncUserGroupAndChannel ::
  ( Member UserGroupStore r,
    Member (Input (Local ())) r,
    Member ConversationStore r,
    Member ConversationSubsystem r,
    Member TinyLog r
  ) =>
  SyncUserGroupAndChannel ->
  Sem r ()
runSyncUserGroupAndChannel (SyncUserGroupAndChannel {..}) = do
  loc <- input
  mUserGroup <- getUserGroup teamId userGroupId False
  when (isNothing mUserGroup) $
    Log.warn $
      field "team" (toByteString teamId)
        . field "user_group" (toByteString userGroupId)
        . field "conv" (toByteString convId)
        . msg (val "User group not found for sync")
  mConv <- getConversation convId
  when (isNothing mConv) $
    Log.warn $
      field "conv" (toByteString convId)
        . field "team" (toByteString teamId)
        . field "user_group" (toByteString userGroupId)
        . msg (val "Conversation not found for sync")
  for_ mConv $ \conv -> do
    let usersFromGroup = foldMap (runIdentity . (.members)) mUserGroup
        (botMembers, localMembers) = localBotsAndUsers conv.localMembers
        newUsers = V.filter (\u -> u `notElem` fmap (.id_) localMembers) usersFromGroup

    for_ (nonEmpty . V.toList $ newUsers) $ \newUsersUnqualified -> do
      -- FUTUREWORK: Currently legalhold is not compatible with MLS, but to avoid future issues, we need to verify that there is no LH conflict before adding users to a conversation.
      if length conv.localMembers + length newUsersUnqualified > hardTruncationLimit
        then
          -- FUTUREWORK: send an email to the admins of the team notifying them of this
          Log.warn $
            field "conv" (toByteString convId)
              . field "team" (toByteString teamId)
              . field "user_group" (toByteString userGroupId)
              . msg (val "Skipping sync due to member limit")
        else do
          let role = roleNameWireMember
              users = fmap (flip Qualified (tDomain loc)) newUsersUnqualified
              userList = toUserList loc users
              -- Just set it to zeros, so the old clients don't fail to parse
              -- this notification.
              nilId = Qualified (Id UUID.nil) (tDomain loc)
              qActor = flip Qualified (tDomain loc) <$> actor
              notificationFrom = maybe (EventFromSCIM nilId) EventFromUser qActor
              action = ConversationJoin {joinType = ExternalAdd, ..}

          (extraLocals, extraRemotes) <- upsertMembers convId (fmap (,role) userList)
          void $
            notifyConversationAction
              (sing @'ConversationJoinTag)
              notificationFrom
              False
              Nothing
              (loc $> conv)
              (Set.fromList $ fmap (.id_) (localMembers <> extraLocals))
              (Set.fromList $ fmap (.id_) (conv.remoteMembers <> extraRemotes))
              (Set.fromList botMembers)
              action
              def

localBotsAndUsers :: (Foldable f) => f LocalMember -> ([BotMember], [LocalMember])
localBotsAndUsers = foldMap botOrUser
  where
    botOrUser m = case m.service of
      -- we drop invalid bots here, which shouldn't happen
      Just _ -> (toList (newBotMember m), [])
      Nothing -> ([], [m])

runSyncUserGroup ::
  ( Member UserGroupStore r,
    Member BackgroundJobsPublisher r,
    Member Random r,
    Member TinyLog r
  ) =>
  SyncUserGroup ->
  Sem r ()
runSyncUserGroup SyncUserGroup {..} = do
  mChannels <- getUserGroupChannels teamId userGroupId
  when (isNothing mChannels) $
    Log.warn $
      field "team" (toByteString teamId)
        . field "user_group" (toByteString userGroupId)
        . msg (val "No channels found for user group")
  let channels = fromMaybe mempty mChannels
  for_ channels $ \convId -> do
    let syncUserGroupAndChannel = SyncUserGroupAndChannel {..}
    jobId <- newId
    publishJob jobId (JobSyncUserGroupAndChannel syncUserGroupAndChannel)
