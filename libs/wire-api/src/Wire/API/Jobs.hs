{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Jobs where

import Control.Arrow ((&&&))
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema
import Data.Text as Text
import GHC.TypeLits
import Imports
import Test.QuickCheck (oneof)
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

-- | The queue/table for jobs that operate on meetings.
type MeetingsQueueName = "meetings"

meetingsQueueName :: Text
meetingsQueueName = Text.pack $ symbolVal (Proxy @MeetingsQueueName)

-- | The queue/table for jobs that operate on conversations.
type ConversationsQueueName = "conversations"

conversationsQueueName :: Text
conversationsQueueName = Text.pack $ symbolVal (Proxy @ConversationsQueueName)

-- | Empty payload because the schedule itself carries all execution context.
data MeetingsCleanupJob = MeetingsCleanupJob
  deriving stock (Eq, Generic, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingsCleanupJob)

instance ToSchema MeetingsCleanupJob where
  schema = object $ pure MeetingsCleanupJob

instance Arbitrary MeetingsCleanupJob where
  arbitrary = pure MeetingsCleanupJob

-- | Payload for adminless deletions.
-- Arbiter persists these payloads and workers decode them later, so changes to
-- field names or shapes require a coordinated rollout. The origin user is
-- optional for jobs created by system reconciliation; the request ID is always
-- captured when a job is scheduled.
data AdminlessDeletionJob = AdminlessDeletionJob
  { adminlessDeletionJobTeamId :: TeamId,
    adminlessDeletionJobConversationId :: ConvId,
    adminlessDeletionJobOrigUserId :: Maybe UserId,
    adminlessDeletionJobRequestId :: RequestId
  }
  deriving stock (Eq, Generic, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema AdminlessDeletionJob)

instance Arbitrary AdminlessDeletionJob where
  arbitrary = AdminlessDeletionJob <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToSchema AdminlessDeletionJob where
  schema =
    object $
      AdminlessDeletionJob
        <$> (.adminlessDeletionJobTeamId) .= field "team_id" schema
        <*> (.adminlessDeletionJobConversationId) .= field "conversation_id" schema
        <*> (.adminlessDeletionJobOrigUserId) .= maybe_ (optField "orig_user_id" schema)
        <*> (.adminlessDeletionJobRequestId) .= field "request_id" schema

-- | Payload for adminless reminders.
-- Arbiter persists these payloads and workers decode them later, so changes to
-- field names or shapes require a coordinated rollout. The origin user is
-- optional for jobs created by system reconciliation; the request ID is always
-- captured when a job is scheduled.
data AdminlessReminderJob = AdminlessReminderJob
  { adminlessReminderJobTeamId :: TeamId,
    adminlessReminderJobConversationId :: ConvId,
    adminlessReminderJobOrigUserId :: Maybe UserId,
    adminlessReminderJobDeletionScheduledFor :: UTCTimeMillis,
    adminlessReminderJobRequestId :: RequestId
  }
  deriving stock (Eq, Generic, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema AdminlessReminderJob)

instance Arbitrary AdminlessReminderJob where
  arbitrary = AdminlessReminderJob <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToSchema AdminlessReminderJob where
  schema =
    object $
      AdminlessReminderJob
        <$> (.adminlessReminderJobTeamId) .= field "team_id" schema
        <*> (.adminlessReminderJobConversationId) .= field "conversation_id" schema
        <*> (.adminlessReminderJobOrigUserId) .= maybe_ (optField "orig_user_id" schema)
        <*> (.adminlessReminderJobDeletionScheduledFor) .= field "deletion_scheduled_for" schema
        <*> (.adminlessReminderJobRequestId) .= field "request_id" schema

-- | Common representation for all queue payload envelopes.
-- The queue-specific sum supplies the type tag and its associated data schema,
-- while this helper guarantees the stable {"type": ..., "data": ...} shape.
taggedJobPayloadObjectSchema ::
  forall tag payload.
  (Bounded tag, Enum tag, ToSchema tag) =>
  (payload -> tag) ->
  (tag -> ObjectSchema SwaggerDoc payload) ->
  ObjectSchema SwaggerDoc payload
taggedJobPayloadObjectSchema toTag toSchema =
  snd <$> (toTag &&& id) .= bind (fst .= tagObjectSchema) (snd .= dispatch toSchema)
  where
    tagObjectSchema :: ObjectSchema SwaggerDoc tag
    tagObjectSchema = field "type" schema

-- | Payload persisted in the meetings queue. Keep the type tag and nested data
-- shape stable when changing scheduled jobs. The sum makes the queue
-- extensible without changing its JSON envelope.
data MeetingsJobPayload
  = MeetingsCleanup MeetingsCleanupJob
  deriving stock (Eq, Generic, Show)

data MeetingsJobPayloadTag
  = MeetingsCleanupTag
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via GenericUniform MeetingsJobPayloadTag

instance ToSchema MeetingsJobPayloadTag where
  schema =
    enum @Text $
      element "meetings_cleanup" MeetingsCleanupTag

makePrisms ''MeetingsJobPayload

meetingsJobPayloadObjectSchema :: ObjectSchema SwaggerDoc MeetingsJobPayload
meetingsJobPayloadObjectSchema = taggedJobPayloadObjectSchema toTag toSchema
  where
    toTag :: MeetingsJobPayload -> MeetingsJobPayloadTag
    toTag =
      \case
        MeetingsCleanup {} -> MeetingsCleanupTag

    toSchema :: MeetingsJobPayloadTag -> ObjectSchema SwaggerDoc MeetingsJobPayload
    toSchema = \case
      MeetingsCleanupTag -> tag _MeetingsCleanup (field "data" schema)

instance ToSchema MeetingsJobPayload where
  schema = object meetingsJobPayloadObjectSchema

deriving via (Schema MeetingsJobPayload) instance FromJSON MeetingsJobPayload

deriving via (Schema MeetingsJobPayload) instance ToJSON MeetingsJobPayload

deriving via (Schema MeetingsJobPayload) instance S.ToSchema MeetingsJobPayload

instance Arbitrary MeetingsJobPayload where
  arbitrary = MeetingsCleanup <$> arbitrary

-- | Payload persisted in the conversations queue. Keep the type tags and
-- nested data shapes stable when changing scheduled jobs.
data ConversationsJobPayload
  = AdminlessDeletion AdminlessDeletionJob
  | AdminlessReminder AdminlessReminderJob
  deriving stock (Eq, Generic, Show)

data ConversationsJobPayloadTag
  = AdminlessReminderTag
  | AdminlessDeletionTag
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via GenericUniform ConversationsJobPayloadTag

instance ToSchema ConversationsJobPayloadTag where
  schema =
    enum @Text $
      mconcat
        [ element "adminless_deletion" AdminlessDeletionTag,
          element "adminless_reminder" AdminlessReminderTag
        ]

makePrisms ''ConversationsJobPayload

conversationsJobPayloadObjectSchema :: ObjectSchema SwaggerDoc ConversationsJobPayload
conversationsJobPayloadObjectSchema = taggedJobPayloadObjectSchema toTag toSchema
  where
    toTag :: ConversationsJobPayload -> ConversationsJobPayloadTag
    toTag =
      \case
        AdminlessDeletion {} -> AdminlessDeletionTag
        AdminlessReminder {} -> AdminlessReminderTag

    toSchema :: ConversationsJobPayloadTag -> ObjectSchema SwaggerDoc ConversationsJobPayload
    toSchema = \case
      AdminlessDeletionTag -> tag _AdminlessDeletion (field "data" schema)
      AdminlessReminderTag -> tag _AdminlessReminder (field "data" schema)

instance ToSchema ConversationsJobPayload where
  schema = object conversationsJobPayloadObjectSchema

deriving via (Schema ConversationsJobPayload) instance FromJSON ConversationsJobPayload

deriving via (Schema ConversationsJobPayload) instance ToJSON ConversationsJobPayload

deriving via (Schema ConversationsJobPayload) instance S.ToSchema ConversationsJobPayload

instance Arbitrary ConversationsJobPayload where
  arbitrary = oneof [AdminlessDeletion <$> arbitrary, AdminlessReminder <$> arbitrary]

-- | Registry for the jobs we expose via Arbiter.
type JobRegistry =
  '[ '(MeetingsQueueName, MeetingsJobPayload),
     '(ConversationsQueueName, ConversationsJobPayload)
   ]
