-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Meeting
  ( -- * Time zone
    TimeZone (..),
    timeZoneTZ,
    parseTimeZone,
    renderTimeZone,
    defaultLegacyTimeZone,

    -- * Meeting type
    MeetingType (..),
    renderMeetingType,
    parseMeetingType,

    -- * Meetings (V19)
    Meeting (..),
    MeetingWithConversation (..),
    NewMeeting (..),
    UpdateMeeting (..),

    -- * Legacy meetings (V17/V18)
    MeetingV18 (..),
    MeetingWithConversationV18 (..),
    NewMeetingV18 (..),
    UpdateMeetingV18,
    UpdateMeetingLegacy (..),

    -- * Legacy meetings (V15/V16)
    MeetingV16 (..),
    MeetingWithConversationV16 (..),
    NewMeetingV16 (..),
    UpdateMeetingV16,

    -- * Conversions
    toLegacy,
    fromLegacy,
    toLegacyWithConv,
    fromLegacyNewMeeting,
    toLegacyV18,
    toLegacyWithConvV18,
    fromLegacyNewMeetingV18,
    legacyUpdateToMeeting,

    -- * Misc
    Recurrence (..),
    Frequency (..),
    MeetingEmailsInvitation (..),
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.ByteString.Char8 qualified as BS
import Data.Id (ConvId, MeetingId, UserId)
import Data.Int qualified as DI
import Data.Json.Util (utcTimeSchema)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified)
import Data.Range (Range)
import Data.Schema
import Data.Text qualified as Text
import Data.Time.Clock
import Data.Time.Zones.All (TZLabel (..), fromTZName, toTZName, tzByLabel)
import Data.Time.Zones.Types (TZ)
import Imports
import Test.QuickCheck (elements)
import Wire.API.Conversation (Conversation, GroupConvType)
import Wire.API.PostgresMarshall (PostgresMarshall (..), PostgresUnmarshall (..))
import Wire.API.User.Identity (EmailAddress)
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

-- | An IANA time zone identifier (e.g. @"Europe/Paris"@), backed by the @tz@
-- package's 'TZLabel'. The loaded 'TZ' is recovered purely via 'timeZoneTZ';
-- 'TZLabel' itself is what is serialized to JSON and Postgres.
newtype TimeZone = TimeZone {timeZoneLabel :: TZLabel}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Bounded, Enum)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TimeZone)

timeZoneTZ :: TimeZone -> TZ
timeZoneTZ = tzByLabel . timeZoneLabel

parseTimeZone :: Text -> Maybe TimeZone
parseTimeZone = fmap TimeZone . fromTZName . BS.pack . Text.unpack

renderTimeZone :: TimeZone -> Text
renderTimeZone = Text.pack . BS.unpack . toTZName . timeZoneLabel

-- | Default for legacy operations (helm @meetings.legacyTimeZone@).
defaultLegacyTimeZone :: TimeZone
defaultLegacyTimeZone = TimeZone Europe__Berlin

instance ToSchema TimeZone where
  schema =
    renderTimeZone
      .= parsedText "TimeZone" (maybe (Left "invalid IANA tzid") Right . parseTimeZone)

instance Arbitrary TimeZone where
  arbitrary = TimeZone <$> elements [minBound .. maxBound]

-- | A V17/V18 meeting (no @type@ field; treated as @scheduled@).
-- @end_time@ is the source of truth (there is no @duration@ field); the
-- @tzid@ field carries the IANA time zone.
data MeetingV18 = MeetingV18
  { id :: Qualified MeetingId,
    title :: Range 1 256 Text,
    creator :: Qualified UserId,
    startTime :: UTCTime,
    endTime :: UTCTime,
    tzid :: TimeZone,
    recurrence :: Maybe Recurrence,
    conversationId :: Qualified ConvId,
    invitedEmails :: [EmailAddress],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingV18)
  deriving (Arbitrary) via (GenericUniform MeetingV18)

-- | A meeting (V19 and later; @immediate@ or @scheduled@). @end_time@ is the
-- source of truth (there is no @duration@ field); the @tzid@ field carries
-- the IANA time zone.
data Meeting = Meeting
  { id :: Qualified MeetingId,
    title :: Range 1 256 Text,
    creator :: Qualified UserId,
    startTime :: UTCTime,
    endTime :: UTCTime,
    tzid :: TimeZone,
    mtype :: MeetingType,
    recurrence :: Maybe Recurrence,
    conversationId :: Qualified ConvId,
    invitedEmails :: [EmailAddress],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Meeting)
  deriving (Arbitrary) via (GenericUniform Meeting)

-- | A legacy meeting (V15/V16). Carries @end_time@ (the source of truth) but
-- has no @tzid@ field; the deprecated @trial@ field is injected (always
-- @false@) in the 'ToSchema' instance and is never stored.
data MeetingV16 = MeetingV16
  { id :: Qualified MeetingId,
    title :: Range 1 256 Text,
    creator :: Qualified UserId,
    startTime :: UTCTime,
    endTime :: UTCTime,
    recurrence :: Maybe Recurrence,
    conversationId :: Qualified ConvId,
    invitedEmails :: [EmailAddress],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingV16)
  deriving (Arbitrary) via (GenericUniform MeetingV16)

-- | V17/V18 object schema. Carries @end_time@ directly (the source of truth)
-- and the @tzid@ field.
meetingV18Object :: ObjectSchema SwaggerDoc MeetingV18
meetingV18Object =
  MeetingV18
    <$> (.id) .= field "qualified_id" schema
    <*> (.title) .= field "title" schema
    <*> (.creator) .= field "qualified_creator" schema
    <*> (.startTime) .= field "start_time" utcTimeSchema
    <*> (.endTime) .= field "end_time" utcTimeSchema
    <*> (.tzid) .= field "tzid" schema
    <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
    <*> (.conversationId) .= field "qualified_conversation" schema
    <*> (.invitedEmails) .= field "invited_emails" (array schema)
    <*> (.createdAt) .= field "created_at" utcTimeSchema
    <*> (.updatedAt) .= field "updated_at" utcTimeSchema

instance ToSchema MeetingV18 where
  schema = objectWithDocModifier (description ?~ "A scheduled meeting") meetingV18Object

-- | V19 object schema. Like the V17/V18 schema plus the required @type@ field.
meetingObject :: ObjectSchema SwaggerDoc Meeting
meetingObject =
  Meeting
    <$> (.id) .= field "qualified_id" schema
    <*> (.title) .= field "title" schema
    <*> (.creator) .= field "qualified_creator" schema
    <*> (.startTime) .= field "start_time" utcTimeSchema
    <*> (.endTime) .= field "end_time" utcTimeSchema
    <*> (.tzid) .= field "tzid" schema
    <*> (.mtype) .= field "type" schema
    <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
    <*> (.conversationId) .= field "qualified_conversation" schema
    <*> (.invitedEmails) .= field "invited_emails" (array schema)
    <*> (.createdAt) .= field "created_at" utcTimeSchema
    <*> (.updatedAt) .= field "updated_at" utcTimeSchema

instance ToSchema Meeting where
  schema = objectWithDocModifier (description ?~ "A meeting (immediate or scheduled)") meetingObject

-- | V16 (V15/V16) object schema. Keeps @end_time@ and appends the always-false
-- @trial@ field (never stored).
meetingV16Object :: ObjectSchema SwaggerDoc MeetingV16
meetingV16Object =
  MeetingV16
    <$> (.id) .= field "qualified_id" schema
    <*> (.title) .= field "title" schema
    <*> (.creator) .= field "qualified_creator" schema
    <*> (.startTime) .= field "start_time" utcTimeSchema
    <*> (.endTime) .= field "end_time" utcTimeSchema
    <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
    <*> (.conversationId) .= field "qualified_conversation" schema
    <*> (.invitedEmails) .= field "invited_emails" (array schema)
    <*> (.createdAt) .= field "created_at" utcTimeSchema
    <*> (.updatedAt) .= field "updated_at" utcTimeSchema
    <* ( const ()
           .= fieldWithDocModifier
             "trial"
             (description ?~ "Deprecated. Always false; team meetings are never trial.")
             (c (False :: Bool))
       )
  where
    -- Constant schema that always encodes @val@ and decodes to @()@, cf. the
    -- @managed@ field of 'Wire.API.Conversation.ConvTeamInfo'.
    c :: (ToJSON a) => a -> ValueSchema SwaggerDoc ()
    c val = mkSchema mempty (const (pure ())) (const (pure (toJSON val)))

instance ToSchema MeetingV16 where
  schema = objectWithDocModifier (description ?~ "A scheduled meeting") meetingV16Object

-- | A 'MeetingV18' extended with the full 'Conversation' associated with it,
-- as returned by the V17/V18 endpoints.
data MeetingWithConversationV18 = MeetingWithConversationV18
  { meeting :: MeetingV18,
    conversation :: Conversation GroupConvType
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingWithConversationV18)
  deriving (Arbitrary) via (GenericUniform MeetingWithConversationV18)

-- | A 'Meeting' extended with the full 'Conversation' associated with it, as
-- returned when creating or updating a meeting. The underlying meeting fields
-- are flattened into the JSON object (emitted alongside @conversation@).
data MeetingWithConversation = MeetingWithConversation
  { meeting :: Meeting,
    conversation :: Conversation GroupConvType
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingWithConversation)
  deriving (Arbitrary) via (GenericUniform MeetingWithConversation)

-- | The V17/V18 meeting object is flattened into the parent object (its
-- fields are emitted alongside @conversation@ rather than nested).
meetingWithConversationV18Object :: ObjectSchema SwaggerDoc MeetingWithConversationV18
meetingWithConversationV18Object =
  MeetingWithConversationV18
    <$> (.meeting) .= meetingV18Object
    <*> (.conversation) .= field "conversation" schema

instance ToSchema MeetingWithConversationV18 where
  schema =
    objectWithDocModifier
      (description ?~ "A scheduled meeting with its associated conversation")
      meetingWithConversationV18Object

-- | The V19 meeting object is flattened into the parent object (its fields
-- are emitted alongside @conversation@ rather than nested).
meetingWithConversationObject :: ObjectSchema SwaggerDoc MeetingWithConversation
meetingWithConversationObject =
  MeetingWithConversation
    <$> (.meeting) .= meetingObject
    <*> (.conversation) .= field "conversation" schema

instance ToSchema MeetingWithConversation where
  schema =
    objectWithDocModifier
      (description ?~ "A meeting (immediate or scheduled) with its associated conversation")
      meetingWithConversationObject

data MeetingWithConversationV16 = MeetingWithConversationV16
  { meeting :: MeetingV16,
    conversation :: Conversation GroupConvType
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingWithConversationV16)
  deriving (Arbitrary) via (GenericUniform MeetingWithConversationV16)

meetingWithConversationV16Object :: ObjectSchema SwaggerDoc MeetingWithConversationV16
meetingWithConversationV16Object =
  MeetingWithConversationV16
    <$> (.meeting) .= meetingV16Object
    <*> (.conversation) .= field "conversation" schema

instance ToSchema MeetingWithConversationV16 where
  schema =
    objectWithDocModifier
      (description ?~ "A scheduled meeting with its associated conversation")
      meetingWithConversationV16Object

-- | Request to create a new meeting (V17/V18). Carries @end_time@ (the
-- source of truth) and the @tzid@ field.
data NewMeetingV18 = NewMeetingV18
  { startTime :: UTCTime,
    endTime :: UTCTime,
    tzid :: TimeZone,
    recurrence :: Maybe Recurrence,
    title :: Range 1 256 Text,
    invitedEmails :: [EmailAddress]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewMeetingV18)
  deriving (Arbitrary) via (GenericUniform NewMeetingV18)

-- | Request to create a new meeting (V19 and later). Like the V17/V18 shape
-- plus the required @type@ field.
data NewMeeting = NewMeeting
  { startTime :: UTCTime,
    endTime :: UTCTime,
    tzid :: TimeZone,
    mtype :: MeetingType,
    recurrence :: Maybe Recurrence,
    title :: Range 1 256 Text,
    invitedEmails :: [EmailAddress]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewMeeting)
  deriving (Arbitrary) via (GenericUniform NewMeeting)

instance ToSchema NewMeetingV18 where
  schema =
    objectWithDocModifier (description ?~ "Request to create a new meeting") $
      NewMeetingV18
        <$> (.startTime) .= field "start_time" utcTimeSchema
        <*> (.endTime) .= field "end_time" utcTimeSchema
        <*> (.tzid) .= field "tzid" schema
        <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
        <*> (.title) .= field "title" schema
        <*> (.invitedEmails) .= (fromMaybe [] <$> optField "invited_emails" (array schema))

instance ToSchema NewMeeting where
  schema =
    objectWithDocModifier (description ?~ "Request to create a new meeting") $
      NewMeeting
        <$> (.startTime) .= field "start_time" utcTimeSchema
        <*> (.endTime) .= field "end_time" utcTimeSchema
        <*> (.tzid) .= field "tzid" schema
        <*> (.mtype) .= field "type" schema
        <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
        <*> (.title) .= field "title" schema
        <*> (.invitedEmails) .= (fromMaybe [] <$> optField "invited_emails" (array schema))

-- | Request to create a new meeting (V15/V16). Carries @end_time@ but no @tzid@.
data NewMeetingV16 = NewMeetingV16
  { startTime :: UTCTime,
    endTime :: UTCTime,
    recurrence :: Maybe Recurrence,
    title :: Range 1 256 Text,
    invitedEmails :: [EmailAddress]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewMeetingV16)
  deriving (Arbitrary) via (GenericUniform NewMeetingV16)

instance ToSchema NewMeetingV16 where
  schema =
    objectWithDocModifier (description ?~ "Request to create a new meeting (V16)") $
      NewMeetingV16
        <$> (.startTime) .= field "start_time" utcTimeSchema
        <*> (.endTime) .= field "end_time" utcTimeSchema
        <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
        <*> (.title) .= field "title" schema
        <*> (.invitedEmails) .= (fromMaybe [] <$> optField "invited_emails" (array schema))

data Recurrence = Recurrence
  { -- | The interval between occurrences, e.g., every 2 weeks for Weekly frequency with interval=2
    freq :: Frequency,
    interval :: Int,
    until :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Recurrence)
  deriving (Arbitrary) via (GenericUniform Recurrence)

data Frequency = Daily | Weekly | Monthly | Yearly
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Frequency)
  deriving (Arbitrary) via (GenericUniform Frequency)

instance ToSchema Frequency where
  schema =
    enum @Text $
      mconcat
        [ element "daily" Daily,
          element "weekly" Weekly,
          element "monthly" Monthly,
          element "yearly" Yearly
        ]

-- | Whether a meeting is @immediate@ or @scheduled@. Exposed at API version
-- V19 and later; pre-V19 meetings are always @scheduled@.
data MeetingType = Immediate | Scheduled
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema MeetingType)
  deriving (Arbitrary) via (GenericUniform MeetingType)

instance ToSchema MeetingType where
  schema =
    enum @Text $
      mconcat
        [ element "immediate" Immediate,
          element "scheduled" Scheduled
        ]

renderMeetingType :: MeetingType -> Text
renderMeetingType = \case
  Immediate -> "immediate"
  Scheduled -> "scheduled"

parseMeetingType :: Text -> Maybe MeetingType
parseMeetingType = \case
  "immediate" -> Just Immediate
  "scheduled" -> Just Scheduled
  _ -> Nothing

-- | Request to update an existing meeting (V19). @tzid@ is optional: 'Just'
-- sets the meeting's IANA time zone, while omitting it leaves the stored time
-- zone unchanged; @end_time@ is optional. @type@ is optional: omitting it
-- leaves the stored meeting type unchanged.
data UpdateMeeting = UpdateMeeting
  { startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    title :: Maybe (Range 1 256 Text),
    -- | 'Just x' means "set 'recurrence' to 'x', meaning set to a value or unset it"
    recurrence :: Maybe (Maybe Recurrence),
    tzid :: Maybe TimeZone,
    mtype :: Maybe MeetingType
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema UpdateMeeting)
  deriving (Arbitrary) via (GenericUniform UpdateMeeting)

instance ToSchema UpdateMeeting where
  schema =
    objectWithDocModifier (description ?~ "Request to update a meeting") $
      UpdateMeeting
        <$> (.startTime) .= maybe_ (optField "start_time" utcTimeSchema)
        <*> (.endTime) .= maybe_ (optField "end_time" utcTimeSchema)
        <*> (.title) .= maybe_ (optField "title" schema)
        <*> (.recurrence) .= fmap Just (maybe_ (maybe_ (optField' "recurrence" schema)))
        <*> (.tzid) .= maybe_ (optField "tzid" schema)
        <*> (.mtype) .= maybe_ (optField "type" schema)

-- | Request to update an existing meeting on the frozen V15-V18 endpoints.
-- Identical to 'UpdateMeeting' except it carries no @type@ field, so legacy
-- clients cannot change the stored meeting type.
data UpdateMeetingLegacy = UpdateMeetingLegacy
  { startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    title :: Maybe (Range 1 256 Text),
    recurrence :: Maybe (Maybe Recurrence),
    tzid :: Maybe TimeZone
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema UpdateMeetingLegacy)
  deriving (Arbitrary) via (GenericUniform UpdateMeetingLegacy)

type UpdateMeetingV16 = UpdateMeetingLegacy

type UpdateMeetingV18 = UpdateMeetingLegacy

instance ToSchema UpdateMeetingLegacy where
  schema =
    objectWithDocModifier (description ?~ "Request to update a meeting") $
      UpdateMeetingLegacy
        <$> (.startTime) .= maybe_ (optField "start_time" utcTimeSchema)
        <*> (.endTime) .= maybe_ (optField "end_time" utcTimeSchema)
        <*> (.title) .= maybe_ (optField "title" schema)
        <*> (.recurrence) .= fmap Just (maybe_ (maybe_ (optField' "recurrence" schema)))
        <*> (.tzid) .= maybe_ (optField "tzid" schema)

-- | Map a legacy update request onto the V19 shape. @mtype@ is always
-- 'Nothing', i.e. the stored meeting type is left unchanged.
legacyUpdateToMeeting :: UpdateMeetingLegacy -> UpdateMeeting
legacyUpdateToMeeting u =
  UpdateMeeting
    { startTime = u.startTime,
      endTime = u.endTime,
      title = u.title,
      recurrence = u.recurrence,
      tzid = u.tzid,
      mtype = Nothing
    }

instance ToSchema Recurrence where
  schema =
    objectWithDocModifier (description ?~ "Recurrence pattern for meetings") $
      Recurrence
        <$> (.freq) .= field "frequency" schema
        <*> (.interval) .= (fromMaybe 1 <$> optField "interval" schema)
        <*> (.until) .= maybe_ (optField "until" utcTimeSchema)

-- | Convert a V19 'Meeting' to the legacy 'MeetingV16' shape. Fields are
-- copied verbatim; @tzid@ and @mtype@ are dropped (@end_time@ is preserved,
-- so no duration needs to be recomputed).
toLegacy :: Meeting -> MeetingV16
toLegacy m =
  MeetingV16
    { id = m.id,
      title = m.title,
      creator = m.creator,
      startTime = m.startTime,
      endTime = m.endTime,
      recurrence = m.recurrence,
      conversationId = m.conversationId,
      invitedEmails = m.invitedEmails,
      createdAt = m.createdAt,
      updatedAt = m.updatedAt
    }

-- | Convert a legacy 'MeetingV16' to the V19 'Meeting' shape, injecting the
-- given 'TimeZone' as @tzid@ and defaulting @mtype@ to 'Scheduled'. All other
-- fields (including @end_time@) are preserved.
fromLegacy :: TimeZone -> MeetingV16 -> Meeting
fromLegacy tz m =
  Meeting
    { id = m.id,
      title = m.title,
      creator = m.creator,
      startTime = m.startTime,
      endTime = m.endTime,
      tzid = tz,
      mtype = Scheduled,
      recurrence = m.recurrence,
      conversationId = m.conversationId,
      invitedEmails = m.invitedEmails,
      createdAt = m.createdAt,
      updatedAt = m.updatedAt
    }

-- | 'toLegacy' lifted over 'MeetingWithConversation'.
toLegacyWithConv :: MeetingWithConversation -> MeetingWithConversationV16
toLegacyWithConv mwc =
  MeetingWithConversationV16 {meeting = toLegacy mwc.meeting, conversation = mwc.conversation}

-- | Convert a V19 'Meeting' to the legacy V18 'MeetingV18' shape (drops @mtype@).
toLegacyV18 :: Meeting -> MeetingV18
toLegacyV18 m =
  MeetingV18
    { id = m.id,
      title = m.title,
      creator = m.creator,
      startTime = m.startTime,
      endTime = m.endTime,
      tzid = m.tzid,
      recurrence = m.recurrence,
      conversationId = m.conversationId,
      invitedEmails = m.invitedEmails,
      createdAt = m.createdAt,
      updatedAt = m.updatedAt
    }

-- | 'toLegacyV18' lifted over 'MeetingWithConversation'.
toLegacyWithConvV18 :: MeetingWithConversation -> MeetingWithConversationV18
toLegacyWithConvV18 mwc =
  MeetingWithConversationV18 {meeting = toLegacyV18 mwc.meeting, conversation = mwc.conversation}

-- | Convert a V16 'NewMeetingV16' to the V19 'NewMeeting', injecting the
-- given 'TimeZone' as @tzid@ and defaulting @mtype@ to 'Scheduled'.
-- @end_time@ is preserved (it is the source of truth).
fromLegacyNewMeeting :: TimeZone -> NewMeetingV16 -> NewMeeting
fromLegacyNewMeeting tz nm =
  NewMeeting
    { startTime = nm.startTime,
      endTime = nm.endTime,
      tzid = tz,
      mtype = Scheduled,
      recurrence = nm.recurrence,
      title = nm.title,
      invitedEmails = nm.invitedEmails
    }

-- | Convert a V17/V18 'NewMeetingV18' to the V19 'NewMeeting', defaulting
-- @mtype@ to 'Scheduled' (no time zone injection; V17/V18 requests carry
-- @tzid@).
fromLegacyNewMeetingV18 :: NewMeetingV18 -> NewMeeting
fromLegacyNewMeetingV18 nm =
  NewMeeting
    { startTime = nm.startTime,
      endTime = nm.endTime,
      tzid = nm.tzid,
      mtype = Scheduled,
      recurrence = nm.recurrence,
      title = nm.title,
      invitedEmails = nm.invitedEmails
    }

-- | Request to add/remove invited email
newtype MeetingEmailsInvitation = MeetingEmailsInvitation
  { emails :: [EmailAddress]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingEmailsInvitation)
  deriving (Arbitrary) via (GenericUniform MeetingEmailsInvitation)

instance ToSchema MeetingEmailsInvitation where
  schema =
    objectWithDocModifier (description ?~ "Emails invitation") $
      MeetingEmailsInvitation
        <$> (.emails) .= field "emails" (array schema)

instance PostgresMarshall (Maybe Text, Maybe DI.Int32, Maybe UTCTime) (Maybe Recurrence) where
  postgresMarshall Nothing = (Nothing, Nothing, Nothing)
  postgresMarshall (Just r) =
    ( Just $ case r.freq of
        Daily -> "daily"
        Weekly -> "weekly"
        Monthly -> "monthly"
        Yearly -> "yearly",
      Just (fromIntegral r.interval),
      r.until
    )

instance PostgresUnmarshall (Maybe Text, Maybe DI.Int32, Maybe UTCTime) (Maybe Recurrence) where
  postgresUnmarshall (Nothing, _, _) = Right Nothing
  postgresUnmarshall (Just f, Just i, u) = do
    freq <- case f of
      "daily" -> Right Daily
      "weekly" -> Right Weekly
      "monthly" -> Right Monthly
      "yearly" -> Right Yearly
      _ -> Left $ "Unknown frequency: " <> f
    pure . Just $
      Recurrence
        { freq = freq,
          interval = fromIntegral i,
          until = u
        }
  postgresUnmarshall (Just _, Nothing, _) = Left "Missing interval for recurrence"
