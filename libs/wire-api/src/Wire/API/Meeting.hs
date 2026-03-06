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

module Wire.API.Meeting where

import Control.Lens ((?~))
import Data.Id (ConvId, MeetingId, UserId)
import Data.Int qualified as DI
import Data.Json.Util (utcTimeSchema)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified)
import Data.Range (Range)
import Data.Schema
import Data.Time.Clock
import Deriving.Aeson
import Imports
import Wire.API.PostgresMarshall (PostgresMarshall (..), PostgresUnmarshall (..))
import Wire.API.User.Identity (EmailAddress)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

-- | Core Meeting type
data Meeting = Meeting
  { id :: Qualified MeetingId,
    title :: Range 1 256 Text,
    creator :: Qualified UserId,
    startTime :: UTCTime,
    endTime :: UTCTime,
    recurrence :: Maybe Recurrence,
    conversationId :: Qualified ConvId,
    invitedEmails :: [EmailAddress],
    trial :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Meeting)
  deriving (Arbitrary) via (GenericUniform Meeting)

instance ToSchema Meeting where
  schema =
    objectWithDocModifier "Meeting" (description ?~ "A scheduled meeting") $
      Meeting
        <$> (.id) .= field "qualified_id" schema
        <*> (.title) .= field "title" schema
        <*> (.creator) .= field "qualified_creator" schema
        <*> (.startTime) .= field "start_time" utcTimeSchema
        <*> (.endTime) .= field "end_time" utcTimeSchema
        <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
        <*> (.conversationId) .= field "qualified_conversation" schema
        <*> (.invitedEmails) .= field "invited_emails" (array schema)
        <*> (.trial) .= field "trial" schema
        <*> (.createdAt) .= field "created_at" utcTimeSchema
        <*> (.updatedAt) .= field "updated_at" utcTimeSchema

-- | Request to create a new meeting
data NewMeeting = NewMeeting
  { startTime :: UTCTime,
    endTime :: UTCTime,
    recurrence :: Maybe Recurrence,
    title :: Range 1 256 Text,
    invitedEmails :: [EmailAddress]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewMeeting)
  deriving (Arbitrary) via (GenericUniform NewMeeting)

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
    enum @Text "Frequency" $
      mconcat
        [ element "daily" Daily,
          element "weekly" Weekly,
          element "monthly" Monthly,
          element "yearly" Yearly
        ]

instance ToSchema NewMeeting where
  schema =
    objectWithDocModifier "NewMeeting" (description ?~ "Request to create a new meeting") $
      NewMeeting
        <$> (.startTime) .= field "start_time" utcTimeSchema
        <*> (.endTime) .= field "end_time" utcTimeSchema
        <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
        <*> (.title) .= field "title" schema
        <*> (.invitedEmails) .= (fromMaybe [] <$> optField "invited_emails" (array schema))

-- | Request to update an existing meeting
data UpdateMeeting = UpdateMeeting
  { startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    title :: Maybe (Range 1 256 Text),
    -- | 'Just x' means "set 'recurrence' to 'x', meaning set to a value or unset it"
    recurrence :: Maybe (Maybe Recurrence)
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema UpdateMeeting)
  deriving (Arbitrary) via (GenericUniform UpdateMeeting)

instance ToSchema UpdateMeeting where
  schema =
    objectWithDocModifier "UpdateMeeting" (description ?~ "Request to update a meeting") $
      UpdateMeeting
        <$> (.startTime) .= maybe_ (optField "start_time" utcTimeSchema)
        <*> (.endTime) .= maybe_ (optField "end_time" utcTimeSchema)
        <*> (.title) .= maybe_ (optField "title" schema)
        <*> (.recurrence) .= fmap Just (maybe_ (maybe_ (optField' "recurrence" schema)))

instance ToSchema Recurrence where
  schema =
    objectWithDocModifier "Recurrence" (description ?~ "Recurrence pattern for meetings") $
      Recurrence
        <$> (.freq) .= field "frequency" schema
        <*> (.interval) .= (fromMaybe 1 <$> optField "interval" schema)
        <*> (.until) .= maybe_ (optField "until" utcTimeSchema)

-- | Request to add/remove invited email
newtype MeetingEmailsInvitation = MeetingEmailsInvitation
  { emails :: [EmailAddress]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingEmailsInvitation)
  deriving (Arbitrary) via (GenericUniform MeetingEmailsInvitation)

instance ToSchema MeetingEmailsInvitation where
  schema =
    objectWithDocModifier "MeetingEmailsInvitation" (description ?~ "Emails invitation") $
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
