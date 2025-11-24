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
import Data.Aeson ()
import Data.Id (ConvId, UserId, uuidSchema)
import Data.Json.Util (utcTimeSchema)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified, qualifiedSchema)
import Data.Schema
import Data.Time.Clock
import Data.UUID (UUID)
import Deriving.Aeson
import Imports
import Servant (FromHttpApiData, ToHttpApiData)
import Wire.API.User.Identity (EmailAddress)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

-- | Unique identifier for a meeting
newtype MeetingId = MeetingId {unMeetingId :: UUID}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, S.ToSchema, S.ToParamSchema)
  deriving (Arbitrary) via (GenericUniform MeetingId)

instance ToSchema MeetingId where
  schema = MeetingId <$> unMeetingId .= uuidSchema

instance ToSchema (Qualified MeetingId) where
  schema = qualifiedSchema "MeetingId" "id" schema

-- | Core Meeting type
data Meeting = Meeting
  { id :: Qualified MeetingId,
    title :: Text,
    creator :: Qualified UserId,
    startDate :: UTCTime,
    endDate :: UTCTime,
    recurrence :: Maybe Recurrence,
    conversationId :: Qualified ConvId,
    invitedEmails :: [EmailAddress],
    trial :: Bool
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
        <*> (.startDate) .= field "start_date" utcTimeSchema
        <*> (.endDate) .= field "end_date" utcTimeSchema
        <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
        <*> (.conversationId) .= field "qualified_conversation" schema
        <*> (.invitedEmails) .= field "invited_emails" (array schema)
        <*> (.trial) .= field "trial" schema

-- | Request to create a new meeting
data NewMeeting = NewMeeting
  { startDate :: UTCTime,
    endDate :: UTCTime,
    recurrence :: Maybe Recurrence,
    title :: Text,
    invitedEmails :: [EmailAddress]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewMeeting)
  deriving (Arbitrary) via (GenericUniform NewMeeting)

data Recurrence = Recurrence
  { freq :: Frequency,
    interval :: Maybe Int,
    until :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Recurrence)
  deriving (Arbitrary) via (GenericUniform Recurrence)

data Frequency = Daily | Weekly | Monthly | Yearly
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Frequency)
  deriving (Arbitrary) via (GenericUniform Frequency)

instance ToSchema NewMeeting where
  schema =
    objectWithDocModifier "NewMeeting" (description ?~ "Request to create a new meeting") $
      NewMeeting
        <$> (.startDate) .= field "start_date" utcTimeSchema
        <*> (.endDate) .= field "end_date" utcTimeSchema
        <*> (.recurrence) .= maybe_ (optField "recurrence" schema)
        <*> (.title) .= field "title" schema
        <*> (.invitedEmails) .= (fromMaybe [] <$> optField "invited_emails" (array schema))

-- | Request to update an existing meeting
data UpdateMeeting = UpdateMeeting
  { startDate :: Maybe UTCTime,
    endDate :: Maybe UTCTime,
    title :: Maybe Text,
    recurrence :: Maybe Recurrence
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema UpdateMeeting)
  deriving (Arbitrary) via (GenericUniform UpdateMeeting)

instance ToSchema UpdateMeeting where
  schema =
    objectWithDocModifier "UpdateMeeting" (description ?~ "Request to update a meeting") $
      UpdateMeeting
        <$> (.startDate) .= maybe_ (optField "start_date" utcTimeSchema)
        <*> (.endDate) .= maybe_ (optField "end_date" utcTimeSchema)
        <*> (.title) .= maybe_ (optField "title" schema)
        <*> (.recurrence) .= maybe_ (optField "recurrence" schema)

instance ToSchema Frequency where
  schema =
    enum @Text "Frequency" $
      mconcat
        [ element "Daily" Daily,
          element "Weekly" Weekly,
          element "Monthly" Monthly,
          element "Yearly" Yearly
        ]

instance ToSchema Recurrence where
  schema =
    objectWithDocModifier "Recurrence" (description ?~ "Recurrence pattern for meetings") $
      Recurrence
        <$> (.freq) .= field "frequency" schema
        <*> (.interval) .= maybe_ (optField "interval" schema)
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
