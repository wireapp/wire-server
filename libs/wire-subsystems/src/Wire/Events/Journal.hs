{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Events.Journal
  ( userActivateJournal,
    userUpdateJournal,
    userDeleteJournal,
    userEmailRemoveJournal,
    journalEvent,
    journalUserEvent,
  )
where

import Control.Lens
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.Id
import Data.Proto
import Data.Proto.Id
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding (encodeMessage)
import Data.UUID.V4 (nextRandom)
import Imports
import Polysemy
import Proto.UserEvents (UserEvent'EventType (..))
import Proto.UserEvents qualified as Proto
import Proto.UserEvents_Fields qualified as U
import Wire.API.User
import Wire.API.UserEvent qualified as API
import Wire.AWSSubsystem qualified as AWS

-- | Journal a user activation event
userActivateJournal ::
  ( Member AWS.AWSSubsystem r,
    Member (Embed IO) r
  ) =>
  User ->
  Sem r ()
userActivateJournal u@User {..} =
  journalEvent UserEvent'USER_ACTIVATE (userId u) (userEmail u) (Just userLocale) userTeam (Just userDisplayName)

-- | Journal a user update event
userUpdateJournal ::
  ( Member AWS.AWSSubsystem r,
    Member (Embed IO) r
  ) =>
  UserId ->
  Maybe EmailAddress ->
  Maybe Locale ->
  Maybe Name ->
  Sem r ()
userUpdateJournal uid em loc =
  journalEvent UserEvent'USER_UPDATE uid em loc Nothing

-- | Journal a user email removal event
userEmailRemoveJournal ::
  ( Member AWS.AWSSubsystem r,
    Member (Embed IO) r
  ) =>
  UserId ->
  EmailAddress ->
  Sem r ()
userEmailRemoveJournal uid em =
  journalEvent UserEvent'USER_EMAIL_REMOVE uid (Just em) Nothing Nothing Nothing

-- | Journal a user deletion event
userDeleteJournal ::
  ( Member AWS.AWSSubsystem r,
    Member (Embed IO) r
  ) =>
  UserId ->
  Sem r ()
userDeleteJournal uid =
  journalEvent UserEvent'USER_DELETE uid Nothing Nothing Nothing Nothing

-- | Low-level journal event function
journalEvent ::
  ( Member AWS.AWSSubsystem r,
    Member (Embed IO) r
  ) =>
  UserEvent'EventType ->
  UserId ->
  Maybe EmailAddress ->
  Maybe Locale ->
  Maybe TeamId ->
  Maybe Name ->
  Sem r ()
journalEvent typ uid em loc tid nm = do
  mbQueueUrl <- AWS.getJournalQueueUrl
  forM_ mbQueueUrl $ \queueUrl -> do
    ts <- now
    rnd <- embed nextRandom
    let userEvent :: Proto.UserEvent =
          defMessage
            & U.eventType .~ typ
            & U.userId .~ toBytes uid
            & U.utcTime .~ ts
            & U.maybe'email .~ (toByteString' <$> em)
            & U.maybe'locale .~ (pack . show <$> loc)
            & U.maybe'teamId .~ (toBytes <$> tid)
            & U.maybe'name .~ (toByteString' <$> nm)
        encoded = fromStrict $ B64.encode $ encodeMessage userEvent
    void $ AWS.enqueueFIFO queueUrl "user.events" rnd encoded

-- | Journal a Wire.API.UserEvent by pattern matching on its constructors
journalUserEvent ::
  ( Member AWS.AWSSubsystem r,
    Member (Embed IO) r
  ) =>
  UserId ->
  API.UserEvent ->
  Sem r ()
journalUserEvent orig e = case e of
  API.UserActivated acc ->
    userActivateJournal acc
  API.UserUpdated API.UserUpdatedData {eupName = Just name} ->
    userUpdateJournal orig Nothing Nothing (Just name)
  API.UserUpdated API.UserUpdatedData {eupLocale = Just loc} ->
    userUpdateJournal orig Nothing (Just loc) Nothing
  API.UserIdentityUpdated (API.UserIdentityUpdatedData _ (Just em) _) ->
    userUpdateJournal orig (Just em) Nothing Nothing
  API.UserIdentityRemoved (API.UserIdentityRemovedData _ (Just em) _) ->
    userEmailRemoveJournal orig em
  API.UserDeleted {} ->
    userDeleteJournal orig
  _ ->
    pure ()
