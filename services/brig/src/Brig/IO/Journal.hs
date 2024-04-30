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

module Brig.IO.Journal
  ( userActivate,
    userUpdate,
    userDelete,
    userEmailRemove,
  )
where

import Brig.AWS qualified as AWS
import Brig.App
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
import Proto.UserEvents (UserEvent, UserEvent'EventType (..))
import Proto.UserEvents_Fields qualified as U
import Wire.API.User

-- [Note: journaling]
-- User journal operations to SQS are a no-op when the service is started
-- without journaling arguments for user updates

userActivate :: (MonadReader Env m, MonadIO m) => User -> m ()
userActivate u@User {..} = journalEvent UserEvent'USER_ACTIVATE (userId u) (userEmail u) (Just userLocale) userTeam (Just userDisplayName)

userUpdate :: (MonadReader Env m, MonadIO m) => UserId -> Maybe Email -> Maybe Locale -> Maybe Name -> m ()
userUpdate uid em loc = journalEvent UserEvent'USER_UPDATE uid em loc Nothing

userEmailRemove :: (MonadReader Env m, MonadIO m) => UserId -> Email -> m ()
userEmailRemove uid em = journalEvent UserEvent'USER_EMAIL_REMOVE uid (Just em) Nothing Nothing Nothing

userDelete :: (MonadReader Env m, MonadIO m) => UserId -> m ()
userDelete uid = journalEvent UserEvent'USER_DELETE uid Nothing Nothing Nothing Nothing

journalEvent :: (MonadReader Env m, MonadIO m) => UserEvent'EventType -> UserId -> Maybe Email -> Maybe Locale -> Maybe TeamId -> Maybe Name -> m ()
journalEvent typ uid em loc tid nm =
  -- this may be the only place that uses awsEnv from brig Env.  refactor it to use the
  -- DeleteQueue effect instead?
  view awsEnv >>= \env -> for_ (view AWS.userJournalQueue env) $ \queue -> do
    ts <- now
    rnd <- liftIO nextRandom
    let userEvent :: UserEvent =
          defMessage
            & U.eventType .~ typ
            & U.userId .~ toBytes uid
            & U.utcTime .~ ts
            & U.maybe'email .~ (toByteString' <$> em)
            & U.maybe'locale .~ (pack . show <$> loc)
            & U.maybe'teamId .~ (toBytes <$> tid)
            & U.maybe'name .~ (toByteString' <$> nm) -- []
        encoded = fromStrict $ B64.encode $ encodeMessage userEvent
    AWS.execute env (AWS.enqueueFIFO queue "user.events" rnd encoded)
