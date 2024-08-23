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

module Brig.AWS.Types
  ( -- * SES Notification
    SESNotification (..),
    SESBounceType (..),
  )
where

import Data.Aeson
import Imports
import Wire.API.User.Identity

-------------------------------------------------------------------------------
-- Notifications

data SESNotification
  = MailBounce !SESBounceType [Email] Value
  | MailComplaint [Email] Value
  deriving (Eq, Show)

data SESBounceType
  = BounceUndetermined
  | BouncePermanent
  | BounceTransient
  deriving (Eq, Show)

instance FromJSON SESBounceType where
  parseJSON "Undetermined" = pure BounceUndetermined
  parseJSON "Permanent" = pure BouncePermanent
  parseJSON "Transient" = pure BounceTransient
  parseJSON x = fail $ "Unknown type: " <> show x

instance FromJSON SESNotification where
  parseJSON = withObject "SESNotification" $ \o -> do
    t <- o .: "notificationType"
    case (t :: Text) of
      "Bounce" -> do
        b <- o .: "bounce"
        bt <- b .: "bounceType"
        br <- b .: "bouncedRecipients"
        em <- mapM (\r -> r .: "emailAddress") br
        pure $! MailBounce bt em (Object o)
      "Complaint" -> do
        c <- o .: "complaint"
        cr <- c .: "complainedRecipients"
        em <- mapM (\r -> r .: "emailAddress") cr
        pure $! MailComplaint em (Object o)
      x -> fail ("Brig.AWS: Unexpected notification type" ++ show x)
