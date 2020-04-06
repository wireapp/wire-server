-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

import Brig.Types (Email (..))
import Data.Aeson
import Imports

-------------------------------------------------------------------------------
-- Notifications

data SESNotification
  = MailBounce !SESBounceType [Email]
  | MailComplaint [Email]
  deriving (Eq, Show)

data SESBounceType
  = BounceUndetermined
  | BouncePermanent
  | BounceTransient
  deriving (Eq, Show)

instance FromJSON SESBounceType where
  parseJSON "Undetermined" = return BounceUndetermined
  parseJSON "Permanent" = return BouncePermanent
  parseJSON "Transient" = return BounceTransient
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
        return $! MailBounce bt em
      "Complaint" -> do
        c <- o .: "complaint"
        cr <- c .: "complainedRecipients"
        em <- mapM (\r -> r .: "emailAddress") cr
        return $! MailComplaint em
      x -> fail ("Brig.AWS: Unexpected notification type" ++ show x)
