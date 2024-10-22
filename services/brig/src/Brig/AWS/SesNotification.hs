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

module Brig.AWS.SesNotification
  ( onEvent,
  )
where

import Brig.AWS.Types
import Brig.App
import Data.Mailbox
import Imports
import Polysemy (Member)
import System.Logger.Class (field, msg, (~~))
import System.Logger.Class qualified as Log
import Wire.API.User.Identity
import Wire.UserSubsystem

onEvent :: (Member UserSubsystem r) => SESNotification -> AppT r ()
onEvent (MailBounce BouncePermanent recipients) = onPermanentBounce recipients
onEvent (MailBounce BounceTransient recipients) = onTransientBounce recipients
onEvent (MailBounce BounceUndetermined recipients) = onUndeterminedBounce recipients
onEvent (MailComplaint recipients) = onComplaint recipients

onPermanentBounce :: (Member UserSubsystem r) => [Mailbox] -> AppT r ()
onPermanentBounce = mapM_ $ \mailbox -> do
  logEmailEvent "Permanent bounce" mailbox.address
  liftSem $ blockListInsert mailbox.address

onTransientBounce :: [Mailbox] -> AppT r ()
onTransientBounce = mapM_ (logEmailEvent "Transient bounce" . (.address))

onUndeterminedBounce :: [Mailbox] -> AppT r ()
onUndeterminedBounce = mapM_ (logEmailEvent "Undetermined bounce" . (.address))

onComplaint :: (Member UserSubsystem r) => [Mailbox] -> AppT r ()
onComplaint = mapM_ $ \e -> do
  logEmailEvent "Complaint" e.address
  liftSem $ blockListInsert e.address

logEmailEvent :: Text -> EmailAddress -> AppT r ()
logEmailEvent t e = Log.info $ field "email" (fromEmail e) ~~ msg t
