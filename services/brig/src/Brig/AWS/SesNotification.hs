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

import Brig.App
import Data.Aeson
import Imports
import Polysemy (Member)
import System.Logger.Class (field, msg, (~~))
import System.Logger.Class qualified as Log
import Wire.API.AWS.Types
import Wire.API.User.Identity
import Wire.UserSubsystem

onEvent :: (Member UserSubsystem r) => SESNotification -> AppT r ()
onEvent (MailBounce BouncePermanent es e) = onPermanentBounce e es
onEvent (MailBounce BounceTransient es e) = onTransientBounce e es
onEvent (MailBounce BounceUndetermined es e) = onUndeterminedBounce e es
onEvent (MailComplaint es e) = onComplaint e es

onPermanentBounce :: (Member UserSubsystem r) => SESOriginalEvent -> [Email] -> AppT r ()
onPermanentBounce event = mapM_ $ \email -> do
  logEmailEvent event "Permanent bounce" email
  liftSem $ blockListInsert email (Just event)

onTransientBounce :: SESOriginalEvent -> [Email] -> AppT r ()
onTransientBounce e = mapM_ (logEmailEvent e "Transient bounce")

onUndeterminedBounce :: SESOriginalEvent -> [Email] -> AppT r ()
onUndeterminedBounce e = mapM_ (logEmailEvent e "Undetermined bounce")

onComplaint :: (Member UserSubsystem r) => SESOriginalEvent -> [Email] -> AppT r ()
onComplaint event = mapM_ $ \email -> do
  logEmailEvent event "Complaint" email
  liftSem $ blockListInsert email (Just event)

logEmailEvent :: SESOriginalEvent -> Text -> Email -> AppT r ()
logEmailEvent event t e = Log.info $ field "email" (fromEmail e) ~~ msg t ~~ field "event" (encode event)
