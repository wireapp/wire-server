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
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore qualified as BlacklistStore
import Brig.Email
import Imports
import Polysemy (Member)
import System.Logger.Class (field, msg, (~~))
import System.Logger.Class qualified as Log
import Wire.API.User.Identity
import Wire.UserKeyStore (userEmailKey)

onEvent :: (Member BlacklistStore r) => SESNotification -> AppT r ()
onEvent (MailBounce BouncePermanent es) = onPermanentBounce es
onEvent (MailBounce BounceTransient es) = onTransientBounce es
onEvent (MailBounce BounceUndetermined es) = onUndeterminedBounce es
onEvent (MailComplaint es) = onComplaint es

onPermanentBounce :: (Member BlacklistStore r) => [Email] -> AppT r ()
onPermanentBounce = mapM_ $ \e -> do
  logEmailEvent "Permanent bounce" e
  liftSem $ BlacklistStore.insert (mkEmailKey e)

onTransientBounce :: [Email] -> AppT r ()
onTransientBounce = mapM_ (logEmailEvent "Transient bounce")

onUndeterminedBounce :: [Email] -> AppT r ()
onUndeterminedBounce = mapM_ (logEmailEvent "Undetermined bounce")

onComplaint :: (Member BlacklistStore r) => [Email] -> AppT r ()
onComplaint = mapM_ $ \e -> do
  logEmailEvent "Complaint" e
  liftSem $ BlacklistStore.insert (mkEmailKey e)

logEmailEvent :: Text -> Email -> AppT r ()
logEmailEvent t e = Log.info $ field "email" (fromEmail e) ~~ msg t
