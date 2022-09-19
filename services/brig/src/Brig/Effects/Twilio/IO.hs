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

module Brig.Effects.Twilio.IO (twilioToIO) where

import Bilge.Retry
import Brig.Effects.Twilio
import Control.Monad.Catch
import Control.Retry
import Imports
import Polysemy
import qualified Ropes.Twilio as Ropes

twilioToIO ::
  forall r a.
  Member (Embed IO) r =>
  Sem (Twilio ': r) a ->
  Sem r a
twilioToIO =
  interpret $
    embed @IO . \case
      LookupPhone cred m txt detail code ->
        liftIO . try @_ @Ropes.ErrorResponse $
          recovering x3 httpHandlers $
            const $ Ropes.lookupPhone cred m txt detail code

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000
