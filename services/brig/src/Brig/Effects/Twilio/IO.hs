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
import Brig.App
import Brig.Effects.Twilio
import Control.Lens (view)
import Control.Monad.Catch
import Control.Retry
import Imports
import Polysemy
import qualified Ropes.Twilio as Ropes

twilioToIO ::
  forall m r a.
  (Member (Embed m) r, MonadReader Env m, MonadIO m) =>
  Sem (Twilio ': r) a ->
  Sem r a
twilioToIO =
  interpret $
    embed @m . \case
      LookupPhone txt detail code -> do
        cred <- view twilioCreds
        m <- view httpManager
        liftIO . try @_ @Ropes.ErrorResponse $
          recovering x3 httpHandlers $
            const $ Ropes.lookupPhone cred m txt detail code

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000
