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

module Spar.Sem.AssIDStore.Cassandra where

import Cassandra
import Control.Monad.Except (runExceptT)
import Imports hiding (MonadReader (..), Reader)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import SAML2.WebSSO (fromTime)
import qualified Spar.Data as Data
import Spar.Sem.AssIDStore
import Spar.Sem.Now (Now)
import qualified Spar.Sem.Now as Now
import Wire.API.User.Saml (Opts, TTLError)

assIDStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Now, Error TTLError, Embed IO, Input Opts] r) =>
  Sem (AssIDStore ': r) a ->
  Sem r a
assIDStoreToCassandra =
  interpret $ \case
    Store itla t -> do
      denv <- Data.mkEnv <$> input <*> (fromTime <$> Now.get)
      a <- embed @m $ runExceptT $ runReaderT (Data.storeAssID itla t) denv
      case a of
        Left err -> throw err
        Right () -> pure ()
    UnStore itla -> embed @m $ Data.unStoreAssID itla
    IsAlive itla -> embed @m $ Data.isAliveAssID itla
