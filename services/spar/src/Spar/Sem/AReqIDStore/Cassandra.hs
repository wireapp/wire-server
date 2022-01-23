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

module Spar.Sem.AReqIDStore.Cassandra where

import Cassandra
import Control.Monad.Except (runExceptT)
import Imports hiding (MonadReader (..), Reader)
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import SAML2.WebSSO (fromTime)
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import Spar.Error
import Spar.Sem.AReqIDStore
import Spar.Sem.Now (Now)
import qualified Spar.Sem.Now as Now
import Wire.API.User.Saml (Opts, TTLError)

aReqIDStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Now, Error TTLError, Embed IO, Input Opts] r) =>
  Sem (AReqIDStore ': r) a ->
  Sem r a
aReqIDStoreToCassandra = interpret $ \case
  Store itla t -> do
    denv <- Data.mkEnv <$> input <*> (fromTime <$> Now.get)
    a <- embed @m $ runExceptT $ runReaderT (Data.storeAReqID itla t) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  UnStore itla -> embed @m $ Data.unStoreAReqID itla
  IsAlive itla -> embed @m $ Data.isAliveAReqID itla

ttlErrorToSparError :: Member (Error SparError) r => Sem (Error TTLError ': r) a -> Sem r a
ttlErrorToSparError = mapError (SAML.CustomError . SparCassandraTTLError)
