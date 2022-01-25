{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.Sem.BindCookieStore.Cassandra where

import Cassandra
import Control.Monad.Except (runExceptT)
import Imports hiding (MonadReader (..), Reader)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import SAML2.WebSSO (fromTime)
import qualified Spar.Data as Data
import Spar.Sem.BindCookieStore
import Spar.Sem.Now (Now)
import qualified Spar.Sem.Now as Now
import Wire.API.User.Saml (Opts, TTLError)

bindCookieStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Now, Error TTLError, Embed IO, Input Opts] r) =>
  Sem (BindCookieStore ': r) a ->
  Sem r a
bindCookieStoreToCassandra = interpret $ \case
  Insert sbc uid ndt -> do
    denv <- Data.mkEnv <$> input <*> (fromTime <$> Now.get)
    a <- embed @m $ runExceptT $ runReaderT (Data.insertBindCookie sbc uid ndt) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  Lookup bc -> embed @m $ Data.lookupBindCookie bc
