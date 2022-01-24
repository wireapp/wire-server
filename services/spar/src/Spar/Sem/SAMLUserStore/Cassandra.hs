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

module Spar.Sem.SAMLUserStore.Cassandra where

import Cassandra
import qualified Control.Monad.Catch as Catch
import Data.String.Conversions (cs)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Final
import qualified SAML2.WebSSO.Error as SAML
import qualified Spar.Data as Data
import Spar.Error
import Spar.Sem.SAMLUserStore

samlUserStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (SAMLUserStore ': r) a ->
  Sem r a
samlUserStoreToCassandra =
  interpret $
    embed . \case
      Insert ur uid -> Data.insertSAMLUser ur uid
      Get ur -> Data.getSAMLUser ur
      GetAnyByIssuer is -> Data.getSAMLAnyUserByIssuer is
      GetSomeByIssuer is -> Data.getSAMLSomeUsersByIssuer is
      DeleteByIssuer is -> Data.deleteSAMLUsersByIssuer is
      Delete uid ur -> Data.deleteSAMLUser uid ur

interpretClientToIO ::
  Members '[Error SparError, Final IO] r =>
  ClientState ->
  Sem (Embed Client ': r) a ->
  Sem r a
interpretClientToIO ctx = interpret $ \case
  Embed action -> withStrategicToFinal @IO $ do
    action' <- liftS $ runClient ctx action
    st <- getInitialStateS
    handler' <- bindS $ throw @SparError . SAML.CustomError . SparCassandraError . cs . show @SomeException
    pure $ action' `Catch.catch` \e -> handler' $ e <$ st
