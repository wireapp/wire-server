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

module Spar.Sem.AReqIDStore.Cassandra
  ( aReqIDStoreToCassandra,
  )
where

import Cassandra as Cas
import Control.Lens
import Control.Monad.Except
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import Spar.Data.Instances ()
import Spar.Options
import Spar.Sem.AReqIDStore
import Wire.API.User.Saml
import Wire.Sem.Now (Now)
import qualified Wire.Sem.Now as Now

aReqIDStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Now, Error TTLError, Embed IO, Input Opts] r) =>
  Sem (AReqIDStore ': r) a ->
  Sem r a
aReqIDStoreToCassandra = interpret $ \case
  Store itla t -> do
    denv <- Data.mkEnv <$> input <*> Now.get
    a <- embed @m $ runExceptT $ runReaderT (storeAReqID itla t) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  UnStore itla -> embed @m $ unStoreAReqID itla
  IsAlive itla -> embed @m $ isAliveAReqID itla

storeAReqID ::
  (HasCallStack, MonadReader Data.Env m, MonadClient m, MonadError TTLError m) =>
  AReqId ->
  SAML.Time ->
  m ()
storeAReqID (SAML.ID rid) (SAML.Time endOfLife) = do
  env <- ask
  TTL ttl <- Data.mkTTLAuthnRequests env endOfLife
  retry x5 . write ins $ params LocalQuorum (rid, ttl)
  where
    ins :: PrepQuery W (SAML.XmlText, Int32) ()
    ins = "INSERT INTO authreq (req) VALUES (?) USING TTL ?"

unStoreAReqID ::
  (HasCallStack, MonadClient m) =>
  AReqId ->
  m ()
unStoreAReqID (SAML.ID rid) = retry x5 . write del . params LocalQuorum $ Identity rid
  where
    del :: PrepQuery W (Identity SAML.XmlText) ()
    del = "DELETE FROM authreq WHERE req = ?"

isAliveAReqID ::
  (HasCallStack, MonadClient m) =>
  AReqId ->
  m Bool
isAliveAReqID (SAML.ID rid) =
  (==) (Just 1) <$> (retry x1 . query1 sel . params LocalQuorum $ Identity rid)
  where
    sel :: PrepQuery R (Identity SAML.XmlText) (Identity Int64)
    sel = "SELECT COUNT(*) FROM authreq WHERE req = ?"
