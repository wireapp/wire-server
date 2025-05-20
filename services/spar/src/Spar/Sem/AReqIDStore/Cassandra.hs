-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
  ( MonadClient m,
    ( Member (Embed m) r,
      Member Now r,
      Member (Error TTLError) r,
      Member (Embed IO) r,
      Member (Input Opts) r
    )
  ) =>
  Sem (AReqIDStore ': r) a ->
  Sem r a
aReqIDStoreToCassandra = interpret $ \case
  Store itla issuer t -> do
    denv <- Data.mkEnv <$> input <*> Now.get
    a <- embed @m $ runExceptT $ runReaderT (storeAReqID itla issuer t) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  GetIdpIssuer itla -> embed @m $ getAReqID itla
  UnStore itla -> embed @m $ unStoreAReqID itla

storeAReqID ::
  (HasCallStack, MonadReader Data.Env m, MonadClient m, MonadError TTLError m) =>
  AReqId ->
  SAML.Issuer ->
  SAML.Time ->
  m ()
storeAReqID (SAML.ID rid) issuer (SAML.Time endOfLife) = do
  env <- ask
  TTL ttl <- Data.mkTTLAuthnRequests env endOfLife
  retry x5 . write ins $ params LocalQuorum (rid, issuer, ttl)
  where
    ins :: PrepQuery W (Text, SAML.Issuer, Int32) ()
    ins = "INSERT INTO authreq (req, idp_issuer) VALUES (?,?) USING TTL ?"

getAReqID ::
  (HasCallStack, MonadClient m) =>
  AReqId ->
  m (Maybe SAML.Issuer)
getAReqID (SAML.ID rid) =
  listToMaybe <$> (runIdentity <$$> (retry x1 . query sel . params LocalQuorum $ Identity rid))
  where
    sel :: PrepQuery R (Identity Text) (Identity SAML.Issuer)
    sel = "SELECT idp_issuer FROM authreq WHERE req = ?"

unStoreAReqID ::
  (HasCallStack, MonadClient m) =>
  AReqId ->
  m ()
unStoreAReqID (SAML.ID rid) = retry x5 . write del . params LocalQuorum $ Identity rid
  where
    del :: PrepQuery W (Identity Text) ()
    del = "DELETE FROM authreq WHERE req = ?"
