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

module Spar.Sem.AssIDStore.Cassandra
  ( assIDStoreToCassandra,
  )
where

import Cassandra as Cas
import Control.Lens
import Control.Monad.Except
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import Spar.Data.Instances ()
import Spar.Options
import Spar.Sem.AssIDStore
import Wire.API.User.Saml
import Wire.Sem.Now (Now)
import qualified Wire.Sem.Now as Now

assIDStoreToCassandra ::
  forall m r a.
  ( MonadClient m,
    ( Member (Embed m) r,
      Member Now r,
      Member (Error TTLError) r,
      Member (Embed IO) r,
      Member (Input Opts) r
    )
  ) =>
  Sem (AssIDStore ': r) a ->
  Sem r a
assIDStoreToCassandra =
  interpret $ \case
    Store itla t -> do
      denv <- Data.mkEnv <$> input <*> Now.get
      a <- embed @m $ runExceptT $ runReaderT (storeAssID itla t) denv
      case a of
        Left err -> throw err
        Right () -> pure ()
    UnStore itla -> embed @m $ unStoreAssID itla
    IsAlive itla -> embed @m $ isAliveAssID itla

storeAssID ::
  (HasCallStack, MonadReader Data.Env m, MonadClient m, MonadError TTLError m) =>
  AssId ->
  SAML.Time ->
  m ()
storeAssID (SAML.ID aid) (SAML.Time endOfLife) = do
  env <- ask
  TTL ttl <- Data.mkTTLAssertions env endOfLife
  retry x5 . write ins $ params LocalQuorum (aid, ttl)
  where
    ins :: PrepQuery W (Text, Int32) ()
    ins = "INSERT INTO authresp (resp) VALUES (?) USING TTL ?"

unStoreAssID ::
  (HasCallStack, MonadClient m) =>
  AssId ->
  m ()
unStoreAssID (SAML.ID aid) = retry x5 . write del . params LocalQuorum $ Identity aid
  where
    del :: PrepQuery W (Identity Text) ()
    del = "DELETE FROM authresp WHERE resp = ?"

isAliveAssID ::
  (HasCallStack, MonadClient m) =>
  AssId ->
  m Bool
isAliveAssID (SAML.ID aid) =
  (==) (Just 1) <$> (retry x1 . query1 sel . params LocalQuorum $ Identity aid)
  where
    sel :: PrepQuery R (Identity Text) (Identity Int64)
    sel = "SELECT COUNT(*) FROM authresp WHERE resp = ?"
