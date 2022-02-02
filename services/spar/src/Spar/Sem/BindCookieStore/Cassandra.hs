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

module Spar.Sem.BindCookieStore.Cassandra
  ( bindCookieStoreToCassandra,
  )
where

import Cassandra as Cas
import Control.Lens
import Control.Monad.Except
import Data.Id
import Data.String.Conversions
import Data.Time
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import SAML2.WebSSO (fromTime)
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import Spar.Sem.BindCookieStore
import Spar.Sem.Now (Now)
import qualified Spar.Sem.Now as Now
import qualified Web.Cookie as Cky
import Wire.API.Cookie
import Wire.API.User.Saml

bindCookieStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Now, Error TTLError, Embed IO, Input Opts] r) =>
  Sem (BindCookieStore ': r) a ->
  Sem r a
bindCookieStoreToCassandra = interpret $ \case
  Insert sbc uid ndt -> do
    denv <- Data.mkEnv <$> input <*> (fromTime <$> Now.get)
    a <- embed @m $ runExceptT $ runReaderT (insertBindCookie sbc uid ndt) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  Lookup bc -> embed @m $ lookupBindCookie bc

-- | Associate the value of a 'BindCookie' with its 'UserId'.  The 'TTL' of this entry should be the
-- same as the one of the 'AuthnRequest' sent with the cookie.
insertBindCookie ::
  (HasCallStack, MonadClient m, MonadReader Data.Env m, MonadError TTLError m) =>
  SetBindCookie ->
  UserId ->
  NominalDiffTime ->
  m ()
insertBindCookie cky uid ttlNDT = do
  env <- ask
  TTL ttlInt32 <- Data.mkTTLAuthnRequestsNDT env ttlNDT
  let ckyval = cs . Cky.setCookieValue . SAML.fromSimpleSetCookie . getSimpleSetCookie $ cky
  retry x5 . write ins $ params LocalQuorum (ckyval, uid, ttlInt32)
  where
    ins :: PrepQuery W (ST, UserId, Int32) ()
    ins = "INSERT INTO bind_cookie (cookie, session_owner) VALUES (?, ?) USING TTL ?"

-- | The counter-part of 'insertBindCookie'.
lookupBindCookie :: (HasCallStack, MonadClient m) => BindCookie -> m (Maybe UserId)
lookupBindCookie (cs . fromBindCookie -> ckyval :: ST) =
  runIdentity <$$> do
    (retry x1 . query1 sel $ params LocalQuorum (Identity ckyval))
  where
    sel :: PrepQuery R (Identity ST) (Identity UserId)
    sel = "SELECT session_owner FROM bind_cookie WHERE cookie = ?"
