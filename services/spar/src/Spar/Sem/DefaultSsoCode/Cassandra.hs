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

module Spar.Sem.DefaultSsoCode.Cassandra
  ( defaultSsoCodeToCassandra,
  )
where

import Cassandra
import Imports
import Polysemy
import qualified SAML2.WebSSO.Types as SAML
import Spar.Data.Instances ()
import Spar.Sem.DefaultSsoCode

defaultSsoCodeToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (DefaultSsoCode ': r) a ->
  Sem r a
defaultSsoCodeToCassandra =
  interpret $
    embed @m . \case
      Get -> getDefaultSsoCode
      Store ip -> storeDefaultSsoCode ip
      Delete -> deleteDefaultSsoCode

-- It's important to maintain two invariants:
-- 1) whenever there is a default code, it must also exist in the idp table
-- 2) there can always only be one default SSO code selected

getDefaultSsoCode ::
  (HasCallStack, MonadClient m) =>
  m (Maybe SAML.IdPId)
getDefaultSsoCode =
  runIdentity
    <$$> (retry x1 . query1 sel $ params LocalQuorum ())
  where
    sel :: PrepQuery R () (Identity SAML.IdPId)
    sel = "SELECT idp FROM default_idp WHERE partition_key_always_default = 'default' ORDER BY idp LIMIT 1"

storeDefaultSsoCode ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  m ()
storeDefaultSsoCode idpId = do
  -- there is a race condition here which means there could potentially be more
  -- than one entry (violating invariant 2).
  -- However, the SELECT query will deterministally pick one of them due to the
  -- `ORDER BY` clause. The others will get removed by `deleteDefaultSsoCode`
  -- the next time this function is called (as it removes all entries).
  deleteDefaultSsoCode
  retry x5 . write ins $ params LocalQuorum (Identity idpId)
  where
    ins :: PrepQuery W (Identity SAML.IdPId) ()
    ins = "INSERT INTO default_idp (partition_key_always_default, idp) VALUES ('default', ?)"

deleteDefaultSsoCode ::
  (HasCallStack, MonadClient m) =>
  m ()
deleteDefaultSsoCode = retry x5 . write del $ params LocalQuorum ()
  where
    del :: PrepQuery W () ()
    del = "DELETE FROM default_idp WHERE partition_key_always_default = 'default'"
