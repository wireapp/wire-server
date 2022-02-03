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

module Spar.Sem.IdPRawMetadataStore.Cassandra
  ( idpRawMetadataStoreToCassandra,
  )
where

import Cassandra as Cas
import Control.Lens
import Data.String.Conversions
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML
import Spar.Data.Instances ()
import Spar.Sem.IdPRawMetadataStore

idpRawMetadataStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (IdPRawMetadataStore ': r) a ->
  Sem r a
idpRawMetadataStoreToCassandra =
  interpret $
    embed @m . \case
      Store i t -> storeIdPRawMetadata i t
      Get i -> getIdPRawMetadata i
      Delete i -> deleteIdPRawMetadata i

storeIdPRawMetadata ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  ST ->
  m ()
storeIdPRawMetadata idp raw = retry x5 . write ins $ params LocalQuorum (idp, raw)
  where
    ins :: PrepQuery W (SAML.IdPId, ST) ()
    ins = "INSERT INTO idp_raw_metadata (id, metadata) VALUES (?, ?)"

getIdPRawMetadata ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  m (Maybe ST)
getIdPRawMetadata idp =
  runIdentity
    <$$> (retry x1 . query1 sel $ params LocalQuorum (Identity idp))
  where
    sel :: PrepQuery R (Identity SAML.IdPId) (Identity ST)
    sel = "SELECT metadata FROM idp_raw_metadata WHERE id = ?"

deleteIdPRawMetadata ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  m ()
deleteIdPRawMetadata idp = retry x5 . write del $ params LocalQuorum (Identity idp)
  where
    del :: PrepQuery W (Identity SAML.IdPId) ()
    del = "DELETE FROM idp_raw_metadata WHERE id = ?"
