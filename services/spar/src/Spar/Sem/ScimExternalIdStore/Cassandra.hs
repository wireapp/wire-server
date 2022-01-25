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

module Spar.Sem.ScimExternalIdStore.Cassandra (
  scimExternalIdStoreToCassandra,
  insertScimExternalId,
  lookupScimExternalId,
  deleteScimExternalId ) where

import Brig.Types.Common (Email, fromEmail)
import Cassandra
import Data.Id
import Imports
import Polysemy
import Spar.Sem.ScimExternalIdStore (ScimExternalIdStore (..))

scimExternalIdStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (ScimExternalIdStore ': r) a ->
  Sem r a
scimExternalIdStoreToCassandra =
  interpret $
    embed @m . \case
      Insert tid em uid -> insertScimExternalId tid em uid
      Lookup tid em -> lookupScimExternalId tid em
      Delete tid em -> deleteScimExternalId tid em

-- | If a scim externalId does not have an associated saml idp issuer, it cannot be stored in
-- table @spar.user@.  In those cases, and only in those cases, we store the mapping to
-- 'UserId' here.  (Note that since there is no associated IdP, the externalId is required to
-- be an email address, so we enforce that in the type signature, even though we only use it
-- as a 'Text'.)
insertScimExternalId :: (HasCallStack, MonadClient m) => TeamId -> Email -> UserId -> m ()
insertScimExternalId tid (fromEmail -> email) uid =
  retry x5 . write insert $ params LocalQuorum (tid, email, uid)
  where
    insert :: PrepQuery W (TeamId, Text, UserId) ()
    insert = "INSERT INTO scim_external (team, external_id, user) VALUES (?, ?, ?)"

-- | The inverse of 'insertScimExternalId'.
lookupScimExternalId :: (HasCallStack, MonadClient m) => TeamId -> Email -> m (Maybe UserId)
lookupScimExternalId tid (fromEmail -> email) = runIdentity <$$> (retry x1 . query1 sel $ params LocalQuorum (tid, email))
  where
    sel :: PrepQuery R (TeamId, Text) (Identity UserId)
    sel = "SELECT user FROM scim_external WHERE team = ? and external_id = ?"

-- | The other inverse of 'insertScimExternalId' :).
deleteScimExternalId :: (HasCallStack, MonadClient m) => TeamId -> Email -> m ()
deleteScimExternalId tid (fromEmail -> email) =
  retry x5 . write delete $ params LocalQuorum (tid, email)
  where
    delete :: PrepQuery W (TeamId, Text) ()
    delete = "DELETE FROM scim_external WHERE team = ? and external_id = ?"
