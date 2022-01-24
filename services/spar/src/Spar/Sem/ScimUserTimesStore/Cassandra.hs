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

module Spar.Sem.ScimUserTimesStore.Cassandra where

import Imports
import Polysemy
import Spar.Sem.ScimUserTimesStore (ScimUserTimesStore(..))
import Cassandra as Cas
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Web.Scim.Schema.Common (WithId (..))
import Web.Scim.Schema.Meta (Meta (..), WithMeta (..))


scimUserTimesStoreToCassandra :: forall m r a. (MonadClient m, Member (Embed m) r) => Sem (ScimUserTimesStore ': r) a -> Sem r a
scimUserTimesStoreToCassandra =
  interpret $
    embed @m . \case
      Write wm -> writeScimUserTimes wm
      Read uid -> readScimUserTimes uid
      Delete uid -> deleteScimUserTimes uid

----------------------------------------------------------------------
-- SCIM user records
--
-- docs/developer/scim/storage.md {#DevScimStorageUsers}

-- | Store creation and last-update time from the scim metadata under a user id.
writeScimUserTimes :: (HasCallStack, MonadClient m) => WithMeta (WithId UserId a) -> m ()
writeScimUserTimes (WithMeta meta (WithId uid _)) =
  retry x5 . write ins $
    params
      LocalQuorum
      ( uid,
        toUTCTimeMillis $ created meta,
        toUTCTimeMillis $ lastModified meta
      )
  where
    ins :: PrepQuery W (UserId, UTCTimeMillis, UTCTimeMillis) ()
    ins = "INSERT INTO scim_user_times (uid, created_at, last_updated_at) VALUES (?, ?, ?)"

-- | Read creation and last-update time from database for a given user id.
readScimUserTimes :: (HasCallStack, MonadClient m) => UserId -> m (Maybe (UTCTimeMillis, UTCTimeMillis))
readScimUserTimes uid = do
  retry x1 . query1 sel $ params LocalQuorum (Identity uid)
  where
    sel :: PrepQuery R (Identity UserId) (UTCTimeMillis, UTCTimeMillis)
    sel = "SELECT created_at, last_updated_at FROM scim_user_times WHERE uid = ?"

-- | Delete a SCIM user's access times by id.
-- You'll also want to ensure they are deleted in Brig and in the SAML Users table.
deleteScimUserTimes ::
  (HasCallStack, MonadClient m) =>
  UserId ->
  m ()
deleteScimUserTimes uid = retry x5 . write del $ params LocalQuorum (Identity uid)
  where
    del :: PrepQuery W (Identity UserId) ()
    del = "DELETE FROM scim_user_times WHERE uid = ?"
