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

module Wire.ScimUserTimesStore.Cassandra
  ( scimUserTimesStoreToCassandra,
  )
where

import Cassandra as Cas
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Imports
import Polysemy
import Web.Scim.Schema.Common (WithId (..))
import Web.Scim.Schema.Meta (Meta (..), WithMeta (..))
import Wire.ScimUserTimesStore (ScimUserTimes (..), ScimUserTimesStore (..))

scimUserTimesStoreToCassandra :: forall m r a. (MonadClient m, Member (Embed m) r) => Sem (ScimUserTimesStore ': r) a -> Sem r a
scimUserTimesStoreToCassandra =
  interpret $
    embed @m . \case
      Write emailType emailPrimary wm -> writeScimUserTimes emailType emailPrimary wm
      Read uid -> readScimUserTimes uid
      ReadMulti uids -> readScimUserTimesMulti uids
      Delete uid -> deleteScimUserTimes uid

----------------------------------------------------------------------
-- SCIM user records
--
-- docs/developer/scim/storage.md {#DevScimStorageUsers}

-- | Store creation and last-update time from the scim metadata under a user
-- id, together with the SCIM email metadata (@type@, @primary@) of the stored
-- email entry (if any was supplied by the IdP).
writeScimUserTimes :: (HasCallStack, MonadClient m) => Maybe Text -> Maybe Bool -> WithMeta (WithId UserId a) -> m ()
writeScimUserTimes emailType emailPrimary (WithMeta meta (WithId uid _)) =
  retry x5 . write ins $
    params
      LocalQuorum
      ( uid,
        toUTCTimeMillis $ created meta,
        toUTCTimeMillis $ lastModified meta,
        emailType,
        emailPrimary
      )
  where
    ins :: PrepQuery W (UserId, UTCTimeMillis, UTCTimeMillis, Maybe Text, Maybe Bool) ()
    ins = "INSERT INTO scim_user_times (uid, created_at, last_updated_at, email_type, email_primary) VALUES (?, ?, ?, ?, ?)"

-- | Read creation and last-update time (and SCIM email metadata) from the
-- database for a given user id.
readScimUserTimes :: (HasCallStack, MonadClient m) => UserId -> m (Maybe ScimUserTimes)
readScimUserTimes uid = do
  fmap rowToScimUserTimes
    <$> retry x1 (query1 sel $ params LocalQuorum (Identity uid))
  where
    sel :: PrepQuery R (Identity UserId) (UTCTimeMillis, UTCTimeMillis, Maybe Text, Maybe Bool)
    sel = "SELECT created_at, last_updated_at, email_type, email_primary FROM scim_user_times WHERE uid = ?"

readScimUserTimesMulti :: (HasCallStack, MonadClient m) => [UserId] -> m [(UserId, ScimUserTimes)]
readScimUserTimesMulti uid = do
  fmap rowToUserTimesMulti
    <$> retry x1 (query sel $ params LocalQuorum (Identity uid))
  where
    sel :: PrepQuery R (Identity [UserId]) (UserId, UTCTimeMillis, UTCTimeMillis, Maybe Text, Maybe Bool)
    sel = "SELECT uid, created_at, last_updated_at, email_type, email_primary FROM scim_user_times WHERE uid IN ?"

rowToScimUserTimes :: (UTCTimeMillis, UTCTimeMillis, Maybe Text, Maybe Bool) -> ScimUserTimes
rowToScimUserTimes (created_, lastUpdated, emailType, emailPrimary) =
  ScimUserTimes created_ lastUpdated emailType emailPrimary

rowToUserTimesMulti :: (UserId, UTCTimeMillis, UTCTimeMillis, Maybe Text, Maybe Bool) -> (UserId, ScimUserTimes)
rowToUserTimesMulti (uid, created_, lastUpdated, emailType, emailPrimary) =
  (uid, ScimUserTimes created_ lastUpdated emailType emailPrimary)


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
