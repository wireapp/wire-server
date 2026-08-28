{-# LANGUAGE TemplateHaskell #-}

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

module Wire.ScimUserMetaStore
  ( ScimUserMeta (..),
    ScimUserMetaStore (..),
    write,
    read,
    readMulti,
    delete,
  )
where

import Data.Id (UserId)
import Data.Json.Util (UTCTimeMillis)
import Imports (Bool, Eq, Maybe, Show, Text)
import Polysemy
import Web.Scim.Schema.Common (WithId)
import Web.Scim.Schema.Meta (WithMeta)

-- | SCIM user metadata stored under a user id: creation and last-update time,
-- plus the SCIM email metadata (@type@, @primary@) of the stored email entry.
--
-- The backing Cassandra table is still called @spar.scim_user_times@ (renaming
-- it would require a migration); the store is no longer just about times.
data ScimUserMeta = ScimUserMeta
  { scimUserMetaCreated :: UTCTimeMillis,
    scimUserMetaLastUpdated :: UTCTimeMillis,
    scimUserMetaEmailType :: Maybe Text,
    scimUserMetaEmailPrimary :: Maybe Bool
  }
  deriving (Eq, Show)

data ScimUserMetaStore m a where
  Write :: Maybe Text -> Maybe Bool -> WithMeta (WithId UserId t) -> ScimUserMetaStore m ()
  Read :: UserId -> ScimUserMetaStore m (Maybe ScimUserMeta)
  ReadMulti :: [UserId] -> ScimUserMetaStore m [(UserId, ScimUserMeta)]
  Delete :: UserId -> ScimUserMetaStore m ()

makeSem ''ScimUserMetaStore
