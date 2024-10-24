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

module Spar.Sem.ScimTokenStore
  ( ScimTokenStore (..),
    insert,
    lookup,
    lookupByTeam,
    updateName,
    delete,
    deleteByTeam,
  )
where

import Data.Id
import Imports hiding (lookup)
import Polysemy
import Wire.API.User.Scim

data ScimTokenStore m a where
  Insert :: ScimToken -> ScimTokenInfo -> ScimTokenStore m ()
  Lookup :: ScimToken -> ScimTokenStore m (Maybe ScimTokenInfo)
  LookupByTeam :: TeamId -> ScimTokenStore m [ScimTokenInfo]
  UpdateName ::TeamId -> ScimTokenId -> Text -> ScimTokenStore m ()
  Delete :: TeamId -> ScimTokenId -> ScimTokenStore m ()
  DeleteByTeam :: TeamId -> ScimTokenStore m ()

makeSem ''ScimTokenStore
