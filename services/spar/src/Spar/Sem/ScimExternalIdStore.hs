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

module Spar.Sem.ScimExternalIdStore
  ( ScimExternalIdStore (..),
    insert,
    lookup,
    delete,
    insertStatus,
    lookupStatus,
  )
where

import Data.Id (TeamId, UserId)
import Imports (Maybe, Show)
import Polysemy
import Polysemy.Check (deriveGenericK)
import Spar.Scim.Types
import Wire.API.User.Identity
import Wire.API.User.Scim

data ScimExternalIdStore m a where
  Insert :: TeamId -> EmailAddress -> UserId -> ScimExternalIdStore m ()
  Lookup :: TeamId -> EmailAddress -> ScimExternalIdStore m (Maybe UserId)
  Delete :: TeamId -> EmailAddress -> ScimExternalIdStore m ()
  -- NB: the fact that we are using `Email` in some cases here and `ValidExternalId` in others has historical reasons (this table was only used for non-saml accounts in the past, now it is used for *all* scim-managed accounts).  the interface would work equally well with just `Text` here (for unvalidated scim external id).
  InsertStatus :: TeamId -> ValidExternalId -> UserId -> ScimUserCreationStatus -> ScimExternalIdStore m ()
  LookupStatus :: TeamId -> ValidExternalId -> ScimExternalIdStore m (Maybe (UserId, ScimUserCreationStatus))

deriving instance Show (ScimExternalIdStore m a)

makeSem ''ScimExternalIdStore
deriveGenericK ''ScimExternalIdStore
