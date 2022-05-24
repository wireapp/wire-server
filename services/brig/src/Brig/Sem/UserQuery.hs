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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brig.Sem.UserQuery where

import Brig.Password
import Brig.Types
import Brig.Types.Intra
import Data.Handle
import Data.Id
import Data.Json.Util
import Imports
import Polysemy

type Activated = Bool

type UserRow =
  ( UserId,
    Name,
    Maybe Pict,
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe [Asset],
    Activated,
    Maybe AccountStatus,
    Maybe UTCTimeMillis,
    Maybe Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    Maybe ManagedBy
  )

-- Represents a 'UserAccount'
type AccountRow =
  ( UserId,
    Name,
    Maybe Pict,
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe [Asset],
    Activated,
    Maybe AccountStatus,
    Maybe UTCTimeMillis,
    Maybe Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    Maybe ManagedBy
  )

type UserRowInsert =
  ( UserId,
    Name,
    Pict,
    [Asset],
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe Password,
    Activated,
    AccountStatus,
    Maybe UTCTimeMillis,
    Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    ManagedBy
  )

deriving instance Show UserRowInsert

data UserQuery m a where
  GetId :: UserId -> UserQuery m (Maybe UserId) -- idSelect
  GetUsers :: [UserId] -> UserQuery m [UserRow] -- usersSelect
  GetName :: UserId -> UserQuery m (Maybe Name) -- nameSelect
  GetLocale :: UserId -> UserQuery m (Maybe (Maybe Language, Maybe Country)) -- localeSelect
  GetAuthentication :: UserId -> UserQuery m (Maybe (Maybe Password, Maybe AccountStatus)) -- authSelect
  GetPassword :: UserId -> UserQuery m (Maybe Password) -- passwordSelect
  GetActivated :: UserId -> UserQuery m Bool -- activatedSelect
  GetAccountStatus :: UserId -> UserQuery m (Maybe AccountStatus) -- statusSelect
  GetAccountStatuses :: [UserId] -> UserQuery m [(UserId, Bool, Maybe AccountStatus)] -- accountStateSelectAll
  GetTeam :: UserId -> UserQuery m (Maybe TeamId) -- teamSelect
  GetAccounts :: [UserId] -> UserQuery m [AccountRow] -- accountsSelect
  -- FUTUREWORK: The 'InsertAccount' action should perhaps be in an account store effect
  InsertAccount ::
    UserAccount ->
    -- | If a bot: conversation and team
    --   (if a team conversation)
    Maybe (ConvId, Maybe TeamId) ->
    Maybe Password ->
    -- | Whether the user is activated
    Bool ->
    UserQuery m ()
  UpdateUser :: UserId -> UserUpdate -> UserQuery m ()

makeSem ''UserQuery
