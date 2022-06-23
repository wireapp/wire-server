{-# LANGUAGE OverloadedStrings #-}

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

module Brig.Types.User
  ( ManagedByUpdate (..),
    RichInfoUpdate (..),
    PasswordResetPair,
    HavePendingInvitations (..),
  )
where

import Data.Aeson
import Imports
import Wire.API.User
import Wire.API.User.Password
import Wire.API.User.RichInfo

newtype ManagedByUpdate = ManagedByUpdate {mbuManagedBy :: ManagedBy} deriving (Eq, Show, Generic)

data HavePendingInvitations
  = WithPendingInvitations
  | NoPendingInvitations
  deriving (Eq, Show, Generic)

newtype RichInfoUpdate = RichInfoUpdate {riuRichInfo :: RichInfoAssocList} deriving (Eq, Show, Generic)

instance FromJSON ManagedByUpdate where
  parseJSON = withObject "managed-by-update" $ \o ->
    ManagedByUpdate <$> o .: "managed_by"

instance ToJSON ManagedByUpdate where
  toJSON m = object ["managed_by" .= mbuManagedBy m]

instance FromJSON RichInfoUpdate where
  parseJSON = withObject "rich-info-update" $ \o ->
    RichInfoUpdate <$> o .: "rich_info"

instance ToJSON RichInfoUpdate where
  toJSON (RichInfoUpdate rif) = object ["rich_info" .= rif]

type PasswordResetPair = (PasswordResetKey, PasswordResetCode)
