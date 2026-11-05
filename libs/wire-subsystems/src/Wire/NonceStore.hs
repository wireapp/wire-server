{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.NonceStore where

import Data.Id (UserId)
import Data.Nonce (Nonce, NonceTtlSecs)
import Imports
import Polysemy

data NonceStore m a where
  InsertNonce :: NonceTtlSecs -> UserId -> Text -> Nonce -> NonceStore m ()
  LookupAndDeleteNonce :: UserId -> Text -> NonceStore m (Maybe Nonce)
  DeleteNonce :: UserId -> Text -> NonceStore m ()

makeSem ''NonceStore
