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

module Wire.CargoholdAPIAccess where

import Polysemy
import Wire.API.Backup (BackupCleanupResult)

data CargoholdAPIAccess m a where
  -- | One batch of the cleanup of orphaned backup files (internal cargohold endpoint).
  CleanupBackups :: CargoholdAPIAccess m BackupCleanupResult

makeSem ''CargoholdAPIAccess
