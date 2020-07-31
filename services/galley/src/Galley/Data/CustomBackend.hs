{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Data.CustomBackend
  ( getCustomBackend,
    setCustomBackend,
    deleteCustomBackend,
  )
where

import Cassandra
import Data.Domain (Domain)
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Galley.Types
import Imports

getCustomBackend :: MonadClient m => Domain -> m (Maybe CustomBackend)
getCustomBackend domain =
  fmap toCustomBackend <$> do
    retry x1 $ query1 Cql.selectCustomBackend (params Quorum (Identity domain))
  where
    toCustomBackend (backendConfigJsonUrl, backendWebappWelcomeUrl) =
      CustomBackend {..}

setCustomBackend :: MonadClient m => Domain -> CustomBackend -> m ()
setCustomBackend domain CustomBackend {..} = do
  retry x5 $ write Cql.updateCustomBackend (params Quorum (backendConfigJsonUrl, backendWebappWelcomeUrl, domain))

deleteCustomBackend :: MonadClient m => Domain -> m ()
deleteCustomBackend domain = do
  retry x5 $ write Cql.deleteCustomBackend (params Quorum (Identity domain))
