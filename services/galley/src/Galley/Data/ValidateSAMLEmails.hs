{-# LANGUAGE ViewPatterns #-}

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

module Galley.Data.ValidateSAMLEmails
  ( setValidateSAMLEmails,
    getValidateSAMLEmails,
  )
where

import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Galley.Data.Queries
import Imports
import Wire.API.Team.Feature (TeamFeatureStatus (..))

getValidateSAMLEmails :: MonadClient m => TeamId -> m (Maybe TeamFeatureStatus)
getValidateSAMLEmails tid = fmap toFeatureStatus <$> do
  retry x1 $ query1 selectValidateSAMLEmails (params Quorum (Identity tid))
  where
    toFeatureStatus (Identity Nothing) = TeamFeatureDisabled
    toFeatureStatus (Identity (Just status)) = status

setValidateSAMLEmails :: MonadClient m => TeamId -> TeamFeatureStatus -> m ()
setValidateSAMLEmails tid featureStatus = do
  retry x5 $ write updateValidateSAMLEmails (params Quorum (featureStatus, tid))
