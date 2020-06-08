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

module Galley.Data.TeamFeatures
  ( setFlag,
    getFlag,
  )
where

import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Imports
import Wire.API.Team.Feature (TeamFeatureName (..), TeamFeatureStatus (..))

-- | Is a given feature enabled or disabled?  Returns 'Nothing' if team does not exist or the
-- feature flag in Cassandra is null.
getFlag :: MonadClient m => TeamId -> TeamFeatureName -> m (Maybe TeamFeatureStatus)
getFlag tid feature = (>>= runIdentity) <$> retry x1 (query1 (select feature) (params Quorum (Identity tid)))

-- | Enable or disable feature flag.
setFlag :: MonadClient m => TeamId -> TeamFeatureName -> TeamFeatureStatus -> m ()
setFlag tid feature flag = do retry x5 $ write (update feature) (params Quorum (flag, tid))

select :: TeamFeatureName -> PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureStatus))
select feature = fromString $ "select " <> toCol feature <> " from team_features where team_id = ?"

update :: TeamFeatureName -> PrepQuery W (TeamFeatureStatus, TeamId) ()
update feature = fromString $ "update team_features set " <> toCol feature <> " = ? where team_id = ?"

toCol :: TeamFeatureName -> String
toCol TeamFeatureLegalHold = "legalhold_status"
toCol TeamFeatureSSO = "sso_status"
toCol TeamFeatureSearchVisibility = "search_visibility_status"
toCol TeamFeatureValidateSAMLEmails = "validate_saml_emails"
