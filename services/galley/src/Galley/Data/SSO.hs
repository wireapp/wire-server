{-# LANGUAGE ViewPatterns #-}
module Galley.Data.SSO
    ( setSSOTeamConfig
    , getSSOTeamConfig
    ) where

import Imports
import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Galley.Types.Teams.SSO
import Galley.Data.Queries

-- | Return whether a given team is allowed to enable/disable sso
getSSOTeamConfig :: MonadClient m => TeamId -> m (Maybe SSOTeamConfig)
getSSOTeamConfig tid = fmap toSSOTeamConfig <$> do
    retry x1 $ query1 selectSSOTeamConfig (params Quorum (Identity tid))
  where
    toSSOTeamConfig (Identity Nothing)       = SSOTeamConfig SSODisabled
    toSSOTeamConfig (Identity (Just status)) = SSOTeamConfig status

-- | Determines whether a given team is allowed to enable/disable sso
setSSOTeamConfig :: MonadClient m => TeamId -> SSOTeamConfig -> m ()
setSSOTeamConfig tid SSOTeamConfig{ssoTeamConfigStatus} = do
    retry x5 $ write updateSSOTeamConfig (params Quorum (ssoTeamConfigStatus, tid))
