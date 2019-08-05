{-# LANGUAGE ViewPatterns #-}
module Galley.Data.Teams
    ( setSSOTeamConfig
    , getSSOTeamConfig
    ) where

import Imports
import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Galley.Types.Teams.Feature

-- | Return whether a given team is allowed to enable/disable sso
getSSOTeamConfig :: MonadClient m => TeamId -> m (Maybe SSOTeamConfig)
getSSOTeamConfig tid = fmap toLegalHoldTeamConfig <$> do
    retry x1 $ query1 selectSSOTeamConfig (params Quorum (Identity tid))
  where
    toLegalHoldTeamConfig (Identity status) = SSOTeamConfig status

-- | Determines whether a given team is allowed to enable/disable sso
setSSOTeamConfig :: MonadClient m => TeamId -> SSOTeamConfig -> m ()
setSSOTeamConfig tid SSOTeamConfig{ssoTeamConfigStatus} = do
    retry x5 $ write updateSSOTeamConfig (params Quorum (ssoTeamConfigStatus, tid))

selectSSOTeamConfig :: PrepQuery R (Identity TeamId) (Identity SSOStatus)
selectSSOTeamConfig =
  "select sso_status from team_features where team_id = ?"

updateSSOTeamConfig :: PrepQuery W (SSOStatus, TeamId) ()
updateSSOTeamConfig =
  "update team_features set sso_status = ? where team_id = ?"
