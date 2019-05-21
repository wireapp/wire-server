module Galley.Data.LegalHold
    ( createSettings
    , getSettings
    , removeSettings
    ) where

import Imports
import Cassandra
import Data.Id
import Galley.Data.Queries
import Galley.Data.Instances ()

import Brig.Types.Team.LegalHold

-- | Determines whether a given team is allowed to enable/disable legalhold
setEnabled :: MonadClient m => TeamId -> Bool -> m ()
setEnabled tid isLegalHoldEnabled = do
    retry x5 $ write setLegalHoldEnabled (params Quorum (isLegalHoldEnabled, tid))

-- | Returns 'False' if legal hold is not enabled for this team
createSettings :: MonadClient m => LegalHoldService -> m Bool
createSettings (LegalHoldService tid url fpr tok) = do
    results <- retry x1 $ trans insertLegalHoldSettings (params Quorum (tid, url, fpr, tok))
    pure . not . null $ results

-- | Returns 'Nothing' if no settings are saved OR if legal hold is disabled for this team.
getSettings :: MonadClient m => TeamId -> m (Maybe LegalHoldService)
getSettings tid = fmap toLegalHoldService <$> do
     retry x1 $ query1 selectLegalHoldSettings (params Quorum (Identity tid))
  where
    toLegalHoldService (teamId, httpsUrl, fingerprint, tok) =
        LegalHoldService teamId httpsUrl fingerprint tok

removeSettings :: MonadClient m => TeamId -> m ()
removeSettings tid = retry x5 (write removeLegalHoldSettings (params Quorum (Identity tid)))
