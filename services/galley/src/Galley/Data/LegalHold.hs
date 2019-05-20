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

createSettings :: MonadClient m => LegalHoldService -> m ()
createSettings (LegalHoldService tid url fpr tok) = do
    retry x5 $ write insertLegalHoldSettings (params Quorum (tid, url, fpr, tok))

getSettings :: MonadClient m => TeamId -> m (Maybe LegalHoldService)
getSettings tid = fmap toLegalHoldService <$> do
     retry x1 $ query1 selectLegalHoldSettings (params Quorum (Identity tid))
  where
    toLegalHoldService (teamId, httpsUrl, fingerprint, tok) =
        LegalHoldService teamId httpsUrl fingerprint tok

removeSettings :: MonadClient m => TeamId -> m ()
removeSettings tid = retry x5 (write removeLegalHoldSettings (params Quorum (Identity tid)))
