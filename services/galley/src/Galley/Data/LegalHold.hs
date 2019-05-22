module Galley.Data.LegalHold
    ( setEnabled
    , getEnabled
    , createSettings
    , getSettings
    , removeSettings
    , LegalHoldNotAvailable(..)
    ) where

import Imports
import Cassandra
import Data.Id
import Galley.Data.Queries
import Galley.Data.Instances ()

import Brig.Types.Team.LegalHold

-- | Return whether a given team is allowed to enable/disable legalhold
getEnabled :: MonadClient m => TeamId -> m Bool
getEnabled tid = maybe False runIdentity <$> do
    retry x1 $ query1 getLegalHoldEnabled (params Quorum (Identity tid))

-- | Determines whether a given team is allowed to enable/disable legalhold
setEnabled :: MonadClient m => TeamId -> Bool -> m ()
setEnabled tid isLegalHoldEnabled = do
    retry x5 $ write setLegalHoldEnabled (params Quorum (isLegalHoldEnabled, tid))

-- | Returns 'False' if legal hold is not enabled for this team
-- The Caller is responsible for checking whether legal hold is enabled for this team
createSettings :: MonadClient m => LegalHoldService -> m ()
createSettings (LegalHoldService tid url fpr tok) = do
    retry x1 $ write insertLegalHoldSettings (params Quorum (url, fpr, tok, tid))

data LegalHoldNotAvailable = LegalHoldDisabledForTeam | LegalHoldServiceNotRegistered
    deriving (Show, Eq)

-- | Returns 'Nothing' if no settings are saved
-- The Caller is responsible for checking whether legal hold is enabled for this team
getSettings :: MonadClient m => TeamId -> m (Maybe LegalHoldService)
getSettings tid = fmap toLegalHoldService <$> do
    retry x1 $ query1 selectLegalHoldSettings (params Quorum (Identity tid))
  where
    toLegalHoldService (httpsUrl, fingerprint, tok) = LegalHoldService tid httpsUrl fingerprint tok

removeSettings :: MonadClient m => TeamId -> m ()
removeSettings tid = retry x5 (write removeLegalHoldSettings (params Quorum (Identity tid)))
