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
createSettings :: MonadClient m => LegalHoldService -> m Bool
createSettings (LegalHoldService tid url fpr tok) = do
    results <- retry x1 $ trans insertLegalHoldSettings (params Quorum (url, fpr, tok, tid))
    pure . not . null $ results

data LegalHoldNotAvailable = LegalHoldDisabledForTeam | LegalHoldServiceNotRegistered
    deriving (Show, Eq)

-- | Returns 'Nothing' if no settings are saved OR if legal hold is disabled for this team.
getSettings :: MonadClient m => TeamId -> m (Either LegalHoldNotAvailable LegalHoldService)
getSettings tid = do
    mValues <- retry x1 $ query1 selectLegalHoldSettings (params Quorum (Identity tid))
    pure $ case mValues of
        Nothing -> Left LegalHoldDisabledForTeam
        Just (_, _, _, _enabled@False) -> Left LegalHoldDisabledForTeam
        Just (Just httpsUrl, Just fingerprint, Just tok, _enabled@True)
          -> Right $ LegalHoldService tid httpsUrl fingerprint tok
        _ -> Left LegalHoldServiceNotRegistered

removeSettings :: MonadClient m => TeamId -> m ()
removeSettings tid = retry x5 (write removeLegalHoldSettings (params Quorum (Identity tid)))
