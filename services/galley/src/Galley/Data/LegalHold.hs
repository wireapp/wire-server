{-# LANGUAGE ViewPatterns #-}
module Galley.Data.LegalHold
    ( setLegalHoldTeamConfig
    , getLegalHoldTeamConfig
    , createSettings
    , getSettings
    , removeSettings
    , Galley.Data.LegalHold.insertPendingPrekeys
    , Galley.Data.LegalHold.selectPendingPrekeys
    , Galley.Data.LegalHold.dropPendingPrekeys
    , getUserLegalHoldStatus
    , setUserLegalHoldStatus
    ) where

import Imports
import Cassandra
import Control.Lens (unsnoc)
import Data.Id
import Brig.Types.Client.Prekey
import Galley.Data.Queries as Q
import Galley.Data.Instances ()
import Brig.Types.Instances ()

import Brig.Types.Team.LegalHold

-- | Return whether a given team is allowed to enable/disable legalhold
getLegalHoldTeamConfig :: MonadClient m => TeamId -> m (Maybe LegalHoldTeamConfig)
getLegalHoldTeamConfig tid = fmap toLegalHoldTeamConfig <$> do
    retry x1 $ query1 selectLegalHoldTeamConfig (params Quorum (Identity tid))
  where
    toLegalHoldTeamConfig (Identity status) = LegalHoldTeamConfig status

-- | Determines whether a given team is allowed to enable/disable legalhold
setLegalHoldTeamConfig :: MonadClient m => TeamId -> LegalHoldTeamConfig -> m ()
setLegalHoldTeamConfig tid LegalHoldTeamConfig{legalHoldTeamConfigStatus} = do
    retry x5 $ write updateLegalHoldTeamConfig (params Quorum (legalHoldTeamConfigStatus, tid))

-- | Returns 'False' if legal hold is not enabled for this team
-- The Caller is responsible for checking whether legal hold is enabled for this team
createSettings :: MonadClient m => LegalHoldService -> m ()
createSettings (LegalHoldService tid url fpr tok) = do
    retry x1 $ write insertLegalHoldSettings (params Quorum (url, fpr, tok, tid))

-- | Returns 'Nothing' if no settings are saved
-- The Caller is responsible for checking whether legal hold is enabled for this team
getSettings :: MonadClient m => TeamId -> m (Maybe LegalHoldService)
getSettings tid = fmap toLegalHoldService <$> do
    retry x1 $ query1 selectLegalHoldSettings (params Quorum (Identity tid))
  where
    toLegalHoldService (httpsUrl, fingerprint, tok) = LegalHoldService tid httpsUrl fingerprint tok

removeSettings :: MonadClient m => TeamId -> m ()
removeSettings tid = retry x5 (write removeLegalHoldSettings (params Quorum (Identity tid)))

insertPendingPrekeys :: MonadClient m => UserId -> [Prekey] -> m ()
insertPendingPrekeys uid keys = retry x5 . batch $
    forM_ keys $ \key ->
        addPrepQuery Q.insertPendingPrekeys (toTuple key)
  where
    toTuple (Prekey keyId key) = (uid, keyId, key)

selectPendingPrekeys :: MonadClient m => UserId -> m (Maybe ([Prekey], LastPrekey))
selectPendingPrekeys uid =
    pickLastKey . fmap fromTuple
    <$> retry x1 (query Q.selectPendingPrekeys (params Quorum (Identity uid)))
  where
    fromTuple (keyId, key) = Prekey keyId key
    pickLastKey allPrekeys =
        case unsnoc allPrekeys of
            Nothing -> Nothing
            Just (keys, lst) -> pure (keys, lastPrekey . prekeyKey $ lst)

dropPendingPrekeys :: MonadClient m => UserId -> m ()
dropPendingPrekeys uid = retry x5 (write Q.dropPendingPrekeys (params Quorum (Identity uid)))

getUserLegalHoldStatus :: MonadClient m => UserId -> m UserLegalHoldStatusResponse
getUserLegalHoldStatus uid = do
    result <- retry x1 (query1 Q.selectUserLegalHoldStatus (params Quorum (Identity uid)))
    pure $ case result of
        Nothing -> UserLegalHoldStatusResponse UserLegalHoldDisabled Nothing
        Just (status, fingerprint) -> UserLegalHoldStatusResponse status Nothing

setUserLegalHoldStatus :: MonadClient m => UserId -> UserLegalHoldStatus -> m ()
setUserLegalHoldStatus uid status =
    retry x5 (write Q.updateUserLegalHoldStatus (params Quorum (status, uid)))
