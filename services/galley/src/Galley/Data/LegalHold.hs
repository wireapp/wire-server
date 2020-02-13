{-# LANGUAGE ViewPatterns #-}

module Galley.Data.LegalHold
  ( setLegalHoldTeamConfig,
    getLegalHoldTeamConfig,
    createSettings,
    getSettings,
    removeSettings,
    Galley.Data.LegalHold.insertPendingPrekeys,
    Galley.Data.LegalHold.selectPendingPrekeys,
    Galley.Data.LegalHold.dropPendingPrekeys,
    setUserLegalHoldStatus,
  )
where

import Brig.Types.Client.Prekey
import Brig.Types.Instances ()
import Brig.Types.Team.LegalHold
import Cassandra
import Control.Lens (unsnoc)
import Data.Id
import Data.LegalHold
import Galley.Data.Instances ()
import Galley.Data.Queries as Q
import Imports

-- | Return whether a given team is allowed to enable/disable legalhold
getLegalHoldTeamConfig :: MonadClient m => TeamId -> m (Maybe LegalHoldTeamConfig)
getLegalHoldTeamConfig tid = fmap toLegalHoldTeamConfig <$> do
  retry x1 $ query1 selectLegalHoldTeamConfig (params Quorum (Identity tid))
  where
    toLegalHoldTeamConfig (Identity Nothing) = LegalHoldTeamConfig LegalHoldDisabled
    toLegalHoldTeamConfig (Identity (Just status)) = LegalHoldTeamConfig status

-- | Determines whether a given team is allowed to enable/disable legalhold
setLegalHoldTeamConfig :: MonadClient m => TeamId -> LegalHoldTeamConfig -> m ()
setLegalHoldTeamConfig tid LegalHoldTeamConfig {legalHoldTeamConfigStatus} = do
  retry x5 $ write updateLegalHoldTeamConfig (params Quorum (legalHoldTeamConfigStatus, tid))

-- | Returns 'False' if legal hold is not enabled for this team
-- The Caller is responsible for checking whether legal hold is enabled for this team
createSettings :: MonadClient m => LegalHoldService -> m ()
createSettings (LegalHoldService tid url fpr tok key) = do
  retry x1 $ write insertLegalHoldSettings (params Quorum (url, fpr, tok, key, tid))

-- | Returns 'Nothing' if no settings are saved
-- The Caller is responsible for checking whether legal hold is enabled for this team
getSettings :: MonadClient m => TeamId -> m (Maybe LegalHoldService)
getSettings tid = fmap toLegalHoldService <$> do
  retry x1 $ query1 selectLegalHoldSettings (params Quorum (Identity tid))
  where
    toLegalHoldService (httpsUrl, fingerprint, tok, key) = LegalHoldService tid httpsUrl fingerprint tok key

removeSettings :: MonadClient m => TeamId -> m ()
removeSettings tid = retry x5 (write removeLegalHoldSettings (params Quorum (Identity tid)))

insertPendingPrekeys :: MonadClient m => UserId -> [Prekey] -> m ()
insertPendingPrekeys uid keys = retry x5 . batch
  $ forM_ keys
  $ \key ->
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

setUserLegalHoldStatus :: MonadClient m => TeamId -> UserId -> UserLegalHoldStatus -> m ()
setUserLegalHoldStatus tid uid status =
  retry x5 (write Q.updateUserLegalHoldStatus (params Quorum (status, tid, uid)))
