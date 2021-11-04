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

module Galley.Cassandra.LegalHold (isTeamLegalholdWhitelisted) where

import Cassandra
import Data.Id
import Galley.Data.Queries as Q
import Galley.Types.Teams
import Imports

-- | Returns 'False' if legal hold is not enabled for this team
-- The Caller is responsible for checking whether legal hold is enabled for this team
createSettings :: MonadClient m => LegalHoldService -> m ()
createSettings (LegalHoldService tid url fpr tok key) = do
  retry x1 $ write insertLegalHoldSettings (params LocalQuorum (url, fpr, tok, key, tid))

-- | Returns 'Nothing' if no settings are saved
-- The Caller is responsible for checking whether legal hold is enabled for this team
getSettings :: MonadClient m => TeamId -> m (Maybe LegalHoldService)
getSettings tid =
  fmap toLegalHoldService <$> do
    retry x1 $ query1 selectLegalHoldSettings (params LocalQuorum (Identity tid))
  where
    toLegalHoldService (httpsUrl, fingerprint, tok, key) = LegalHoldService tid httpsUrl fingerprint tok key

removeSettings :: MonadClient m => TeamId -> m ()
removeSettings tid = retry x5 (write removeLegalHoldSettings (params LocalQuorum (Identity tid)))

insertPendingPrekeys :: MonadClient m => UserId -> [Prekey] -> m ()
insertPendingPrekeys uid keys = retry x5 . batch $
  forM_ keys $
    \key ->
      addPrepQuery Q.insertPendingPrekeys (toTuple key)
  where
    toTuple (Prekey keyId key) = (uid, keyId, key)

selectPendingPrekeys :: MonadClient m => UserId -> m (Maybe ([Prekey], LastPrekey))
selectPendingPrekeys uid =
  pickLastKey . fmap fromTuple
    <$> retry x1 (query Q.selectPendingPrekeys (params LocalQuorum (Identity uid)))
  where
    fromTuple (keyId, key) = Prekey keyId key
    pickLastKey allPrekeys =
      case unsnoc allPrekeys of
        Nothing -> Nothing
        Just (keys, lst) -> pure (keys, lastPrekey . prekeyKey $ lst)

dropPendingPrekeys :: MonadClient m => UserId -> m ()
dropPendingPrekeys uid = retry x5 (write Q.dropPendingPrekeys (params LocalQuorum (Identity uid)))

setUserLegalHoldStatus :: MonadClient m => TeamId -> UserId -> UserLegalHoldStatus -> m ()
setUserLegalHoldStatus tid uid status =
  retry x5 (write Q.updateUserLegalHoldStatus (params LocalQuorum (status, tid, uid)))

setTeamLegalholdWhitelisted :: MonadClient m => TeamId -> m ()
setTeamLegalholdWhitelisted tid =
  retry x5 (write Q.insertLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid)))

unsetTeamLegalholdWhitelisted :: MonadClient m => TeamId -> m ()
unsetTeamLegalholdWhitelisted tid =
  retry x5 (write Q.removeLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid)))

isTeamLegalholdWhitelisted :: (MonadReader Env m, MonadClient m) => TeamId -> m Bool
isTeamLegalholdWhitelisted tid = do
  lhFlag <- view (options . Opts.optSettings . Opts.setFeatureFlags . flagLegalHold)
  liftClient $ C.isTeamLegalholdWhitelisted lhFlag tid

isTeamLegalholdWhitelisted :: FeatureLegalHold -> TeamId -> Client Bool
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledPermanently _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledByDefault _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldWhitelistTeamsAndImplicitConsent tid =
  isJust <$> (runIdentity <$$> retry x5 (query1 Q.selectLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid))))
