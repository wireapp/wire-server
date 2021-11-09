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

module Galley.Cassandra.LegalHold
  ( interpretLegalHoldStoreToCassandra,
    isTeamLegalholdWhitelisted,

    -- * Used by tests
    selectPendingPrekeys,
  )
where

import Brig.Types.Client.Prekey
import Brig.Types.Instances ()
import Brig.Types.Team.LegalHold
import Cassandra
import Control.Lens (unsnoc)
import Data.Id
import Data.LegalHold
import Galley.Cassandra.Instances ()
import qualified Galley.Cassandra.Queries as Q
import Galley.Cassandra.Store
import Galley.Effects.LegalHoldStore (LegalHoldStore (..))
import Galley.Env
import Galley.External.LegalHoldService.Internal
import Galley.Monad
import Galley.Types.Teams
import Imports
import Polysemy
import qualified Polysemy.Reader as P

interpretLegalHoldStoreToCassandra ::
  Members '[Embed IO, P.Reader ClientState, P.Reader Env] r =>
  FeatureLegalHold ->
  Sem (LegalHoldStore ': r) a ->
  Sem r a
interpretLegalHoldStoreToCassandra lh = interpret $ \case
  CreateSettings s -> embedClient $ createSettings s
  GetSettings tid -> embedClient $ getSettings tid
  RemoveSettings tid -> embedClient $ removeSettings tid
  InsertPendingPrekeys uid pkeys -> embedClient $ insertPendingPrekeys uid pkeys
  SelectPendingPrekeys uid -> embedClient $ selectPendingPrekeys uid
  DropPendingPrekeys uid -> embedClient $ dropPendingPrekeys uid
  SetUserLegalHoldStatus tid uid st -> embedClient $ setUserLegalHoldStatus tid uid st
  SetTeamLegalholdWhitelisted tid -> embedClient $ setTeamLegalholdWhitelisted tid
  UnsetTeamLegalholdWhitelisted tid -> embedClient $ unsetTeamLegalholdWhitelisted tid
  IsTeamLegalholdWhitelisted tid -> embedClient $ isTeamLegalholdWhitelisted lh tid
  -- FUTUREWORK: should this action be part of a separate effect?
  MakeVerifiedRequestFreshManager fpr url r ->
    embedApp $ makeVerifiedRequestFreshManager fpr url r
  MakeVerifiedRequest fpr url r ->
    embedApp $ makeVerifiedRequest fpr url r

-- | Returns 'False' if legal hold is not enabled for this team
-- The Caller is responsible for checking whether legal hold is enabled for this team
createSettings :: MonadClient m => LegalHoldService -> m ()
createSettings (LegalHoldService tid url fpr tok key) = do
  retry x1 $ write Q.insertLegalHoldSettings (params LocalQuorum (url, fpr, tok, key, tid))

-- | Returns 'Nothing' if no settings are saved
-- The Caller is responsible for checking whether legal hold is enabled for this team
getSettings :: MonadClient m => TeamId -> m (Maybe LegalHoldService)
getSettings tid =
  fmap toLegalHoldService <$> do
    retry x1 $ query1 Q.selectLegalHoldSettings (params LocalQuorum (Identity tid))
  where
    toLegalHoldService (httpsUrl, fingerprint, tok, key) = LegalHoldService tid httpsUrl fingerprint tok key

removeSettings :: MonadClient m => TeamId -> m ()
removeSettings tid = retry x5 (write Q.removeLegalHoldSettings (params LocalQuorum (Identity tid)))

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

isTeamLegalholdWhitelisted :: FeatureLegalHold -> TeamId -> Client Bool
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledPermanently _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledByDefault _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldWhitelistTeamsAndImplicitConsent tid =
  isJust <$> (runIdentity <$$> retry x5 (query1 Q.selectLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid))))
