-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.API.MLS.KeyPackages
  ( uploadKeyPackages,
    claimKeyPackages,
    countKeyPackages,
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.API.MLS.KeyPackages.Validation
import Brig.App
import qualified Brig.Data.Client as Data
import qualified Brig.Data.MLS.KeyPackage as Data
import Brig.IO.Intra
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Id
import Data.Qualified
import qualified Data.Set as Set
import Imports
import Wire.API.Federation.Error
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.Team.LegalHold
import Wire.API.User.Client

uploadKeyPackages :: Local UserId -> ClientId -> KeyPackageUpload -> Handler r ()
uploadKeyPackages lusr cid (kpuKeyPackages -> kps) = do
  let identity = mkClientIdentity (qUntagged lusr) cid
  kps' <- traverse (validateKeyPackage identity) kps
  lift . wrapClient $ Data.insertKeyPackages (tUnqualified lusr) cid kps'

claimKeyPackages :: Local UserId -> Qualified UserId -> Handler r KeyPackageBundle
claimKeyPackages lusr =
  foldQualified
    lusr
    (claimLocalKeyPackages lusr)
    (\_ -> throwStd federationNotImplemented)

claimLocalKeyPackages :: Local UserId -> Local UserId -> Handler r KeyPackageBundle
claimLocalKeyPackages lusr target = do
  clients <- map clientId <$> wrapClientE (Data.lookupClients (tUnqualified target))
  withExceptT clientError $
    wrapHttpClientE $ guardLegalhold (ProtectedUser (tUnqualified lusr)) (mkUserClients [(tUnqualified target, clients)])
  lift $
    KeyPackageBundle . Set.fromList . catMaybes <$> traverse mkEntry clients
  where
    mkEntry :: ClientId -> AppIO r (Maybe KeyPackageBundleEntry)
    mkEntry c =
      runMaybeT $
        uncurry (KeyPackageBundleEntry (qUntagged target) c)
          <$> wrapClientM (Data.claimKeyPackage target c)

countKeyPackages :: Local UserId -> ClientId -> Handler r KeyPackageCount
countKeyPackages lusr c =
  lift $
    KeyPackageCount . fromIntegral
      <$> wrapClient (Data.countKeyPackages (tUnqualified lusr) c)
