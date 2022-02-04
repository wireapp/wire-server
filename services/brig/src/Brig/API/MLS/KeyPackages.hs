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

module Brig.API.MLS.KeyPackages where

import Brig.API.Error
import Brig.API.Handler
import qualified Brig.Data.MLS.KeyPackage as Data
import Control.Monad.Trans.Except
import Data.Id
import Data.Json.Util
import Data.Qualified
import Imports
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation

uploadKeyPackages :: Local UserId -> ClientId -> KeyPackageUpload -> Handler r ()
uploadKeyPackages lusr cid (kpuKeyPackages -> kps) = do
  let identity = mkClientIdentity (qUntagged lusr) cid
  traverse_ (validateKeyPackageData identity) kps
  lift $ Data.insertKeyPackages (tUnqualified lusr) cid kps

validateKeyPackageData :: ClientIdentity -> KeyPackageData -> Handler r ()
validateKeyPackageData identity = parseKeyPackage >=> validateKeyPackage identity

parseKeyPackage :: KeyPackageData -> Handler r KeyPackage
parseKeyPackage (fromBase64ByteString . kpData -> kpd) =
  either (throwE . mlsProtocolError) pure (decodeMLS kpd)

validateKeyPackage :: ClientIdentity -> KeyPackage -> Handler r ()
validateKeyPackage identity (kpTBS -> kp) = do
  validateCredential identity (kpCredential kp)

validateCredential :: ClientIdentity -> Credential -> Handler r ()
validateCredential identity cred = do
  -- TODO: validate signature
  identity' <-
    either (throwE . mlsProtocolError) pure $
      decodeMLS' (bcIdentity cred)
  when (identity /= identity') $
    throwE mlsIdentityMismatch
