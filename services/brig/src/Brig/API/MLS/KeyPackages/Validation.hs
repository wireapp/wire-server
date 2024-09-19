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

module Brig.API.MLS.KeyPackages.Validation
  ( -- * Main key package validation function
    validateUploadedKeyPackage,
    validateLifetime',
    mlsProtocolError,
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.App
import Brig.Data.Client qualified as Data
import Brig.Options
import Control.Applicative
import Data.ByteString qualified as LBS
import Data.Qualified
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Imports
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Lifetime
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Validation

validateUploadedKeyPackage ::
  ClientIdentity ->
  RawMLS KeyPackage ->
  Handler r (KeyPackageRef, CipherSuiteTag, KeyPackageData)
validateUploadedKeyPackage identity kp = do
  (cs, lt) <- either mlsProtocolError pure $ validateKeyPackage (Just identity) kp.value

  validateLifetime lt

  -- Authenticate signature key. This is performed only upon uploading a key
  -- package for a local client.
  loc <- qualifyLocal ()
  foldQualified
    loc
    ( \_ -> do
        mkey :: Maybe LByteString <-
          lift . wrapClient $
            Data.lookupMLSPublicKey
              (ciUser identity)
              (ciClient identity)
              (csSignatureScheme cs)
        key :: LByteString <-
          maybe
            (mlsProtocolError "No key associated to the given identity and signature scheme")
            pure
            mkey
        when (key /= LBS.fromStrict kp.value.leafNode.signatureKey) $
          mlsProtocolError "Unrecognised signature key"
    )
    (\_ -> pure ())
    (cidQualifiedClient identity)

  let kpd = KeyPackageData kp.raw
  pure (kpRef cs kpd, cs, kpd)

validateLifetime :: Lifetime -> Handler r ()
validateLifetime lt = do
  now <- liftIO getPOSIXTime
  mMaxLifetime <- setKeyPackageMaximumLifetime <$> asks (.settings)
  either mlsProtocolError pure $
    validateLifetime' now mMaxLifetime lt

validateLifetime' :: POSIXTime -> Maybe NominalDiffTime -> Lifetime -> Either Text ()
validateLifetime' now mMaxLifetime lt = do
  when (tsPOSIX (ltNotBefore lt) > now) $
    Left "Key package not_before date is in the future"
  when (tsPOSIX (ltNotAfter lt) <= now) $
    Left "Key package is expired"
  for_ mMaxLifetime $ \maxLifetime ->
    when (tsPOSIX (ltNotAfter lt) > now + maxLifetime) $
      Left "Key package expiration time is too far in the future"

mlsProtocolError :: Text -> Handler r a
mlsProtocolError msg =
  throwStd . dynErrorToWai $
    (dynError @(MapError 'MLSProtocolError))
      { eMessage = msg
      }
