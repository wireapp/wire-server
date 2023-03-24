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
    validateKeyPackage,
    validateLeafNode,
    mlsProtocolError,
    validateLifetime',
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.App
import qualified Brig.Data.Client as Data
import Brig.Options
import Control.Applicative
import Control.Lens (view)
import qualified Data.ByteString.Lazy as LBS
import Data.Qualified
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Imports
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.MLS.Capabilities
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Lifetime
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation

validateKeyPackage ::
  ClientIdentity ->
  RawMLS KeyPackage ->
  Handler r (KeyPackageRef, KeyPackageData)
validateKeyPackage identity (RawMLS (KeyPackageData -> kpd) kp) = do
  loc <- qualifyLocal ()
  -- get ciphersuite
  cs <-
    maybe
      (mlsProtocolError "Unsupported ciphersuite")
      pure
      $ cipherSuiteTag kp.cipherSuite

  let ss = csSignatureScheme cs

  -- Authenticate signature key. This is performed only upon uploading a key
  -- package for a local client.
  foldQualified
    loc
    ( \_ -> do
        key <-
          fmap LBS.toStrict $
            maybe
              (mlsProtocolError "No key associated to the given identity and signature scheme")
              pure
              =<< lift (wrapClient (Data.lookupMLSPublicKey (ciUser identity) (ciClient identity) ss))
        when (key /= kp.leafNode.signatureKey) $
          mlsProtocolError "Unrecognised signature key"
    )
    (pure . const ())
    (cidQualifiedClient identity)

  -- validate signature
  unless
    ( csVerifySignatureWithLabel
        cs
        kp.leafNode.signatureKey
        "KeyPackageTBS"
        kp.tbs
        kp.signature_
    )
    $ mlsProtocolError "Invalid signature"
  -- validate protocol version
  maybe
    (mlsProtocolError "Unsupported protocol version")
    pure
    (pvTag (kp.protocolVersion) >>= guard . (== ProtocolMLS10))

  -- validate credential, lifetime and capabilities
  validateLeafNode identity kp.leafNode

  pure (kpRef cs kpd, kpd)

validateLeafNode :: ClientIdentity -> LeafNode -> Handler r ()
validateLeafNode identity leafNode = do
  validateCredential identity leafNode.credential
  validateSource leafNode.source
  validateCapabilities leafNode.capabilities

validateCredential :: ClientIdentity -> Credential -> Handler r ()
validateCredential identity (BasicCredential cred) = do
  identity' <-
    either credentialError pure $
      decodeMLS' cred
  when (identity /= identity') $
    throwStd (errorToWai @'MLSIdentityMismatch)
  where
    credentialError e =
      mlsProtocolError $
        "Failed to parse identity: " <> e

validateSource :: LeafNodeSource -> Handler r ()
validateSource (LeafNodeSourceKeyPackage lt) = validateLifetime lt
validateSource s =
  mlsProtocolError $
    "Expected 'key_package' source, got '"
      <> (leafNodeSourceTag s).name
      <> "'"

validateLifetime :: Lifetime -> Handler r ()
validateLifetime lt = do
  now <- liftIO getPOSIXTime
  mMaxLifetime <- setKeyPackageMaximumLifetime <$> view settings
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

validateCapabilities :: Capabilities -> Handler r ()
validateCapabilities _ = pure ()

mlsProtocolError :: Text -> Handler r a
mlsProtocolError msg =
  throwStd . toWai $
    (dynError @(MapError 'MLSProtocolError))
      { eMessage = msg
      }
