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
    validateKeyPackageData,

    -- * Exported for unit tests
    findExtensions,
    validateLifetime',
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.App
import Brig.Options
import Control.Applicative
import Control.Lens (view)
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Imports
import Wire.API.ErrorDescription
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation

validateKeyPackageData :: ClientIdentity -> KeyPackageData -> Handler r (KeyPackageRef, KeyPackageData)
validateKeyPackageData identity kpd = do
  -- parse key package data
  (kp, tbs) <- parseKeyPackage kpd
  -- get ciphersuite
  cs <-
    maybe
      (throwErrorDescription (mlsProtocolError "Unsupported ciphersuite"))
      pure
      $ cipherSuiteTag (kpCipherSuite (kpTBS kp))
  -- validate signature
  -- FUTUREWORK: authenticate signature key
  let key = bcSignatureKey (kpCredential (kpTBS kp))
  unless (csVerifySignature cs key (LBS.toStrict tbs) (kpSignature kp)) $
    throwErrorDescription (mlsProtocolError "Invalid signature")
  -- validate credential and extensions
  validateKeyPackage identity kp
  pure (kpRef cs kpd, kpd)

-- | Parse a key package, and return parsed structure and signed data.
parseKeyPackage :: KeyPackageData -> Handler r (KeyPackage, LByteString)
parseKeyPackage (kpData -> kpd) = do
  (kp, off) <- either (throwErrorDescription . mlsProtocolError) pure (decodeMLSWith kpSigOffset kpd)
  pure (kp, LBS.take off kpd)

validateKeyPackage :: ClientIdentity -> KeyPackage -> Handler r ()
validateKeyPackage identity (kpTBS -> kp) = do
  maybe
    (throwErrorDescription (mlsProtocolError "Unsupported protocol version"))
    pure
    (pvTag (kpProtocolVersion kp) >>= guard . (== ProtocolMLS10))
  validateCredential identity (kpCredential kp)
  validateExtensions (kpExtensions kp)

validateCredential :: ClientIdentity -> Credential -> Handler r ()
validateCredential identity cred = do
  identity' <-
    either (throwErrorDescription . mlsProtocolError) pure $
      decodeMLS' (bcIdentity cred)
  when (identity /= identity') $
    throwErrorDescriptionType @MLSIdentityMismatch

data RequiredExtensions f = RequiredExtensions
  { reLifetime :: f Lifetime,
    reCapabilities :: f ()
  }

deriving instance (Show (f Lifetime), Show (f ())) => Show (RequiredExtensions f)

instance Alternative f => Semigroup (RequiredExtensions f) where
  RequiredExtensions lt1 cap1 <> RequiredExtensions lt2 cap2 =
    RequiredExtensions (lt1 <|> lt2) (cap1 <|> cap2)

instance Alternative f => Monoid (RequiredExtensions f) where
  mempty = RequiredExtensions empty empty

checkRequiredExtensions :: Applicative f => RequiredExtensions f -> f (RequiredExtensions Identity)
checkRequiredExtensions re =
  RequiredExtensions
    <$> (Identity <$> reLifetime re)
    <*> (Identity <$> reCapabilities re)

findExtensions :: [SomeExtension] -> Maybe (RequiredExtensions Identity)
findExtensions = checkRequiredExtensions . foldMap findExtension

findExtension :: SomeExtension -> RequiredExtensions Maybe
findExtension (SomeExtension SLifetimeExtensionTag lt) = RequiredExtensions (pure lt) Nothing
findExtension (SomeExtension SCapabilitiesExtensionTag _) = RequiredExtensions Nothing (pure ())
findExtension _ = mempty

validateExtensions :: [Extension] -> Handler r ()
validateExtensions exts = do
  re <-
    maybe (throwErrorDescription (mlsProtocolError "Missing required extensions")) pure $
      findExtensions (foldMap (maybeToList . decodeExtension) exts)
  validateLifetime . runIdentity . reLifetime $ re

validateLifetime :: Lifetime -> Handler r ()
validateLifetime lt = do
  now <- liftIO getPOSIXTime
  mMaxLifetime <- setKeyPackageMaximumLifetime <$> view settings
  either (throwErrorDescription . mlsProtocolError) pure $
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
