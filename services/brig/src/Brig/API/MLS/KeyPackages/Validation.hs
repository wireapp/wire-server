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
    reLifetime,
    mlsProtocolError,

    -- * Exported for unit tests
    findExtensions,
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
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Imports
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation


-- TODO: remove Handler, use Sem instead
-- with a timestamp, errors and settings constraint.
validateKeyPackage :: ClientIdentity -> RawMLS KeyPackage -> Handler r (KeyPackageRef, KeyPackageData)
validateKeyPackage identity (RawMLS (KeyPackageData -> kpd) kp) = do
  -- get ciphersuite
  cs <-
    maybe
      (mlsProtocolError "Unsupported ciphersuite")
      pure
      $ cipherSuiteTag (kpCipherSuite kp)

  -- validate signature scheme
  let ss = csSignatureScheme cs
  when (signatureScheme ss /= bcSignatureScheme (kpCredential kp)) $
    mlsProtocolError "Signature scheme incompatible with ciphersuite"

  -- authenticate signature key
  key <-
    fmap LBS.toStrict $
      maybe
        (mlsProtocolError "No key associated to the given identity and signature scheme")
        pure
        =<< lift (wrapClient (Data.lookupMLSPublicKey (ciUser identity) (ciClient identity) ss))
  when (key /= bcSignatureKey (kpCredential kp)) $
    mlsProtocolError "Unrecognised signature key"

  -- validate signature
  unless (csVerifySignature cs key (rmRaw (kpTBS kp)) (kpSignature kp)) $
    mlsProtocolError "Invalid signature"
  -- validate protocol version
  maybe
    (mlsProtocolError "Unsupported protocol version")
    pure
    (pvTag (kpProtocolVersion kp) >>= guard . (== ProtocolMLS10))
  -- validate credential
  validateCredential identity (kpCredential kp)
  -- validate extensions
  validateExtensions (kpExtensions kp)
  pure (kpRef cs kpd, kpd)

validateCredential :: ClientIdentity -> Credential -> Handler r ()
validateCredential identity cred = do
  identity' <-
    either mlsProtocolError pure $
      decodeMLS' (bcIdentity cred)
  when (identity /= identity') $
    throwStd (errorToWai @'MLSIdentityMismatch)

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

data MissingExtensionError
  = MELifetime
  | MECapability

checkRequiredExtensions :: RequiredExtensions Maybe -> Either Text (RequiredExtensions Identity)
checkRequiredExtensions re =
  RequiredExtensions
    <$> maybe (Left "Missing lifetime extension") (pure . Identity) (reLifetime re)
    <*> maybe (Left "Missing capability extension") (pure . Identity) (reCapabilities re)

checkRequiredExtensions' :: RequiredExtensions Maybe -> Either MissingExtensionError (RequiredExtensions Identity)
checkRequiredExtensions' re =
  RequiredExtensions
    <$> maybe (Left MELifetime) (pure . Identity) (reLifetime re)
    <*> maybe (Left MECapability) (pure . Identity) (reCapabilities re)

findExtensions :: [Extension] -> Either Text (RequiredExtensions Identity)
findExtensions = checkRequiredExtensions <=< (getAp . foldMap findExtension)

findExtensions' :: [Extension] -> Either ('MLSDecodingError :: k) (RequiredExtensions Identity)
findExtensions' = checkRequiredExtensions' <=< (getAp . foldMap findExtension')

findExtension :: Extension -> Ap (Either Text) (RequiredExtensions Maybe)
findExtension ext = (Ap (decodeExtension ext) >>=) . foldMap $ \case
  (SomeExtension SLifetimeExtensionTag lt) -> pure $ RequiredExtensions (Just lt) Nothing
  (SomeExtension SCapabilitiesExtensionTag _) -> pure $ RequiredExtensions Nothing (Just ())

findExtension' :: Extension -> Ap (Either ('MLSDecodingError :: k)) (RequiredExtensions Maybe)
findExtension' ext = (Ap (first (const MLSDecodingError) . decodeExtension ext) >>=) . foldMap $ \case
  (SomeExtension SLifetimeExtensionTag lt) -> pure $ RequiredExtensions (Just lt) Nothing
  (SomeExtension SCapabilitiesExtensionTag _) -> pure $ RequiredExtensions Nothing (Just ())

-- TODO: only needs the same constraints as validateLifetime
validateExtensions :: [Extension] -> Handler r ()
validateExtensions exts = do
  re <- either mlsProtocolError pure $ findExtensions exts
  validateLifetime . runIdentity . reLifetime $ re

-- TODO: needs settings and current time from Sem, no need to be in Handler m a
validateLifetime :: Lifetime -> Handler r ()
validateLifetime lt = do
  now <- liftIO getPOSIXTime -- use Sem's get time
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

mlsProtocolError :: Text -> Handler r a
mlsProtocolError msg =
  throwStd . toWai $
    (dynError @(MapError 'MLSProtocolError))
      { eMessage = msg
      }
