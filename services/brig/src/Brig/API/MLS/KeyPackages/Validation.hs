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

module Brig.API.MLS.KeyPackages.Validation (validateKeyPackageData) where

import Brig.API.Error
import Brig.API.Handler
import Control.Applicative
import Imports
import Wire.API.ErrorDescription
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation

validateKeyPackageData :: ClientIdentity -> KeyPackageData -> Handler r (KeyPackageRef, KeyPackageData)
validateKeyPackageData identity kpd = do
  kp <- parseKeyPackage kpd
  validateKeyPackage identity kp
  kdf <-
    maybe
      (throwErrorDescription (mlsProtocolError "Unsupported ciphersuite"))
      pure
      $ cipherSuiteKDF (kpCipherSuite (kpTBS kp))
  pure (kpRef kdf kpd, kpd)

parseKeyPackage :: KeyPackageData -> Handler r KeyPackage
parseKeyPackage (kpData -> kpd) =
  either (throwErrorDescription . mlsProtocolError) pure (decodeMLS kpd)

validateKeyPackage :: ClientIdentity -> KeyPackage -> Handler r ()
validateKeyPackage identity (kpTBS -> kp) = do
  validateCredential identity (kpCredential kp)
  validateExtensions (kpExtensions kp)

validateCredential :: ClientIdentity -> Credential -> Handler r ()
validateCredential identity cred = do
  -- TODO: validate signature
  identity' <-
    either (throwErrorDescription . mlsProtocolError) pure $
      decodeMLS' (bcIdentity cred)
  when (identity /= identity') $
    throwErrorDescriptionType @MLSIdentityMismatch

data RequiredExtensions f = RequiredExtensions
  { reLifetime :: f Lifetime,
    reCapabilities :: f ()
  }

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

findExtensions :: [Extension] -> Maybe (RequiredExtensions Identity)
findExtensions = checkRequiredExtensions . foldMap findExtension

findExtension :: Extension -> RequiredExtensions Maybe
findExtension e = case decodeExtension e of
  Just (SomeExtension SLifetimeExtensionTag lt) -> RequiredExtensions (pure lt) Nothing
  Just (SomeExtension SCapabilitiesExtensionTag _) -> RequiredExtensions Nothing (pure ())
  _ -> mempty

validateExtensions :: [Extension] -> Handler r ()
validateExtensions exts = do
  _re <-
    maybe (throwErrorDescription (mlsProtocolError "Missing required extensions")) pure $
      findExtensions exts
  -- TODO: validate lifetime
  pure ()
