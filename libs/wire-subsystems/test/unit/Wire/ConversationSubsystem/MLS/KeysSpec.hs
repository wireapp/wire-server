-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Affero General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.ConversationSubsystem.MLS.KeysSpec (spec) where

import Crypto.ECC (Curve_P256R1, Curve_P384R1, Curve_P521R1, curveGenerateScalar)
import Crypto.PubKey.ECDSA qualified as ECDSA
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Default (def)
import Data.Proxy
import Imports
import Polysemy
import Polysemy.Input
import Test.Hspec
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Keys
import Wire.API.MLS.Serialisation
import Wire.ConversationSubsystem.MLS.Keys

spec :: Spec
spec = describe "Wire.ConversationSubsystem.MLS.Keys.getMLSRemovalKey" $ do
  -- Missing configuration takes precedence over an unsupported scheme.
  it "reports missing configuration for every cipher suite" $
    for_ allSuites $ \cs ->
      case getRemovalKey Nothing cs of
        Left RemovalKeyNotConfigured -> pure ()
        other -> expectationFailure (show cs <> ": " <> describeResult other)

  it "reports the signature schemes without a removal key as unsupported" $ do
    keys <- generateKeys
    let suites = [cs | cs <- allSuites, isNothing (publicKeyFor (csSignatureScheme cs) (mlsKeysToPublic keys))]
    suites `shouldNotBe` []
    for_ suites $ \cs ->
      case getRemovalKey (Just keys) cs of
        Left (RemovalKeyUnsupported scheme) -> scheme `shouldBe` csSignatureScheme cs
        other -> expectationFailure (show cs <> ": " <> describeResult other)

  it "returns the configured key pair for every other cipher suite" $ do
    keys <- generateKeys
    let label = "KeysSpec"
        message = RawMLS "removal key test message" ()
        otherMessage = RawMLS "another message" ()
        suites = [(cs, pub) | cs <- allSuites, Just pub <- [publicKeyFor (csSignatureScheme cs) (mlsKeysToPublic keys)]]
    suites `shouldNotBe` []
    for_ suites $ \(cs, pub) ->
      case getRemovalKey (Just keys) cs of
        Right (SomeKeyPair (_ :: Proxy ss) kp) -> do
          sig <- signWithLabel @ss label kp message
          unless (csVerifySignatureWithLabel cs pub label message sig) $
            expectationFailure (show cs <> ": signature does not verify with the configured public key")
          when (csVerifySignatureWithLabel cs pub label otherMessage sig) $
            expectationFailure (show cs <> ": signature verifies for another message")
        other -> expectationFailure (show cs <> ": " <> describeResult other)

allSuites :: [CipherSuiteTag]
allSuites = [minBound .. maxBound]

getRemovalKey :: Maybe MLSPrivateKeys -> CipherSuiteTag -> Either RemovalKeyError SomeKeyPair
getRemovalKey keys cs =
  run . runInputConst (config keys) $ getMLSRemovalKey (csSignatureScheme cs)

config :: Maybe MLSPrivateKeys -> ConversationSubsystemConfig
config keys =
  ConversationSubsystemConfig
    { mlsKeys = MLSKeysByPurpose <$> keys,
      federationProtocols = Nothing,
      legalholdDefaults = def,
      maxConvSize = 10,
      listClientsUsingBrig = False
    }

describeResult :: Either RemovalKeyError SomeKeyPair -> String
describeResult = \case
  Left err -> "unexpected " <> show err
  Right _ -> "unexpected key pair"

-- | The configured public key for a signature scheme, if the backend has a
-- removal key for it.
publicKeyFor :: SignatureSchemeTag -> MLSPublicKeys -> Maybe ByteString
publicKeyFor scheme public = unwrapMLSPublicKey <$> key
  where
    key = case scheme of
      Ed25519 -> Just public.ed25519
      Ecdsa_secp256r1_sha256 -> Just public.ecdsa_secp256r1_sha256
      Ecdsa_secp384r1_sha384 -> Just public.ecdsa_secp384r1_sha384
      Ecdsa_secp521r1_sha512 -> Just public.ecdsa_secp521r1_sha512
      Mldsa44 -> Nothing
      Mldsa65 -> Nothing
      Mldsa87 -> Nothing

generateKeys :: IO MLSPrivateKeys
generateKeys = do
  ed <- Ed25519.generateSecretKey
  p256 <- ecdsaKeyPair (Proxy @Curve_P256R1)
  p384 <- ecdsaKeyPair (Proxy @Curve_P384R1)
  p521 <- ecdsaKeyPair (Proxy @Curve_P521R1)
  pure
    MLSPrivateKeys
      { mlsKeyPair_ed25519 = (ed, Ed25519.toPublic ed),
        mlsKeyPair_ecdsa_secp256r1_sha256 = p256,
        mlsKeyPair_ecdsa_secp384r1_sha384 = p384,
        mlsKeyPair_ecdsa_secp521r1_sha512 = p521
      }
  where
    ecdsaKeyPair ::
      (ECDSA.EllipticCurveECDSA curve) =>
      Proxy curve ->
      IO (ECDSA.PrivateKey curve, ECDSA.PublicKey curve)
    ecdsaKeyPair p = do
      priv <- curveGenerateScalar p
      pure (priv, ECDSA.toPublic p priv)
