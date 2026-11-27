-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Rotation of x509 credentials.
--
-- An MLS client is identified by its 'ClientIdentity', which for x509
-- credentials is carried in the subject alternative name of the certificate.
-- Over its lifetime a client may hold several certificates, and each new
-- certificate must come with a fresh signature key pair: renewing a
-- certificate while keeping the old key pair does not recover from a key
-- compromise.
--
-- The backend pins the signature key of each client, so replacing it requires
-- a proof of possession of both keys (see 'MLS.KeyRotation'). These tests
-- check that a client which rotates its x509 credential this way keeps being
-- treated as the same client, and that the key cannot be replaced without
-- such a proof.
--
-- Basic credentials cannot be rotated: a basic credential has no identity
-- other than its key.
--
-- FUTUREWORK: rotating the credential of a client which is already a member
-- of a group, which mls-test-cli does not support.
module Test.MLS.CredentialRotation where

import API.Brig
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import MLS.KeyRotation
import MLS.Util
import SetupHelpers
import Testlib.Prelude

-- | After rotating its x509 credential, a client can register its new
-- signature key with the backend. Key packages signed with the old key are
-- deleted and rejected, and only key packages signed with the new key are
-- accepted.
testRotateX509CredentialKeyPackages :: (HasCallStack) => Ciphersuite -> App ()
testRotateX509CredentialKeyPackages suite = do
  let scheme = csSignatureScheme suite
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient (x509Client suite) alice

  replicateM_ 2 $ uploadNewKeyPackage suite alice1
  -- a key package signed with the old key, which is never uploaded
  (oldKp, _) <- generateKeyPackage alice1 suite

  rotation <- prepareX509Rotation suite alice1
  assertBool "rotation must generate a new signature key" (rotation.newKey /= rotation.oldKey)

  bindResponse (rotateMLSPublicKey alice1 alice1.client rotation.keyPackage rotation.signature) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (getClient alice alice1.client) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "mls_public_keys" %. scheme `shouldMatch` b64 rotation.newKey

  -- key packages signed with the old key are no longer available
  bindResponse (countKeyPackages suite alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` 0

  -- key packages signed with the old key are rejected
  bindResponse (uploadKeyPackages alice1 [oldKp]) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-protocol-error"

  -- key packages signed with the new key are accepted
  void $ uploadNewKeyPackage suite alice1
  bindResponse (countKeyPackages suite alice1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "count" `shouldMatchInt` 1

-- | After rotating its x509 credential, a client can be added to a
-- conversation with a key package signed by its new key, and can then commit
-- as the same client.
testRotateX509CredentialJoinConversation :: (HasCallStack) => Ciphersuite -> App ()
testRotateX509CredentialJoinConversation suite = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1] <- traverse (createMLSClient (x509Client suite)) [alice, bob]

  -- bob1's original signature key was registered when the client was created
  rotation <- prepareX509Rotation suite bob1
  bindResponse (rotateMLSPublicKey bob1 bob1.client rotation.keyPackage rotation.signature) $ \resp ->
    resp.status `shouldMatchInt` 200
  void $ uploadNewKeyPackage suite bob1

  convId <- createNewGroup suite alice1
  resp <- createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle
  event <- resp %. "events" & asList >>= assertOne
  event %. "type" `shouldMatch` "conversation.member-join"
  event %. "data.users.0.qualified_id" `shouldMatch` (bob %. "qualified_id")

  -- An empty commit carries an update path whose leaf node is signed with the
  -- new key; galley checks that key against the one registered in brig.
  -- Application messages would not do: their signatures are never checked by
  -- the backend.
  void $ createPendingProposalCommit convId bob1 >>= sendAndConsumeCommitBundle

-- | Without a signature by the registered key, the key cannot be replaced,
-- neither by the rotation endpoint nor by updating the client.
testRotateX509CredentialRequiresOldKey :: (HasCallStack) => App ()
testRotateX509CredentialRequiresOldKey = do
  let suite = def
      scheme = csSignatureScheme suite
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient (x509Client def) alice
  rotation <- prepareX509Rotation suite alice1

  -- signed with the new key instead of the registered one
  newPrivateKey <- signaturePrivateKey suite alice1
  forged <- signKeyRotation suite newPrivateKey alice1 rotation.oldKey rotation.newKey
  bindResponse (rotateMLSPublicKey alice1 alice1.client rotation.keyPackage forged) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-invalid-key-rotation"

  bindResponse
    (updateClient alice1 def {mlsPublicKeys = Just (object [scheme .= b64 rotation.newKey])})
    $ \resp -> do
      resp.status `shouldMatchInt` 400
      resp.json %. "label" `shouldMatch` "mls-duplicate-public-key"

  bindResponse (getClient alice alice1.client) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "mls_public_keys" %. scheme `shouldMatch` b64 rotation.oldKey

-- | A rotation request cannot be replayed: once the key is replaced, the
-- signature is no longer by the registered key.
testRotateX509CredentialReplay :: (HasCallStack) => App ()
testRotateX509CredentialReplay = do
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient (x509Client def) alice
  rotation <- prepareX509Rotation def alice1
  let rotate = rotateMLSPublicKey alice1 alice1.client rotation.keyPackage rotation.signature
  bindResponse rotate $ \resp -> resp.status `shouldMatchInt` 200
  bindResponse rotate $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-invalid-key-rotation"

-- | The key package must carry a credential for the rotated client, even when
-- the signature by the registered key is valid.
testRotateX509CredentialOtherClientKeyPackage :: (HasCallStack) => App ()
testRotateX509CredentialOtherClientKeyPackage = do
  let suite = def
  alice <- randomUser OwnDomain def
  [alice1, alice2] <- replicateM 2 $ createMLSClient (x509Client def) alice
  oldKey <- mlscli Nothing suite alice1 ["public-key"] Nothing
  oldPrivateKey <- signaturePrivateKey suite alice1
  otherKey <- mlscli Nothing suite alice2 ["public-key"] Nothing
  (otherKp, _) <- generateKeyPackage alice2 suite
  sig <- signKeyRotation suite oldPrivateKey alice1 oldKey otherKey
  bindResponse (rotateMLSPublicKey alice1 alice1.client otherKp sig) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-invalid-key-rotation"

-- | Basic credentials cannot be rotated.
testRotateBasicCredential :: (HasCallStack) => App ()
testRotateBasicCredential = do
  let suite = def
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def alice
  key <- mlscli Nothing suite alice1 ["public-key"] Nothing
  privateKey <- signaturePrivateKey suite alice1
  (kp, _) <- generateKeyPackage alice1 suite
  sig <- signKeyRotation suite privateKey alice1 key key
  bindResponse (rotateMLSPublicKey alice1 alice1.client kp sig) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-invalid-key-rotation"

-- | A user cannot rotate the key of another user's client.
testRotateX509CredentialOtherUser :: (HasCallStack) => App ()
testRotateX509CredentialOtherUser = do
  [alice, bob] <- replicateM 2 $ randomUser OwnDomain def
  alice1 <- createMLSClient (x509Client def) alice
  rotation <- prepareX509Rotation def alice1
  bindResponse (rotateMLSPublicKey bob alice1.client rotation.keyPackage rotation.signature) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "client-not-found"

-- | Only a registered key can be rotated; the first key is registered by
-- updating the client.
testRotateUnregisteredKey :: (HasCallStack) => App ()
testRotateUnregisteredKey = do
  alice <- randomUser OwnDomain def
  alice1 <- createWireClient alice def
  setClientGroupState alice1 ClientGroupState {groups = mempty, keystore = mempty, credType = X509CredentialType}
  rotation <- prepareX509Rotation def alice1
  bindResponse (rotateMLSPublicKey alice1 alice1.client rotation.keyPackage rotation.signature) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "mls-public-key-not-found"

x509Client :: Ciphersuite -> InitMLSClient
x509Client suite = def {ciphersuites = [suite], credType = X509CredentialType}

b64 :: ByteString -> Text
b64 = T.decodeUtf8 . Base64.encode
