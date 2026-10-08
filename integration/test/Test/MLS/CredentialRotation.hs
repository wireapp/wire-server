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
-- These tests check that a client which rotates its x509 credential, including
-- its signature key, keeps being treated as the same client by the backend.
-- Basic credentials are deliberately not covered: a basic credential has no
-- identity other than its key, so it cannot meaningfully be rotated.
module Test.MLS.CredentialRotation where

import API.Brig
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import MLS.Util
import SetupHelpers
import Testlib.Prelude

-- | After rotating its x509 credential, a client can register its new
-- signature key with the backend. Key packages signed with the old key are
-- invalidated, and only key packages signed with the new key are accepted.
testRotateX509CredentialKeyPackages :: (HasCallStack) => Ciphersuite -> App ()
testRotateX509CredentialKeyPackages suite = do
  let scheme = csSignatureScheme suite
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def {ciphersuites = [suite], credType = X509CredentialType} alice
  oldKey <- mlscli Nothing suite alice1 ["public-key"] Nothing

  replicateM_ 2 $ uploadNewKeyPackage suite alice1
  -- a key package signed with the old key, which is never uploaded
  (oldKp, _) <- generateKeyPackage alice1 suite

  newKey <- rotateX509Credential suite alice1
  assertBool "rotation must generate a new signature key" (newKey /= oldKey)

  -- register the new signature key
  bindResponse
    (updateClient alice1 def {mlsPublicKeys = Just (object [scheme .= b64 newKey])})
    $ \resp -> resp.status `shouldMatchInt` 200

  bindResponse (getClient alice alice1.client) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "mls_public_keys" %. scheme `shouldMatch` b64 newKey

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
-- conversation with a key package signed by its new key, and can then send
-- messages as the same client.
testRotateX509CredentialJoinConversation :: (HasCallStack) => Ciphersuite -> App ()
testRotateX509CredentialJoinConversation suite = do
  let scheme = csSignatureScheme suite
      x509Client = def {ciphersuites = [suite], credType = X509CredentialType}
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1] <- traverse (createMLSClient x509Client) [alice, bob]

  -- bob1's original signature key was registered when the client was created
  newKey <- rotateX509Credential suite bob1
  bindResponse
    (updateClient bob1 def {mlsPublicKeys = Just (object [scheme .= b64 newKey])})
    $ \resp -> resp.status `shouldMatchInt` 200
  void $ uploadNewKeyPackage suite bob1

  convId <- createNewGroup suite alice1
  resp <- createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle
  event <- resp %. "events" & asList >>= assertOne
  event %. "type" `shouldMatch` "conversation.member-join"
  event %. "data.users.0.qualified_id" `shouldMatch` (bob %. "qualified_id")

  -- the backend attributes messages signed with the new key to the same client
  void $ createApplicationMessage convId bob1 "hello" >>= sendAndConsumeMessage

b64 :: ByteString -> String
b64 = T.unpack . T.decodeUtf8 . Base64.encode
