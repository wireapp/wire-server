-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ActivationCodeStore.Cassandra (interpretActivationCodeStoreToCassandra) where

import Cassandra
import Data.Id
import Imports
import Polysemy
import Polysemy.Embed
import Util.Timeout
import Wire.API.User.Activation
import Wire.API.User.EmailAddress
import Wire.ActivationCodeStore
import Wire.UserKeyStore

interpretActivationCodeStoreToCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor ActivationCodeStore r
interpretActivationCodeStoreToCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . embed . \case
      LookupActivationCode ek -> do
        liftIO (mkActivationKey ek)
          >>= retry x1 . query1 lookupCode . params LocalQuorum . Identity
      NewActivationCode ek timeout uid -> newActivationCodeImpl ek timeout uid
      DeleteActivationCode ek -> deleteActivationCodeImpl ek
      LookupActivationKey key -> lookupActivationKeyImpl key
      DecrementActivationRetries key -> decrementActivationRetriesImpl key
      DeleteActivationKey key -> deleteActivationKeyImpl key
  where
    lookupCode :: PrepQuery R (Identity ActivationKey) (Maybe UserId, ActivationCode)
    lookupCode =
      [sql|
      SELECT user, code FROM activation_keys WHERE key = ?
      |]

-- | Create a new pending activation for a given 'EmailKey'.
newActivationCodeImpl ::
  (MonadClient m) =>
  EmailKey ->
  -- | The timeout for the activation code.
  Timeout ->
  -- | The user with whom to associate the activation code.
  Maybe UserId ->
  m Activation
newActivationCodeImpl uk timeout u = do
  let typ = "email"
      key = fromEmail (emailKeyOrig uk)
  code <- liftIO genActivationCode
  key' <- liftIO $ mkActivationKey uk
  retry x5 . write keyInsert $ params LocalQuorum (key', typ, key, code, u, maxAttempts, round timeout)
  pure $ Activation key' code

-- | Delete a pending activation code for a given 'EmailKey', if any.
deleteActivationCodeImpl ::
  (MonadClient m) =>
  EmailKey ->
  m ()
deleteActivationCodeImpl uk = do
  key <- liftIO $ mkActivationKey uk
  retry x5 . write keyDelete $ params LocalQuorum (Identity key)

-- | Read the full row for an opaque 'ActivationKey' (unexpired rows only:
-- Cassandra drops expired rows via the TTL, so no expiry filter is needed).
lookupActivationKeyImpl ::
  (MonadClient m) =>
  ActivationKey ->
  m (Maybe ActivationKeyRow)
lookupActivationKeyImpl key = do
  s <- retry x1 . query1 keySelect $ params LocalQuorum (Identity key)
  pure $ case s of
    Just (_, Ascii t, k, c, u, r) -> Just (ActivationKeyRow t k c u r)
    Nothing -> Nothing

-- | Decrement the retry counter by one, preserving the remaining TTL.
-- (TTL-preserving decrement is a Cassandra persistence detail, which is why
-- it lives in the store.)  No-op when the row is absent or already at 0.
decrementActivationRetriesImpl ::
  (MonadClient m) =>
  ActivationKey ->
  m ()
decrementActivationRetriesImpl key = do
  s <- retry x1 . query1 keySelect $ params LocalQuorum (Identity key)
  case s of
    Just (ttl, Ascii t, k, c, u, r)
      | r >= 1 ->
          retry x5 . write keyInsert $ params LocalQuorum (key, t, k, c, u, r - 1, ttl)
    _ -> pure ()

-- | Delete the row for an opaque 'ActivationKey' (brute-force exhaustion).
deleteActivationKeyImpl ::
  (MonadClient m) =>
  ActivationKey ->
  m ()
deleteActivationKeyImpl key =
  retry x5 . write keyDelete $ params LocalQuorum (Identity key)

--------------------------------------------------------------------------------
-- CQL queries

keySelect :: PrepQuery R (Identity ActivationKey) (Int32, Ascii, Text, ActivationCode, Maybe UserId, Int32)
keySelect = "SELECT ttl(code) as ttl, key_type, key_text, code, user, retries FROM activation_keys WHERE key = ?"

keyInsert :: PrepQuery W (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
keyInsert =
  "INSERT INTO activation_keys \
  \(key, key_type, key_text, code, user, retries) VALUES \
  \(?  , ?       , ?       , ?   , ?   , ?      ) USING TTL ?"

keyDelete :: PrepQuery W (Identity ActivationKey) ()
keyDelete = "DELETE FROM activation_keys WHERE key = ?"
