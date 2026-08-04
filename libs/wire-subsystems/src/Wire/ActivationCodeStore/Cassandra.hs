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
      VerifyActivationCode key code -> verifyActivationCodeImpl key code
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

-- | Verify an activation code, decrementing retries or revoking on mismatch.
-- Returns 'Just' the 'EmailKey' and 'UserId' on a match, 'Nothing' otherwise.
verifyActivationCodeImpl ::
  (MonadClient m) =>
  ActivationKey ->
  ActivationCode ->
  m (Maybe (EmailKey, Maybe UserId))
verifyActivationCodeImpl key code = do
  s <- retry x1 . query1 keySelect $ params LocalQuorum (Identity key)
  case s of
    Just (ttl, Ascii t, k, c, u, r) ->
      if
        | c == code -> pure (mkActivationScope t k u)
        | r >= 1 -> do
            retry x5 . write keyInsert $ params LocalQuorum (key, t, k, c, u, r - 1, ttl)
            pure Nothing
        | otherwise -> do
            write keyDelete $ params LocalQuorum (Identity key)
            pure Nothing
    Nothing -> pure Nothing
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
