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
import Data.Text (pack)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as T
import Imports
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.EVP.Digest
import Polysemy
import Polysemy.Embed
import Text.Printf (printf)
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
          >>= retry x1 . query1 cql . params LocalQuorum . Identity
      NewActivationCode ek timeout uid -> newActivationCodeImpl ek timeout uid
  where
    cql :: PrepQuery R (Identity ActivationKey) (Maybe UserId, ActivationCode)
    cql =
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
  code <- liftIO $ genCode
  insert typ key code
  where
    insert t k c = do
      key <- liftIO $ mkActivationKey uk
      retry x5 . write keyInsert $ params LocalQuorum (key, t, k, c, u, maxAttempts, round timeout)
      pure $ Activation key c
    genCode =
      ActivationCode . Ascii.unsafeFromText . pack . printf "%06d"
        <$> randIntegerZeroToNMinusOne 1000000

--------------------------------------------------------------------------------
-- Utilities

mkActivationKey :: EmailKey -> IO ActivationKey
mkActivationKey k = do
  Just d <- getDigestByName "SHA256"
  pure do
    ActivationKey
      . Ascii.encodeBase64Url
      . digestBS d
      . T.encodeUtf8
      $ emailKeyUniq k

keyInsert :: PrepQuery W (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
keyInsert =
  "INSERT INTO activation_keys \
  \(key, key_type, key_text, code, user, retries) VALUES \
  \(?  , ?       , ?       , ?   , ?   , ?      ) USING TTL ?"

-- | Max. number of activation attempts per 'ActivationKey'.
maxAttempts :: Int32
maxAttempts = 3
