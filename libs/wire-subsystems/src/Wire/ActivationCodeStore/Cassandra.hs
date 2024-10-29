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
      NewActivation ek timeout uid -> newActivationImpl ek timeout uid
  where
    cql :: PrepQuery R (Identity ActivationKey) (Maybe UserId, ActivationCode)
    cql =
      [sql|
      SELECT user, code FROM activation_keys WHERE key = ?
      |]

-- | Create a new pending activation for a given 'EmailKey'.
newActivationImpl ::
  (MonadClient m) =>
  EmailKey ->
  -- | The timeout for the activation code.
  Timeout ->
  -- | The user with whom to associate the activation code.
  Maybe UserId ->
  m Activation
newActivationImpl uk timeout u = do
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
