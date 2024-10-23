module Wire.ActivationCodeStore.Cassandra where

import Cassandra
import Data.Id
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as T
import Imports
import OpenSSL.EVP.Digest
import Polysemy
import Polysemy.Embed
import Wire.API.User.Activation
import Wire.ActivationCodeStore
import Wire.UserKeyStore (EmailKey, emailKeyUniq)

interpretActivationCodeStoreToCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor ActivationCodeStore r
interpretActivationCodeStoreToCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      LookupActivationCode ek -> embed do
        liftIO (mkActivationKey ek)
          >>= retry x1 . query1 cql . params LocalQuorum . Identity
  where
    cql :: PrepQuery R (Identity ActivationKey) (Maybe UserId, ActivationCode)
    cql =
      [sql| 
      SELECT user, code FROM activation_keys WHERE key = ?
      |]

mkActivationKey :: EmailKey -> IO ActivationKey
mkActivationKey k = do
  Just d <- getDigestByName "SHA256"
  pure do
    ActivationKey
      . Ascii.encodeBase64Url
      . digestBS d
      . T.encodeUtf8
      $ emailKeyUniq k
