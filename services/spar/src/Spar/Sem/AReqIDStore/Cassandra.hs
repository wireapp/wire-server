module Spar.Sem.AReqIDStore.Cassandra where

import Cassandra
import Control.Monad.Except (runExceptT)
import Imports hiding (MonadReader (..), Reader)
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import SAML2.WebSSO (fromTime)
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import Spar.Error
import Spar.Sem.AReqIDStore
import Spar.Sem.Now (Now)
import qualified Spar.Sem.Now as Now
import Wire.API.User.Saml (Opts, TTLError)

aReqIDStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Now, Error TTLError, Embed IO, Input Opts] r) =>
  Sem (AReqIDStore ': r) a ->
  Sem r a
aReqIDStoreToCassandra = interpret $ \case
  Store itla t -> do
    denv <- Data.mkEnv <$> input <*> (fromTime <$> Now.get)
    a <- embed @m $ runExceptT $ runReaderT (Data.storeAReqID itla t) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  UnStore itla -> embed @m $ Data.unStoreAReqID itla
  IsAlive itla -> embed @m $ Data.isAliveAReqID itla

ttlErrorToSparError :: Member (Error SparError) r => Sem (Error TTLError ': r) a -> Sem r a
ttlErrorToSparError = mapError (SAML.CustomError . SparCassandraTTLError)
