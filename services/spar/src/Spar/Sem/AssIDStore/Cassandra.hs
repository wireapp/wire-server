module Spar.Sem.AssIDStore.Cassandra where

import Cassandra
import Control.Monad.Except (runExceptT)
import Imports hiding (MonadReader (..), Reader)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import SAML2.WebSSO (fromTime)
import qualified Spar.Data as Data
import Spar.Sem.AssIDStore
import Spar.Sem.Now (Now)
import qualified Spar.Sem.Now as Now
import Wire.API.User.Saml (Opts, TTLError)

assIDStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Now, Error TTLError, Embed IO, Input Opts] r) =>
  Sem (AssIDStore ': r) a ->
  Sem r a
assIDStoreToCassandra =
  interpret $ \case
    Store itla t -> do
      denv <- Data.mkEnv <$> input <*> (fromTime <$> Now.get)
      a <- embed @m $ runExceptT $ runReaderT (Data.storeAssID itla t) denv
      case a of
        Left err -> throw err
        Right () -> pure ()
    UnStore itla -> embed @m $ Data.unStoreAssID itla
    IsAlive itla -> embed @m $ Data.isAliveAssID itla
