{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.Sem.AReqIDStore.Cassandra where

import Cassandra
import Imports hiding (MonadReader(..), Reader)
import Polysemy
import Polysemy.Error
import qualified Spar.Data as Data
import Spar.Sem.AReqIDStore
import Control.Monad.Except (runExceptT)
import Wire.API.User.Saml (TTLError, Opts)
import SAML2.WebSSO (getNow, fromTime, HasNow)
import Polysemy.Reader
import Spar.Error
import qualified SAML2.WebSSO as SAML


instance Member (Embed IO) r => HasNow (Sem r) where

aReqIDStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Error TTLError, Embed IO, Reader Opts] r) =>
  Sem (AReqIDStore ': r) a ->
  Sem r a
aReqIDStoreToCassandra = interpret $ \case
  Store itla t -> do
    denv <- Data.mkEnv <$> ask <*> (fromTime <$> getNow)
    a <- embed @m $ runExceptT $ runReaderT (Data.storeAReqID itla t) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  UnStore itla -> embed @m $ Data.unStoreAReqID itla
  IsAlive itla -> embed @m $ Data.isAliveAReqID itla


ttlErrorToSparError :: Member (Error SparError) r => Sem (Error TTLError ': r) a -> Sem r a
ttlErrorToSparError = mapError (SAML.CustomError . SparCassandraTTLError)

