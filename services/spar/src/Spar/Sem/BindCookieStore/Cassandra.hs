{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.BindCookieStore.Cassandra where

import Cassandra
import Control.Monad.Except (runExceptT)
import Imports hiding (MonadReader (..), Reader)
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import SAML2.WebSSO (fromTime, getNow)
import qualified Spar.Data as Data
import Spar.Sem.AReqIDStore.Cassandra ()
import Spar.Sem.BindCookieStore
import Wire.API.User.Saml (Opts, TTLError)

bindCookieStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Error TTLError, Embed IO, Reader Opts] r) =>
  Sem (BindCookieStore ': r) a ->
  Sem r a
bindCookieStoreToCassandra = interpret $ \case
  Insert sbc uid ndt -> do
    denv <- Data.mkEnv <$> ask <*> (fromTime <$> getNow)
    a <- embed @m $ runExceptT $ runReaderT (Data.insertBindCookie sbc uid ndt) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  Lookup bc -> embed @m $ Data.lookupBindCookie bc
