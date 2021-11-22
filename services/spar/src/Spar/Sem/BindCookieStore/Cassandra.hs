{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.BindCookieStore.Cassandra where

import Cassandra
import Control.Monad.Except (runExceptT)
import Imports hiding (MonadReader (..), Reader)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import SAML2.WebSSO (fromTime)
import qualified Spar.Data as Data
import Spar.Sem.BindCookieStore
import Spar.Sem.Now (Now)
import qualified Spar.Sem.Now as Now
import Wire.API.User.Saml (Opts, TTLError)

bindCookieStoreToCassandra ::
  forall m r a.
  (MonadClient m, Members '[Embed m, Now, Error TTLError, Embed IO, Input Opts] r) =>
  Sem (BindCookieStore ': r) a ->
  Sem r a
bindCookieStoreToCassandra = interpret $ \case
  Insert sbc uid ndt -> do
    denv <- Data.mkEnv <$> input <*> (fromTime <$> Now.get)
    a <- embed @m $ runExceptT $ runReaderT (Data.insertBindCookie sbc uid ndt) denv
    case a of
      Left err -> throw err
      Right () -> pure ()
  Lookup bc -> embed @m $ Data.lookupBindCookie bc
