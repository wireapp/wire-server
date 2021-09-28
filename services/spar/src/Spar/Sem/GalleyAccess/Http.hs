{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spar.Sem.GalleyAccess.Http where

import Bilge
import Control.Monad.Except
import Imports hiding (log)
import Polysemy
import Polysemy.Error
import Spar.Error (SparError)
import Spar.Intra.Brig (MonadSparToBrig (..))
import Spar.Intra.Galley (MonadSparToGalley)
import qualified Spar.Intra.Galley as Intra
import Spar.Sem.GalleyAccess
import qualified System.Logger as TinyLog
import qualified System.Logger.Class as TinyLog
import Spar.Sem.Logger (Logger)
import qualified Spar.Sem.Logger as Logger
import Spar.Sem.Logger.TinyLog (fromLevel)

data RunHttpEnv r = RunHttpEnv
  { rheLogger :: Logger.Level -> (TinyLog.Msg -> TinyLog.Msg) -> Sem r (),
    rheManager :: Bilge.Manager,
    rheRequest :: Bilge.Request
  }

newtype RunHttp r a = RunHttp
  { unRunHttp :: ReaderT (RunHttpEnv r) (ExceptT SparError (HttpT (Sem r))) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError SparError, MonadReader (RunHttpEnv r))

instance Member (Embed IO) r => MonadIO (RunHttp r) where
  liftIO = semToRunHttp . embed

instance Member (Embed IO) r => MonadHttp (RunHttp r) where
  handleRequestWithCont r fribia = RunHttp $ lift $ lift $
    handleRequestWithCont r fribia

semToRunHttp :: Sem r a -> RunHttp r a
semToRunHttp = RunHttp . lift . lift . lift

viaRunHttp ::
  Members '[Error SparError, Embed IO] r =>
  RunHttpEnv r ->
  RunHttp r a ->
  Sem r a
viaRunHttp env m = do
  ma <- runHttpT (rheManager env) $ runExceptT $ flip runReaderT env $ unRunHttp m
  case ma of
    Left err -> throw err
    Right a -> pure a

instance TinyLog.MonadLogger (RunHttp r) where
  log lvl msg = do
    logger <- asks rheLogger
    semToRunHttp $ logger (fromLevel lvl) msg

instance Member (Embed IO) r => MonadSparToGalley (RunHttp r) where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

instance Member (Embed IO) r => MonadSparToBrig (RunHttp r) where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

galleyAccessToHttp ::
  Members '[Logger (TinyLog.Msg -> TinyLog.Msg), Error SparError, Embed IO] r =>
  Bilge.Manager ->
  Bilge.Request ->
  Sem (GalleyAccess ': r) a ->
  Sem r a
galleyAccessToHttp mgr req =
  interpret $
    viaRunHttp (RunHttpEnv (\lvl msg -> Logger.log lvl msg) mgr req) . \case
      GetTeamMembers itlt -> Intra.getTeamMembers itlt
      AssertHasPermission itlt perm itlu -> Intra.assertHasPermission itlt perm itlu
      AssertSSOEnabled itlt -> Intra.assertSSOEnabled itlt
      IsEmailValidationEnabledTeam itlt -> Intra.isEmailValidationEnabledTeam itlt
