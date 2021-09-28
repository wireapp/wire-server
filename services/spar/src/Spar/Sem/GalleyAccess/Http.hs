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
import qualified System.Logger as Log
import qualified System.Logger.Class as LogClass

data RunHttpEnv = RunHttpEnv
  { rheLogger :: Log.Logger,
    rheManager :: Bilge.Manager,
    rheRequest :: Bilge.Request
  }

newtype RunHttp a = RunHttp
  { unRunHttp :: ReaderT RunHttpEnv (ExceptT SparError (HttpT IO)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError SparError, MonadIO, MonadHttp, MonadReader RunHttpEnv)

viaRunHttp ::
  Members '[Error SparError, Embed IO] r =>
  RunHttpEnv ->
  RunHttp a ->
  Sem r a
viaRunHttp env m = do
  ma <- embed @IO $ runHttpT (rheManager env) $ runExceptT $ flip runReaderT env $ unRunHttp m
  case ma of
    Left err -> throw err
    Right a -> pure a

instance LogClass.MonadLogger RunHttp where
  log lvl msg = do
    logger <- asks rheLogger
    Log.log logger lvl msg

instance MonadSparToGalley RunHttp where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

instance MonadSparToBrig RunHttp where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

galleyAccessToHttp ::
  Members '[Error SparError, Embed IO] r =>
  Log.Logger ->
  Bilge.Manager ->
  Bilge.Request ->
  Sem (GalleyAccess ': r) a ->
  Sem r a
galleyAccessToHttp logger mgr req =
  interpret $
    viaRunHttp (RunHttpEnv logger mgr req) . \case
      GetTeamMembers itlt -> Intra.getTeamMembers itlt
      AssertHasPermission itlt perm itlu -> Intra.assertHasPermission itlt perm itlu
      AssertSSOEnabled itlt -> Intra.assertSSOEnabled itlt
      IsEmailValidationEnabledTeam itlt -> Intra.isEmailValidationEnabledTeam itlt
