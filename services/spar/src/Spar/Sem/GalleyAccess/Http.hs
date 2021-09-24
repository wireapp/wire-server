{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spar.Sem.GalleyAccess.Http where

import Bilge
import Imports
import Polysemy
import Spar.Sem.GalleyAccess
import Spar.Intra.Galley (MonadSparToGalley)
import Spar.Intra.Brig (MonadSparToBrig (..))
import qualified Spar.Intra.Galley as Intra
import Spar.Error (SparError)
import Control.Monad.Except
import qualified System.Logger.Class as Log
import Polysemy.Error

data RunHttpEnv = RunHttpEnv
  { rheManager :: Bilge.Manager
  , rheRequest :: Bilge.Request
  }

newtype RunHttp a = RunHttp
  { unRunHttp :: ReaderT RunHttpEnv (ExceptT SparError (HttpT IO)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError SparError, MonadIO, MonadHttp, MonadReader RunHttpEnv)


viaRunHttp
    :: Members '[Error SparError, Embed IO] r
    => RunHttpEnv -> RunHttp a -> Sem r a
viaRunHttp env m = do
  ma <- embed @IO $ runHttpT (rheManager env) $ runExceptT $ flip runReaderT env $ unRunHttp m
  case ma of
    Left err -> throw err
    Right a -> pure a


-- TODO(sandy): Implement me
instance Log.MonadLogger RunHttp where
  log _ _ = pure ()

instance MonadSparToGalley RunHttp where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

instance MonadSparToBrig RunHttp where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

galleyAccessToHttp
    :: Members '[Error SparError, Embed IO] r
    => Bilge.Manager -> Bilge.Request -> Sem (GalleyAccess ': r) a -> Sem r a
galleyAccessToHttp mgr req =
  interpret $ viaRunHttp (RunHttpEnv mgr req) . \case
    GetTeamMembers itlt -> Intra.getTeamMembers itlt
    AssertHasPermission itlt perm itlu -> Intra.assertHasPermission itlt perm itlu
    AssertSSOEnabled itlt -> Intra.assertSSOEnabled itlt
    IsEmailValidationEnabledTeam itlt -> Intra.isEmailValidationEnabledTeam itlt

