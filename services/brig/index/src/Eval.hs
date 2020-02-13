{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
  ( runCommand,
  )
where

import Brig.User.Search.Index
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens
import Control.Monad.Catch
import qualified Data.Metrics as Metrics
import Data.Text.Strict.Lens
import qualified Database.V5.Bloodhound as ES
import Imports
import Network.HTTP.Client
import Options
import qualified System.Logger as Log
import System.Logger.Class (Logger, MonadLogger (..))
import URI.ByteString

runCommand :: Logger -> Command -> IO ()
runCommand l = \case
  Create es -> do
    e <- initIndex es
    runIndexIO e (createIndex (es ^. esIndexSettings))
  Reset es -> do
    e <- initIndex es
    runIndexIO e (resetIndex (es ^. esIndexSettings))
  Reindex es cas -> do
    e <- initIndex es
    c <- initDb cas
    runReindexIO e c reindexAll
  where
    initIndex es =
      IndexEnv
        <$> Metrics.metrics
        <*> pure l
        <*> initES es
        <*> pure Nothing
        <*> pure (view esIndex es)
    initES es =
      ES.mkBHEnv (view (esServer . re _ESServer) es)
        <$> newManager defaultManagerSettings
    initDb cas =
      C.init
        $ C.setLogger (C.mkLogger l)
          . C.setContacts (view cHost cas) []
          . C.setPortNumber (fromIntegral (view cPort cas))
          . C.setKeyspace (view cKeyspace cas)
          . C.setProtocolVersion C.V4
        $ C.defSettings

_ESServer :: Prism' ES.Server (URIRef Absolute)
_ESServer = prism toS fromS
  where
    toS =
      ES.Server
        . view utf8
        . serializeURIRef'
        . set pathL mempty
        . set queryL mempty
        . set fragmentL mempty
    fromS x@(ES.Server s) =
      set _Left x
        . parseURI strictURIParserOptions
        $ review utf8 s

--------------------------------------------------------------------------------
-- ReindexIO command monad

newtype ReindexIO a = ReindexIO (ReaderT C.ClientState IndexIO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader C.ClientState,
      MonadThrow,
      MonadCatch
    )

runReindexIO :: IndexEnv -> C.ClientState -> ReindexIO a -> IO a
runReindexIO ixe cas (ReindexIO ma) = runIndexIO ixe (runReaderT ma cas)

instance MonadIndexIO ReindexIO where
  liftIndexIO = ReindexIO . ReaderT . const

instance C.MonadClient ReindexIO where
  liftClient ma = ask >>= \e -> C.runClient e ma
  localState = local

instance MonadLogger ReindexIO where
  log lvl msg = do
    l <- ReindexIO . lift $ asks idxLogger
    Log.log l lvl msg
