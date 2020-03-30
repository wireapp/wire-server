{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brig.Index.Eval
  ( runCommand,
  )
where

import Brig.Index.Migrations
import Brig.Index.Options
import Brig.User.Search.Index
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens
import Control.Monad.Catch
import qualified Data.Metrics as Metrics
import qualified Database.V5.Bloodhound as ES
import Imports
import Network.HTTP.Client
import qualified System.Logger as Log
import System.Logger.Class (Logger, MonadLogger (..))

runCommand :: Logger -> Command -> IO ()
runCommand l = \case
  Create es -> do
    e <- initIndex es
    runIndexIO e $ uncurry createIndexIfNotPresent $ mkCreateIndexSettings es
  Reset es -> do
    e <- initIndex es
    runIndexIO e $ uncurry resetIndex $ mkCreateIndexSettings es
  Reindex es cas -> do
    e <- initIndex es
    c <- initDb cas
    runReindexIO e c reindexAll
  ReindexSameOrNewer es cas -> do
    e <- initIndex es
    c <- initDb cas
    runReindexIO e c reindexAllIfSameOrNewer
  UpdateMapping esURI indexName -> do
    e <- initIndex' esURI indexName
    runIndexIO e updateMapping
  Migrate es cas -> do
    migrate l es cas
  where
    initIndex es =
      initIndex' (es ^. esServer) (es ^. esIndex)
    initIndex' esURI indexName =
      IndexEnv
        <$> Metrics.metrics
        <*> pure l
        <*> initES esURI
        <*> pure Nothing
        <*> pure indexName
    initES esURI =
      ES.mkBHEnv (toESServer esURI)
        <$> newManager defaultManagerSettings
    initDb cas =
      C.init
        $ C.setLogger (C.mkLogger l)
          . C.setContacts (view cHost cas) []
          . C.setPortNumber (fromIntegral (view cPort cas))
          . C.setKeyspace (view cKeyspace cas)
          . C.setProtocolVersion C.V4
        $ C.defSettings

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
