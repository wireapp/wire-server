{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module System.Metrics.Collectd.IO
    ( Context
    , Collector
    , runCollector
    , create
    , terminate
    , wait
    , collectAll
    , collectOne
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async hiding (wait)
import Control.Exception
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Foldable (for_, mapM_)
import Data.IORef
import Data.List (intersperse)
import Data.Monoid
import Data.Time.Clock.POSIX
import Data.Text (unpack)
import Network.HTTP.Client
import Prelude hiding (mapM_)
import System.IO (hFlush, stdout)
import System.Metrics.Collectd.Collectd hiding (values)
import System.Metrics.Collectd.Config
import System.Metrics.Collectd.Json

data Context = Context
    { pool    :: Manager
    , threads :: IORef [Async ()]
    , putMux  :: MVar ()
    }

newtype Collector a = Collector
    { collector :: ReaderT Context IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

runCollector :: Context -> Collector a -> IO a
runCollector x c = runReaderT (collector c) x

create :: IO Context
create = Context <$> newManager defaultManagerSettings
                 <*> newIORef []
                 <*> newMVar ()

terminate :: Collector ()
terminate = Collector $ do
    r <- asks threads
    liftIO $ atomicModifyIORef' r (\t -> ([], t)) >>= mapM_ cancel

wait :: Collector (Maybe SomeException)
wait = Collector $ do
    a <- liftIO . readIORef =<< asks threads
    (_, x) <- liftIO $ waitAnyCatch a
    case x of
        Left e  -> return $ Just e
        Right _ -> return $ Nothing

collectAll :: Config -> Collector ()
collectAll c = mapM_ (collectOne c) (sections c)

collectOne :: Config -> Section -> Collector ()
collectOne c s = Collector $ do
    r <- asks threads
    m <- asks pool
    u <- asks putMux
    liftIO $ do
        a <- async (start m u)
        atomicModifyIORef' r $ \t -> (a:t, ())
  where
    start mg mux = do
        rq <- parseUrlThrow (unpack $ url s)
        forever $ do
            tm <- getPOSIXTime
            rs <- httpLbs rq mg
            for_ (decode' (responseBody rs)) $ \j ->
                forM_ (paths s) $ \p -> do
                    let sel = select (fst p) j
                    mapM_ (output mux (snd p) tm) (values sel)
                    mapM_ (putErr . mconcat . intersperse ".") (errors sel)
            hFlush stdout
            threadDelay (interval c * 1000)

    output mux t tm (x, v) =
        let typeI = Just . mconcat $ intersperse "." x
            plugI = Just (name s)
            ident = Identifier (hostname c) "metrics" plugI (typename t) typeI
            opts  = [Interval (interval c `div` 1000)]
            vals  = ValueList tm [v]
        in withMVar mux $ const (putVal ident opts vals)
