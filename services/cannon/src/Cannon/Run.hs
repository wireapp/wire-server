module Cannon.Run (run) where

import Imports hiding (head)
import Bilge (newManager, defaultManagerSettings, ManagerSettings (..))
import Cannon.App (maxPingInterval)
import Cannon.API (sitemap)
import Cannon.Types (mkEnv, applog, runCannon)
import Cannon.Options
import Cannon.WS hiding (env)
import Control.Lens ((^.))
import Control.Monad.Catch (finally)
import Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Text (strip, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Utilities.Server
import Network.Wai.Handler.Warp hiding (run)
import System.Random.MWC (createSystemRandom)

import qualified Cannon.Dict                 as D
import qualified Data.Metrics.Middleware as Middleware
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified System.IO.Strict            as Strict
import qualified System.Logger               as L
import qualified System.Logger.Extended      as L

run :: Opts -> IO ()
run o = do
    ext <- loadExternal
    m <- Middleware.metrics
    g <- L.mkLogger (o ^. logLevel) (o ^. logNetStrings)
    e <- mkEnv <$> pure m
               <*> pure ext
               <*> pure o
               <*> pure g
               <*> D.empty 128
               <*> newManager defaultManagerSettings { managerConnCount = 128 }
               <*> createSystemRandom
               <*> mkClock
    s <- newSettings $ Server (o^.cannon.host) (o^.cannon.port) (applog e) m (Just idleTimeout) [] []
    let rtree    = compile sitemap
        measured = measureRequests m (treeToPaths rtree)
        app  r k = runCannon e (route rtree r k) r
        middleware :: Wai.Middleware
        middleware = waiPrometheusMiddleware sitemap
                   . measured
                   . catchErrors g m
                   . Gzip.gzip Gzip.def
        start    =  middleware app
    runSettings s start `finally` L.close (applog e)
  where
    idleTimeout = fromIntegral $ maxPingInterval + 3

    -- Each cannon instance advertises its own location (ip or dns name) to gundeck.
    -- Either externalHost or externalHostFile must be set (externalHost takes precedence if both are defined)
    loadExternal :: IO ByteString
    loadExternal = do
      let extFile = fromMaybe (error "One of externalHost or externalHostFile must be defined") (o^.cannon.externalHostFile)
      fromMaybe (readExternal extFile) (return . encodeUtf8 <$> o^.cannon.externalHost)

    readExternal :: FilePath -> IO ByteString
    readExternal f = encodeUtf8 . strip . pack <$> Strict.readFile f
