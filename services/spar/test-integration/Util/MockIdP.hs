{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Util.MockIdP where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.String
import Data.String.Conversions
import Lens.Micro
import Network.HTTP.Types
import Network.Wai
import Text.XML.Util (parseURI')
import URI.ByteString
import Util.Options
import Util.Types

import qualified Control.Concurrent.Async          as Async
import qualified Data.ByteString.Lazy              as LBS
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp


-- serving an application

withMockIdP
    :: forall a m. (MonadIO m, MonadMask m, MonadReader TestEnv m)
    => Application -> m a -> m a
withMockIdP app go = do
  defs <- asks (endpointToSettings . (^. teMockIdp))
  srv <- liftIO . Async.async . Warp.runSettings defs $ app
  go `Control.Monad.Catch.finally` liftIO (Async.cancel srv)


-- test applications

-- | Ignore the request and respond with a metainfo file with an invalid signature.
unconditionallyServeFile :: FilePath -> Application
unconditionallyServeFile filepath _req cont = cont . responseLBS status200 [] =<< LBS.readFile filepath


-- auxiliaries

endpointToSettings :: Endpoint -> Warp.Settings
endpointToSettings endpoint = Warp.defaultSettings { Warp.settingsHost = host, Warp.settingsPort = port }
        where
          host :: Warp.HostPreference = Data.String.fromString . cs $ endpoint ^. epHost
          port :: Int = fromIntegral $ endpoint ^. epPort

endpointToURL :: MonadIO m => Endpoint -> m URI
endpointToURL endpoint = either err pure $ parseURI' urlst
  where
    urlst = "http://" <> host <> ":" <> port
    host  = cs $ endpoint ^. epHost
    port  = cs . show $ endpoint ^. epPort
    err   = liftIO . throwIO . ErrorCall . show . (, (endpoint, urlst))
