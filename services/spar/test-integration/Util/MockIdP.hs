{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Util.MockIdP where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.String
import Data.String.Conversions
import GHC.Stack
import Lens.Micro
import Network.HTTP.Types as HTTP
import Network.Wai
import SAML2.WebSSO
import SAML2.WebSSO.Test.Credentials
import Text.Hamlet.XML
import Text.XML
import Text.XML.DSig
import Text.XML.Util
import URI.ByteString
import Util.Options
import Util.Types

import qualified Control.Concurrent.Async          as Async
import qualified Data.X509                         as X509
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp


-- serving an application

withMockIdP
    :: forall a m. (MonadIO m, MonadMask m, MonadReader TestEnv m)
    => Application -> m a -> m a
withMockIdP app go = do
  defs <- asks (endpointToSettings . (^. teIdPEndpoint))
  srv <- liftIO . Async.async . Warp.runSettings defs $ app
  go `Control.Monad.Catch.finally` liftIO (Async.cancel srv)


-- test applications

serveSampleIdP :: HasCallStack => NewIdP -> IO (Application, TChan [Node])
serveSampleIdP newidp = do
  metaDflt <- sampleIdPMetadata newidp
  chan <- atomically newTChan
  let getNextMeta = atomically $ fromMaybe metaDflt <$> tryReadTChan chan
      app req cont = case pathInfo req of
        ["meta"] -> cont . responseLBS status200 [] . renderLBS def . nodesToDoc =<< getNextMeta
        ["resp"] -> cont $ responseLBS status400 [] ""  -- we do that without the mock server, via 'mkAuthnResponse'
        bad      -> error $ show bad
  pure (app, chan)

sampleIdPMetadata :: NewIdP -> IO [Node]
sampleIdPMetadata newidp = sampleIdPMetadata' sampleIdPPrivkey (newidp ^. nidpPublicKey) newidp

sampleIdPMetadata' :: SignPrivCreds -> X509.SignedCertificate -> NewIdP -> IO [Node]
sampleIdPMetadata' privKey cert newidp = signElementIO privKey [xml|
    <EntityDescriptor
      ID="#{descID}"
      entityID="#{entityID}"
      xmlns="urn:oasis:names:tc:SAML:2.0:metadata">
        <IDPSSODescriptor protocolSupportEnumeration="urn:oasis:names:tc:SAML:2.0:protocol">
            <KeyDescriptor use="signing">
                ^{certNodes}
            <SingleSignOnService Binding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST" Location="#{authnUrl}">
  |]
  where
    descID = "_0c29ba62-a541-11e8-8042-873ef87bdcba"
    entityID = renderURI . _fromIssuer $ newidp ^. nidpIssuer
    authnUrl = newidp ^. nidpRequestUri . to renderURI
    certNodes = case parseLBS def . cs . renderKeyInfo $ cert of
      Right (Document _ sc _) -> [NodeElement sc]
      bad -> error $ show bad


-- auxiliaries

endpointToSettings :: Endpoint -> Warp.Settings
endpointToSettings endpoint = Warp.defaultSettings { Warp.settingsHost = host, Warp.settingsPort = port }
  where
    host :: Warp.HostPreference = Data.String.fromString . cs $ endpoint ^. epHost
    port :: Int = fromIntegral $ endpoint ^. epPort

endpointToURL :: MonadIO m => Endpoint -> ST -> m URI
endpointToURL endpoint path = either err pure $ parseURI' urlst
  where
    urlst = "http://" <> host <> ":" <> port <> "/" <> path
    host  = cs $ endpoint ^. epHost
    port  = cs . show $ endpoint ^. epPort
    err   = liftIO . throwIO . ErrorCall . show . (, (endpoint, urlst))
