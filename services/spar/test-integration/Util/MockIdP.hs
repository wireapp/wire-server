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

import qualified Bilge
import qualified Control.Concurrent.Async          as Async
import qualified Data.X509                         as X509
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp


-- serving an application

withMockIdP
    :: forall a m. (MonadIO m, MonadMask m, MonadReader TestEnv m)
    => Application -> m a -> m a
withMockIdP app go = do
  defs <- asks (endpointToSettings . (^. teTstOpts . to cfgMockIdp . to mockidpBind))
  srv <- liftIO . Async.async . Warp.runSettings defs $ app
  go `Control.Monad.Catch.finally` liftIO (Async.cancel srv)


-- test applications

serveSampleIdP
  :: HasCallStack
  => NewIdP
  -> IO Issuer  -- ^ every call to `/meta` is responded with a fresh issuer.  this is what we
                -- want in most test cases: every idp is created only once.  to testing what
                -- happens if we try to register the same idp twice, we just need to submit
                -- custom metadata through the 'TChan'.
  -> URI  -- ^ request URI
  -> IO (Application, TChan [Node])
serveSampleIdP newidp mkissuer requri = do
  let mkMetaDflt = do
        issuer <- mkissuer
        sampleIdPMetadata newidp issuer requri
  chan <- atomically newTChan
  let getNextMeta = maybe mkMetaDflt pure =<< atomically (tryReadTChan chan)
      app req cont = case pathInfo req of
        ["meta"] -> cont . responseLBS status200 [] . renderLBS def . nodesToDoc =<< getNextMeta
        ["resp"] -> cont $ responseLBS status400 [] ""  -- we do that without the mock server, via 'mkAuthnResponse'
        _        -> cont $ responseLBS status404 [] ""
  pure (app, chan)

sampleIdPMetadata :: MonadIO m => NewIdP -> Issuer -> URI -> m [Node]
sampleIdPMetadata newidp issuer requri = sampleIdPMetadata' sampleIdPPrivkey (newidp ^. nidpPublicKey) issuer requri

sampleIdPMetadata' :: MonadIO m => SignPrivCreds -> X509.SignedCertificate -> Issuer -> URI -> m [Node]
sampleIdPMetadata' privKey cert issuer requri = liftIO $ signElementIO privKey [xml|
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
    entityID = renderURI $ issuer ^. fromIssuer
    authnUrl = renderURI $ requri
    certNodes = case parseLBS def . cs . renderKeyInfo $ cert of
      Right (Document _ sc _) -> [NodeElement sc]
      bad -> error $ show bad


-- auxiliaries

endpointToReq :: Endpoint -> (Bilge.Request -> Bilge.Request)
endpointToReq ep = Bilge.host (ep ^. epHost . to cs) . Bilge.port (ep ^. epPort)

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
