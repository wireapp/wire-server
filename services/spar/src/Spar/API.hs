{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Spar.API where

import Bilge
import Control.Lens
import Data.String.Conversions (cs)
import Data.String (fromString)
import GHC.Stack
import Network.HTTP.Client (responseTimeoutMicro)
import Servant
import Spar.App
import Spar.Options
import Util.Options (epHost, epPort)

import qualified Data.Text as ST
import qualified Network.Wai.Handler.Warp as Warp
import qualified SAML2.WebSSO as SAML
import qualified System.Logger as Log


-- TODO: shutdown?
-- TODO: prometheus-compatible metrics.
runServer :: Opts -> IO ()
runServer sparCtxOpts = do
  sparCtxLogger <- Log.new $ Log.defSettings
                   & Log.setLogLevel (toLevel $ saml sparCtxOpts ^. SAML.cfgLogLevel)
  sparCtxCas <- initCassandra sparCtxOpts sparCtxLogger
  let settings = Warp.defaultSettings
        & Warp.setHost (fromString $ sparCtxOpts ^. to saml . SAML.cfgSPHost)
        . Warp.setPort (sparCtxOpts ^. to saml . SAML.cfgSPPort)
  sparCtxHttpManager <- newManager defaultManagerSettings
      { managerResponseTimeout = responseTimeoutMicro (10 * 1000 * 1000)
      }
  let sparCtxHttpBrig = Bilge.host (sparCtxOpts ^. to brig . epHost . to cs)
                      . Bilge.port (sparCtxOpts ^. to brig . epPort)
                      $ Bilge.empty
  Warp.runSettings settings $ app SparCtx {..}

-- TODO: rename Ctx to Env like everywhere else.
-- FUTUREWORK: use servant-generic?

app :: SparCtx -> Application
app ctx = SAML.setHttpCachePolicy
        $ serve (Proxy @API) (enter (NT (SAML.nt @Spar ctx)) api :: Server API)

type API = "i" :> "status" :> Get '[JSON] ()
      :<|> APIMeta
      :<|> APIAuthReq
      :<|> APIAuthResp

type APIMeta     = "sso" :> "info" :> SAML.APIMeta
type APIAuthReq  = "sso" :> "initiate-login" :> SAML.APIAuthReq
type APIAuthResp = "sso" :> "complete-login" :> SAML.APIAuthResp


api :: ServerT API Spar
api =  pure ()
  :<|> SAML.meta appName (Proxy @API) (Proxy @APIAuthResp)
  :<|> SAML.authreq
  :<|> SAML.authresp onSuccess

appName :: ST.Text
appName = "spar"

onSuccess :: HasCallStack => SAML.UserId -> Spar SAML.Void
onSuccess uid = forwardBrigLogin =<< maybe (createUser uid) pure =<< getUser uid


-- TODO: restructure: cassandra init, runServer in one module; the rest of App and API in another.
