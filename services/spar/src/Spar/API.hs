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

import Control.Lens
import Data.String (fromString)
import GHC.Stack
import Servant
import Spar.App
import Spar.Options

import qualified Data.Text as ST
import qualified Network.Wai.Handler.Warp as Warp
import qualified SAML2.WebSSO as SAML
import qualified System.Logger as Log


-- TODO: redundant cluster?
-- TODO: shutdown?
runServer :: Opts -> IO ()
runServer sparCtxOpts = do
  sparCtxLogger <- Log.new $ Log.defSettings
                   & Log.setLogLevel (toLevel $ saml sparCtxOpts ^. SAML.cfgLogLevel)
  sparCtxCas <- initCassandra sparCtxOpts sparCtxLogger
  let settings = Warp.defaultSettings
        & Warp.setHost (fromString $ sparCtxOpts ^. to saml . SAML.cfgSPHost)
        . Warp.setPort (sparCtxOpts ^. to saml . SAML.cfgSPPort)
  Warp.runSettings settings $ app SparCtx {..}

app :: SparCtx -> Application
app ctx = SAML.setHttpCachePolicy
        $ serve (Proxy @API) (enter (NT (SAML.nt @Spar ctx)) api :: Server API)

type API = "i" :> "status" :> Get '[JSON] ()
      :<|> APIMeta
      :<|> APIAuthReq
      :<|> APIAuthResp

type APIMeta     = "meta" :> SAML.APIMeta
type APIAuthReq  = "authreq" :> SAML.APIAuthReq
type APIAuthResp = "authresp" :> SAML.APIAuthResp


api :: ServerT API Spar
api =  pure ()
  :<|> SAML.meta appName (Proxy @API) (Proxy @APIAuthResp)
  :<|> SAML.authreq
  :<|> SAML.authresp onSuccess

appName :: ST.Text
appName = "spar"

onSuccess :: (HasCallStack, MonadSpar m) => SAML.UserId -> m SAML.Void
onSuccess uid = forwardBrigLogin =<< maybe (createUser uid) pure =<< getUser uid
