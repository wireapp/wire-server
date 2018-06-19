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
import Data.Metrics (metrics)
import Data.String.Conversions (cs)
import Data.String (fromString)
import GHC.Stack
import Network.HTTP.Client (responseTimeoutMicro)
import Servant
import Spar.App

-- TODO:
-- to use Header' '[Strict] (https://github.com/haskell-servant/servant/blob/6d1ae0dccdf4c499d526187fd910297d54121b64/servant/src/Servant/API/Header.hs#L24) we need a more recent version of servant
-- import Servant.API.Header
import Spar.Options
import Spar.Types
import Util.Options (epHost, epPort)
import Data.Text (Text)

import qualified Data.Text as ST
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Utilities.Server as WU
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified System.Logger as Log

runServer :: Opts -> IO ()
runServer sparCtxOpts = do
  sparCtxLogger <- Log.new $ Log.defSettings
                   & Log.setLogLevel (toLevel $ saml sparCtxOpts ^. SAML.cfgLogLevel)
  mx <- metrics
  sparCtxCas <- Data.initCassandra sparCtxOpts sparCtxLogger
  let settings = Warp.defaultSettings
        & Warp.setHost (fromString $ sparCtxOpts ^. to saml . SAML.cfgSPHost)
        . Warp.setPort (sparCtxOpts ^. to saml . SAML.cfgSPPort)
  sparCtxHttpManager <- newManager defaultManagerSettings
      { managerResponseTimeout = responseTimeoutMicro (10 * 1000 * 1000)
      }
  let sparCtxHttpBrig = Bilge.host (sparCtxOpts ^. to brig . epHost . to cs)
                      . Bilge.port (sparCtxOpts ^. to brig . epPort)
                      $ Bilge.empty
  let wrappedApp
    -- . WU.measureRequests mx _
        -- TODO: we need the swagger sitemap from servant for this.  we also want this to be
        -- prometheus-compatible.  not sure about the order in which to do these.
        = WU.catchErrors sparCtxLogger mx
        . SAML.setHttpCachePolicy
        $ app Env {..}
  WU.runSettingsWithShutdown settings wrappedApp 5

-- FUTUREWORK: use servant-generic?

app :: Env -> Application
app ctx = SAML.setHttpCachePolicy
        $ serve (Proxy @API) (enter (NT (SAML.nt @Spar ctx)) api :: Server API)

type API = "i" :> "status" :> Get '[JSON] ()
      :<|> APIMeta
      :<|> APIAuthReq
      :<|> APIAuthResp
      :<|> IdpGet
      :<|> IdpCreate

type APIMeta     = "sso" :> "metainfo" :> SAML.APIMeta
type APIAuthReq  = "sso" :> "initiate-login" :> SAML.APIAuthReq
type APIAuthResp = "sso" :> "complete-login" :> SAML.APIAuthResp

type IdpGet     = Header "Z-User" Text :> "sso" :> "identity-providers" :> Capture "id" IdPId :> Get '[JSON] IDP
type IdpCreate  = Header "Z-User" Text :> "sso" :> "identity-providers" :> ReqBody '[JSON] NewIdP :> PostCreated '[JSON] IDP
type IdpDelete  = Header "Z-User" Text :> "sso" :> "identity-providers" :> Capture "id" IdPId :> DeleteNoContent '[JSON] NoContent


api :: ServerT API Spar
api =  pure ()
  :<|> SAML.meta appName (Proxy @API) (Proxy @APIAuthResp)
  :<|> SAML.authreq
  :<|> SAML.authresp onSuccess
  :<|> idpGet
  :<|> idpCreate

appName :: ST.Text
appName = "spar"

onSuccess :: HasCallStack => SAML.UserId -> Spar SAML.Void
onSuccess uid = forwardBrigLogin =<< maybe (createUser uid) pure =<< getUser uid

type ZUsr = Maybe Text -- TODO: use Brig.UserId, requires some to/from http instances

idpGet :: ZUsr -> IdPId -> Spar IDP
idpGet Nothing _ = throwError err403
idpGet (Just _zusr) _idpId = do
    -- TODO: ensure zusr belongs to a team allowed to see/change this idp
    let issuer = undefined :: SAML.Issuer -- TODO: what's the key? Create fresh UUID? base64 of issuer?
    SAML.getIdPConfigByIssuer issuer

idpCreate :: ZUsr -> IDP -> Spar IDP
idpCreate Nothing _ = throwError err403
idpCreate (Just _zusr) idp = do
    -- TODO: ensure zusr belongs to a team allowed to see/change this idp
    -- TODO: call brig to get the teamId using the zusr
    -- TODO: create uuid?
    SAML.storeIdPConfig idp
    return idp
