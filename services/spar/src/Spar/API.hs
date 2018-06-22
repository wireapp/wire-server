{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import Control.Monad.Except
import Data.Metrics (metrics)
import Data.String.Conversions (ST, cs)
import Data.String (fromString)
import GHC.Stack
import Network.HTTP.Client (responseTimeoutMicro)
import Servant
import Spar.API.Instances ()
import Spar.App
import Spar.Options
import Spar.Types
import Util.Options (epHost, epPort)

import qualified Brig.Types.User as Brig
import qualified Data.Id as Brig
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Utilities.Server as WU
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Brig
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

type IdpGet     = Header "Z-User" Brig.UserId :> "sso" :> "identity-providers" :> Capture "id" SAML.IdPId :> Get '[JSON] IdP
type IdpCreate  = Header "Z-User" Brig.UserId :> "sso" :> "identity-providers" :> ReqBody '[JSON] NewIdP :> PostCreated '[JSON] IdP
-- TODO: type IdpDelete  = Header "Z-User" Brig.UserId :> "sso" :> "identity-providers" :> Capture "id" SAML.IdPId :> DeleteNoContent '[JSON] NoContent


api :: ServerT API Spar
api =  pure ()
  :<|> SAML.meta appName (Proxy @API) (Proxy @APIAuthResp)
  :<|> SAML.authreq
  :<|> SAML.authresp onSuccess
  :<|> idpGet
  :<|> idpCreate

appName :: ST
appName = "spar"

onSuccess :: HasCallStack => SAML.UserRef -> Spar SAML.Void
onSuccess uid = forwardBrigLogin =<< maybe (createUser uid) pure =<< getUser uid

type ZUsr = Maybe Brig.UserId

idpGet :: ZUsr -> SAML.IdPId -> Spar IdP
idpGet zusr idpid = authorizeIdP zusr =<< SAML.getIdPConfig idpid

-- We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness
idpCreate :: ( SAML.SP m, SAML.SPStoreIdP m, SAML.ConfigExtra m ~ Brig.TeamId
             , MonadError ServantErr m
             , Brig.MonadSparToBrig m
             )
          => ZUsr -> NewIdP -> m IdP
idpCreate zusr newIdP = do
  teamid <- getZUsrTeam zusr
  idp <- initializeIdP newIdP teamid
  SAML.storeIdPConfig idp
  pure idp

authorizeIdP :: (HasCallStack, MonadError ServantErr m, Brig.MonadSparToBrig m)
             => ZUsr -> IdP -> m IdP
authorizeIdP Nothing _ = throwError err403 { errBody = "Auth token required" }
authorizeIdP zusr idp = do
  teamid <- getZUsrTeam zusr
  if teamid == idp ^. SAML.idpExtraInfo
    then pure idp
    else throwError err403 { errBody = "Wrong or invalid auth token or not in a team" }

getZUsrTeam :: (HasCallStack, MonadError ServantErr m, Brig.MonadSparToBrig m)
            => ZUsr -> m Brig.TeamId
getZUsrTeam Nothing = throwError err403 { errBody = "Auth token required" }
getZUsrTeam (Just uid) = do
  usr <- Brig.getUser uid
  case Brig.userTeam =<< usr of
    Nothing -> throwError err403 { errBody = "Wrong or invalid auth token or not in a team" }
    Just teamid -> pure teamid

initializeIdP :: (MonadError ServantErr m, SAML.SP m) => NewIdP -> Brig.TeamId -> m IdP
initializeIdP (NewIdP _idpMetadata _idpIssuer _idpRequestUri _idpPublicKey) _idpExtraInfo = do
  _idpId <- SAML.IdPId <$> SAML.createUUID
  pure SAML.IdPConfig {..}
