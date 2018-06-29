{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API
  ( runServer, app, api
  , API
  , APIMeta
  , APIAuthReq
  , APIAuthResp
  , IdpGet
  , IdpCreate
  , IdpDelete
  ) where

import Bilge
import Control.Monad.Except
import Data.Aeson.QQ (aesonQQ)
import Data.Maybe (isJust, fromJust)
import Data.Metrics (metrics)
import Data.Proxy
import Data.String.Conversions (ST, cs)
import Data.String (fromString)
import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.
import GHC.Stack
import Lens.Micro
import Network.HTTP.Client (responseTimeoutMicro)
import Servant
import Servant.Swagger
import Spar.API.Instances ()
import Spar.App
import Spar.Options
import Spar.Types
import Util.Options (epHost, epPort)
import Web.Cookie (SetCookie)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson as Swagger
import qualified Data.Scientific as Swagger
import qualified Brig.Types.User as Brig
import qualified Data.Id as Brig
import qualified Data.X509 as X509
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Utilities.Server as WU
import qualified SAML2.WebSSO as SAML
import qualified Servant.Multipart as SM
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Brig
import qualified System.Logger as Log
import qualified URI.ByteString as URI

runServer :: Opts -> IO ()
runServer sparCtxOpts = do
  sparCtxLogger <- Log.new $ Log.defSettings
                   & Log.setLogLevel (toLevel $ saml sparCtxOpts ^. SAML.cfgLogLevel)
                   & Log.setOutput Log.StdOut
                   & Log.setFormat Nothing
                   & Log.setNetStrings (logNetStrings sparCtxOpts)
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

type API = "i" :> "status" :> GetNoContent '[JSON] NoContent
      :<|> "sso" :> "api-docs" :> Get '[JSON] Swagger
      :<|> APIMeta
      :<|> APIAuthReq
      :<|> APIAuthResp
      :<|> IdpGet
      :<|> IdpCreate
      :<|> IdpDelete

type APIMeta     = "sso" :> "metainfo" :> SAML.APIMeta
type APIAuthReq  = "sso" :> "initiate-login" :> SAML.APIAuthReq
type APIAuthResp = "sso" :> "finalize-login" :> SAML.APIAuthResp

type IdpGet     = Header "Z-User" Brig.UserId :> "sso" :> "identity-providers" :> Capture "id" SAML.IdPId :> Get '[JSON] IdP
type IdpCreate  = Header "Z-User" Brig.UserId :> "sso" :> "identity-providers" :> ReqBody '[JSON] NewIdP :> PostCreated '[JSON] IdP
type IdpDelete  = Header "Z-User" Brig.UserId :> "sso" :> "identity-providers" :> Capture "id" SAML.IdPId :> DeleteNoContent '[JSON] NoContent


api :: ServerT API Spar
api =  pure NoContent
  :<|> pure (toSwagger (Proxy @API))
  :<|> SAML.meta appName (Proxy @API) (Proxy @APIAuthResp)
  :<|> SAML.authreq
  :<|> SAML.authresp onSuccess
  :<|> idpGet
  :<|> idpCreate
  :<|> idpDelete

appName :: ST
appName = "spar"

onSuccess :: HasCallStack => SAML.UserRef -> Spar (SetCookie, URI.URI)
onSuccess uid = forwardBrigLogin =<< maybe (createUser uid) pure =<< getUser uid

type ZUsr = Maybe Brig.UserId

idpGet :: ZUsr -> SAML.IdPId -> Spar IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  authorizeIdP zusr =<< SAML.getIdPConfig idpid

idpDelete :: ZUsr -> SAML.IdPId -> Spar NoContent
idpDelete zusr idpid = withDebugLog "idpDelete" (const Nothing) $ do
    idp <- SAML.getIdPConfig idpid
    void $ authorizeIdP zusr idp
    wrapMonadClient $ Data.deleteIdPConfig idpid (idp ^. SAML.idpIssuer) (idp ^. SAML.idpExtraInfo)
    return NoContent

-- We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness
idpCreate :: ( SAML.SP m, SAML.SPStoreIdP m, SAML.ConfigExtra m ~ Brig.TeamId
             , MonadError ServantErr m
             , Brig.MonadSparToBrig m
             )
          => ZUsr -> NewIdP -> m IdP
idpCreate zusr newIdP = withDebugLog "idpCreate" (Just . show . (^. SAML.idpId)) $ do
  teamid <- getZUsrTeam zusr
  idp <- initializeIdP newIdP teamid
  SAML.storeIdPConfig idp
  pure idp

withDebugLog :: SAML.SP m => String -> (a -> Maybe String) -> m a -> m a
withDebugLog msg showval action = do
  SAML.logger SAML.Debug $ "entering " ++ msg
  val <- action
  let mshowedval = showval val
  SAML.logger SAML.Debug $ "leaving " ++ msg ++ mconcat [": " ++ fromJust mshowedval | isJust mshowedval]
  pure val

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
getZUsrTeam Nothing = throwError err403 { errBody = Aeson.encode [aesonQQ|{"error":"no auth token"}|] }
getZUsrTeam (Just uid) = do
  usr <- Brig.getUser uid
  case Brig.userTeam =<< usr of
    Nothing -> throwError err403 { errBody = Aeson.encode [aesonQQ|{"error":"you need to be team admin to create an IdP"}|] }
    Just teamid -> pure teamid

initializeIdP :: (MonadError ServantErr m, SAML.SP m) => NewIdP -> Brig.TeamId -> m IdP
initializeIdP (NewIdP _idpMetadata _idpIssuer _idpRequestUri _idpPublicKey) _idpExtraInfo = do
  _idpId <- SAML.IdPId <$> SAML.createUUID
  pure SAML.IdPConfig {..}


----------------------------------------------------------------------
-- swagger

-- FUTUREWORK: push orphans upstream to saml2-web-sso, servant-multipart

-- TODO: steal from https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

instance ToSchema Swagger where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())
    & mapped . schema . description ?~ "The swagger docs you are looking at (all details hidden)."

instance HasSwagger route => HasSwagger (SM.MultipartForm SM.Mem resp :> route) where
  toSwagger _proxy = toSwagger (Proxy @route)

instance ToParamSchema Brig.TeamId
instance ToParamSchema Brig.UserId
instance ToParamSchema SAML.IdPId
instance ToSchema Brig.TeamId
instance ToSchema Brig.UserId
instance ToSchema NewIdP
instance ToSchema SAML.AuthnRequest
instance ToSchema (SAML.FormRedirect SAML.AuthnRequest)
instance ToSchema (SAML.IdPConfig Brig.TeamId)  -- TODO: would be nice to add an example here, but that only works for json?
instance ToSchema SAML.IdPId
instance ToSchema (SAML.ID SAML.AuthnRequest)
instance ToSchema SAML.Issuer
instance ToSchema SAML.Time
instance ToSchema SAML.Version

instance ToSchema X509.SignedCertificate where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())

instance ToSchema SAML.SPDesc where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())

instance ToSchema URI.URI where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())

instance ToParamSchema SetCookie where
  toParamSchema _proxy = mkEmptyParamSchema SwaggerBoolean

mkEmptyParamSchema :: SwaggerType t -> ParamSchema t
mkEmptyParamSchema _paramSchemaType = ParamSchema {..}
  where
    _paramSchemaDefault = Nothing :: Maybe Swagger.Value
    _paramSchemaFormat = Nothing :: Maybe Format
    _paramSchemaItems = Nothing :: Maybe (SwaggerItems t)
    _paramSchemaMaximum = Nothing :: Maybe Swagger.Scientific
    _paramSchemaExclusiveMaximum = Nothing :: Maybe Bool
    _paramSchemaMinimum = Nothing :: Maybe Swagger.Scientific
    _paramSchemaExclusiveMinimum = Nothing :: Maybe Bool
    _paramSchemaMaxLength = Nothing :: Maybe Integer
    _paramSchemaMinLength = Nothing :: Maybe Integer
    _paramSchemaPattern = Nothing :: Maybe Pattern
    _paramSchemaMaxItems = Nothing :: Maybe Integer
    _paramSchemaMinItems = Nothing :: Maybe Integer
    _paramSchemaUniqueItems = Nothing :: Maybe Bool
    _paramSchemaEnum = Nothing :: Maybe [Swagger.Value]
    _paramSchemaMultipleOf = Nothing :: Maybe Swagger.Scientific
