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
  ( app, api
  , API
  , APIMeta
  , APIAuthReq
  , APIAuthResp
  , IdpGet
  , IdpCreate
  , IdpDelete
  ) where

import Control.Monad.Except
import Data.Aeson.QQ (aesonQQ)
import Data.Maybe (isJust, fromJust)
import Data.Proxy
import Data.String.Conversions (ST)
import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.
import GHC.Stack
import Lens.Micro
import Servant
import Servant.Swagger
import Spar.API.Instances ()
import Spar.API.Swagger ()
import Spar.App
import Spar.Options
import Spar.Types
import Web.Cookie (SetCookie)

import qualified Data.Aeson as Aeson
import qualified Brig.Types.User as Brig
import qualified Data.Id as Brig
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Brig
import qualified URI.ByteString as URI


-- FUTUREWORK: use servant-generic?

app :: Env -> Application
app ctx = SAML.setHttpCachePolicy
        $ serve (Proxy @API) (enter (NT (SAML.nt @Spar ctx)) (api $ sparCtxOpts ctx) :: Server API)

type API = "i" :> "status" :> Get '[JSON] NoContent
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

-- FUTUREWORK (thanks jschaul): In a more recent version of servant, using Header '[Strict] becomes
-- an option, removing the need for the Maybe and the extra checks. Probably once
-- https://github.com/wireapp/wire-server/pull/373 is merged this can be done.

api :: Opts -> ServerT API Spar
api opts =
       pure NoContent
  :<|> pure (toSwagger (Proxy @API))
  :<|> SAML.meta appName (Proxy @API) (Proxy @APIAuthResp)
  :<|> SAML.authreq (maxttlAuthreqDiffTime opts)
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
  validateNewIdP newIdP
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

validateNewIdP :: (MonadError ServantErr m) => NewIdP -> m ()
validateNewIdP _idp = pure ()
-- TODO:
-- [aesonQQ|{"error":"not a SAML metainfo URL"}|]
-- [aesonQQ|{"error":"invalid or unresponsive request URL"}|]
-- [aesonQQ|{"error":"public keys in request body and metainfo do not match"}|]
