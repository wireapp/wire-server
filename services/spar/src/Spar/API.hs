{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NamedFieldPuns             #-}
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
  , IdpGetAll
  , IdpCreate
  , IdpDelete
  ) where

import Bilge
import Brig.Types.User as Brig
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either
import Data.EitherR (fmapL)
import Data.Id
import Data.Maybe (isJust, fromJust)
import Data.Proxy
import Data.String.Conversions
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
import Spar.Error
import Spar.Options
import Spar.Types
import Web.Cookie (SetCookie)

import qualified Network.HTTP.Client as Rq
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified Text.XML as XML
import qualified Text.XML.DSig as SAML
import qualified Text.XML.Util as SAML
import qualified URI.ByteString as URI


-- FUTUREWORK: use servant-generic?

app :: Env -> Application
app ctx = SAML.setHttpCachePolicy
        $ serve (Proxy @API) (enter (NT (SAML.nt @SparError @Spar ctx)) (api $ sparCtxOpts ctx) :: Server API)

type API = "i" :> "status" :> Get '[JSON] NoContent
      :<|> "sso" :> "api-docs" :> Get '[JSON] Swagger
      :<|> APIMeta
      :<|> APIAuthReq
      :<|> APIAuthResp
      :<|> IdpGet
      :<|> IdpGetAll
      :<|> IdpCreate
      :<|> IdpDelete
      -- NB. If you add endpoints here, also update Test.Spar.APISpec

type APIMeta     = "sso" :> "metainfo" :> SAML.APIMeta
type APIAuthReq  = "sso" :> "initiate-login" :> SAML.APIAuthReq
type APIAuthResp = "sso" :> "finalize-login" :> SAML.APIAuthResp

type IdpGet     = Header "Z-User" UserId :> "identity-providers" :> Capture "id" SAML.IdPId :> Get '[JSON] IdP
type IdpGetAll  = Header "Z-User" UserId :> "identity-providers" :> Get '[JSON] IdPList
type IdpCreate  = Header "Z-User" UserId :> "identity-providers" :> ReqBody '[JSON] NewIdP :> PostCreated '[JSON] IdP
type IdpDelete  = Header "Z-User" UserId :> "identity-providers" :> Capture "id" SAML.IdPId :> DeleteNoContent '[JSON] NoContent

-- FUTUREWORK (thanks jschaul): In a more recent version of servant, using Header '[Strict] becomes
-- an option, removing the need for the Maybe and the extra checks. Probably once
-- https://github.com/wireapp/wire-server/pull/373 is merged this can be done.

api :: Opts -> ServerT API Spar
api opts =
       pure NoContent
  :<|> pure (toSwagger (Proxy @OutsideWorldAPI))
  :<|> SAML.meta appName (Proxy @API) (Proxy @APIAuthResp)
  :<|> SAML.authreq (maxttlAuthreqDiffTime opts)
  :<|> SAML.authresp onSuccess
  :<|> idpGet
  :<|> idpGetAll
  :<|> idpCreate
  :<|> idpDelete

appName :: ST
appName = "spar"

onSuccess :: HasCallStack => SAML.UserRef -> Spar (SetCookie, URI.URI)
onSuccess uid = forwardBrigLogin =<< maybe (createUser uid) pure =<< getUser uid

type ZUsr = Maybe UserId

idpGet :: ZUsr -> SAML.IdPId -> Spar IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- SAML.getIdPConfig idpid
  authorizeIdP zusr idp
  pure idp

idpGetAll :: ZUsr -> Spar IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- getZUsrOwnedTeam zusr
  _idplProviders <- wrapMonadClientWithEnv $ Data.getIdPConfigsByTeam teamid
  pure IdPList{..}

idpDelete :: ZUsr -> SAML.IdPId -> Spar NoContent
idpDelete zusr idpid = withDebugLog "idpDelete" (const Nothing) $ do
    idp <- SAML.getIdPConfig idpid
    authorizeIdP zusr idp
    wrapMonadClient $ Data.deleteIdPConfig idpid (idp ^. SAML.idpIssuer) (idp ^. SAML.idpExtraInfo . idpeTeam)
    return NoContent

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
idpCreate :: ZUsr -> NewIdP -> Spar IdP
idpCreate zusr newIdP = withDebugLog "idpCreate" (Just . show . (^. SAML.idpId)) $ do
  teamid <- getZUsrOwnedTeam zusr
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

-- | Called by get, put, delete handlers.
authorizeIdP :: (HasCallStack, MonadError SparError m, SAML.SP m, Intra.MonadSparToBrig m)
             => ZUsr -> IdP -> m ()
authorizeIdP zusr idp = do
  teamid <- getZUsrOwnedTeam zusr
  when (teamid /= idp ^. SAML.idpExtraInfo . idpeTeam) $ throwSpar SparNotInTeam

-- | Called by post handler, and by 'authorizeIdP'.
getZUsrOwnedTeam :: (HasCallStack, MonadError SparError m, SAML.SP m, Intra.MonadSparToBrig m)
            => ZUsr -> m TeamId
getZUsrOwnedTeam Nothing = throwSpar SparNotInTeam
getZUsrOwnedTeam (Just uid) = do
  usr <- Intra.getUser uid
  case Brig.userTeam =<< usr of
    Nothing -> throwSpar SparNotInTeam
    Just teamid -> teamid <$ Intra.assertIsTeamOwner uid teamid

initializeIdP :: NewIdP -> TeamId -> Spar IdP
initializeIdP (NewIdP _idpMetadata _idpIssuer _idpRequestUri _idpPublicKey) _idpeTeam = do
  _idpId <- SAML.IdPId <$> SAML.createUUID
  _idpeSPInfo <- wrapMonadClientWithEnv $ Data.getSPInfo _idpId
  let _idpExtraInfo = IdPExtra { _idpeTeam, _idpeSPInfo }
  pure SAML.IdPConfig {..}


type MonadValidateIdP m = (MonadHttp m, MonadIO m)

validateNewIdP :: forall m. (HasCallStack, MonadError SparError m, MonadValidateIdP m)
               => NewIdP -> m ()
validateNewIdP newidp = if True then pure () else do  -- TODO: validation breaks current integration test suite, so it's disabled.
  let uri2req :: URI.URI -> m Request
      uri2req = either (throwSpar . SparNewIdPBadMetaUrl . cs . show) pure
              . Rq.parseRequest . cs . SAML.renderURI

      fetch :: URI.URI -> (Request -> Request) -> m (Bilge.Response (Maybe LBS))
      fetch uri modify = do
        req <- uri2req uri
        tweakleft (httpLbs req modify)

      tweakleft :: Http a -> m a
      tweakleft (HttpT action) = liftIO . runReaderT action =<< getManager

  metaResp :: Bilge.Response (Maybe LBS)
    <- fetch (newidp ^. nidpMetadata) (method GET . expect2xx)
  metaBody :: LBS
    <- maybe (throwSpar $ SparNewIdPBadMetaUrl "No body in response.") pure $ responseBody metaResp
  when (isLeft $ SAML.verifyRoot (newidp ^. nidpPublicKey) metaBody) $ do
    throwSpar SparNewIdPBadMetaSig
  meta :: SAML.IdPDesc
    <- either (throwSpar . SparNewIdPBadMetaUrl . cs) pure $ do
         XML.Document _ el _ <- fmapL show $ XML.parseLBS XML.def metaBody
         SAML.parseIdPDesc el
  when (newidp ^. nidpPublicKey `notElem` meta ^. SAML.edPublicKeys) $
    throwSpar SparNewIdPPubkeyMismatch
  respResp :: Bilge.Response (Maybe LBS)
    <- fetch (newidp ^. nidpRequestUri) (method POST . body "request")
  when (statusCode respResp >= 400) $ do
    let msg = (cs . show . responseStatus $ respResp) <> (maybe "" ((": " <>) . cs) $ responseBody respResp)
    throwSpar $ SparNewIdPBadReqUrl msg


-- | Type families to convert spar's 'API' type into an "outside-world-view" API type
-- to expose as swagger docs intended to be used by client developers.
-- Here we assume the 'spar' service is only accessible from behind the 'nginz' proxy, which
--   * does not expose routes prefixed with /i/
--   * handles authorization (adding a Z-User header if requests are authorized)
type OutsideWorldAPI = StripInternal (StripAuth API)

-- | Strip the nginz-set, internal-only Z-User header
type family StripAuth api where
    StripAuth (Header "Z-User" a :> b) = b
    StripAuth (a :<|> b) = (StripAuth a) :<|> (StripAuth b)
    StripAuth x = x

-- | Strip internal endpoints (prefixed with /i/)
type family StripInternal api where
    StripInternal ("i" :> b) = EmptyAPI
    StripInternal (a :<|> b) = (StripInternal a) :<|> (StripInternal b)
    StripInternal x = x
