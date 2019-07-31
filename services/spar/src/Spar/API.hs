{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

-- | The API types, handlers, and WAI 'Application' for whole Spar.
--
-- Note: handlers are defined here, but API types are reexported from "Spar.API.Types". The
-- SCIM branch of the API is fully defined in "Spar.Scim".
module Spar.API
  ( -- * Server
    app, api

    -- * API types
  , API, OutsideWorldAPI
    -- ** Individual API pieces
  , APIAuthReqPrecheck
  , APIAuthReq
  , APIAuthResp
  , IdpGet
  , IdpGetAll
  , IdpCreateXML
  , IdpCreateJSON
  , IdpDelete
  ) where

import Imports
import Control.Lens
import Control.Monad.Except
import Data.Id
import Data.Proxy
import Data.String.Conversions
import Data.Time
import OpenSSL.Random (randBytes)
import Servant
import Servant.Swagger
import Spar.API.Swagger ()
import Spar.API.Types
import Spar.App
import Spar.Error
import Spar.Orphans ()
import Spar.Scim
import Spar.Scim.Swagger ()
import Spar.Types

import qualified Bilge as Bilge
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Base64 as ES
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified URI.ByteString as URI
import qualified Web.Cookie as Cky


app :: Env -> Application
app ctx = SAML.setHttpCachePolicy
        $ serve (Proxy @API) (hoistServer (Proxy @API) (SAML.nt @SparError @Spar ctx) (api $ sparCtxOpts ctx) :: Server API)

api :: Opts -> ServerT API Spar
api opts
     = apiSSO opts
  :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateBind
  :<|> apiIDP
  :<|> apiScim
  :<|> apiINTERNAL

apiSSO :: Opts -> ServerT APISSO Spar
apiSSO opts
     = pure (toSwagger (Proxy @OutsideWorldAPI))
  :<|> SAML.meta appName sparSPIssuer sparResponseURI
  :<|> authreqPrecheck
  :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateLogin
  :<|> authresp

apiIDP :: ServerT APIIDP Spar
apiIDP
     = idpGet
  :<|> idpGetAll
  :<|> idpCreateXML
  :<|> idpCreateXML
  :<|> idpCreateJSON
  :<|> idpDelete

apiINTERNAL :: ServerT APIINTERNAL Spar
apiINTERNAL
     = internalStatus
  :<|> internalDeleteTeam


appName :: ST
appName = "spar"

----------------------------------------------------------------------------
-- SSO API

authreqPrecheck :: Maybe URI.URI -> Maybe URI.URI -> SAML.IdPId -> Spar NoContent
authreqPrecheck msucc merr idpid = validateAuthreqParams msucc merr
                                *> SAML.getIdPConfig idpid
                                *> return NoContent

authreq :: NominalDiffTime -> DoInitiate -> Maybe UserId -> Maybe URI.URI -> Maybe URI.URI -> SAML.IdPId
        -> Spar (WithSetBindCookie (SAML.FormRedirect SAML.AuthnRequest))
authreq _ DoInitiateLogin (Just _) _ _ _ = throwSpar SparInitLoginWithAuth
authreq _ DoInitiateBind Nothing _ _ _   = throwSpar SparInitBindWithoutAuth
authreq authreqttl _ zusr msucc merr idpid = do
  vformat <- validateAuthreqParams msucc merr
  form@(SAML.FormRedirect _ ((^. SAML.rqID) -> reqid)) <- SAML.authreq authreqttl sparSPIssuer idpid
  wrapMonadClient $ Data.storeVerdictFormat authreqttl reqid vformat
  cky <- initializeBindCookie zusr authreqttl
  SAML.logger SAML.Debug $ "setting bind cookie: " <> show cky
  pure $ addHeader cky form

-- | If the user is already authenticated, create bind cookie with a given life expectancy and our
-- domain, and store it in C*.  If the user is not authenticated, return a deletion 'SetCookie'
-- value that deletes any bind cookies on the client.
initializeBindCookie :: Maybe UserId -> NominalDiffTime -> Spar SetBindCookie
initializeBindCookie zusr authreqttl = do
  DerivedOpts { derivedOptsBindCookiePath, derivedOptsBindCookieDomain }
    <- asks (derivedOpts . sparCtxOpts)
  msecret <- if isJust zusr
             then liftIO $ Just . cs . ES.encode <$> randBytes 32
             else pure Nothing
  let updSetCkyDom (SAML.SimpleSetCookie raw) = SAML.SimpleSetCookie raw
        { Cky.setCookieDomain = Just derivedOptsBindCookieDomain }
  cky <- updSetCkyDom <$> (SAML.toggleCookie derivedOptsBindCookiePath $
                            (, authreqttl) <$> msecret :: Spar SetBindCookie)
  forM_ zusr $ \userid -> wrapMonadClientWithEnv $ Data.insertBindCookie cky userid authreqttl
  pure cky

redirectURLMaxLength :: Int
redirectURLMaxLength = 140

validateAuthreqParams :: Maybe URI.URI -> Maybe URI.URI -> Spar VerdictFormat
validateAuthreqParams msucc merr = case (msucc, merr) of
  (Nothing, Nothing) -> pure VerdictFormatWeb
  (Just ok, Just err) -> do
    validateRedirectURL `mapM_` [ok, err]
    pure $ VerdictFormatMobile ok err
  _ -> throwSpar $ SparBadInitiateLoginQueryParams "need-both-redirect-urls"

validateRedirectURL :: URI.URI -> Spar ()
validateRedirectURL uri = do
  unless ((SBS.take 4 . URI.schemeBS . URI.uriScheme $ uri) == "wire") $ do
    throwSpar $ SparBadInitiateLoginQueryParams "invalid-schema"
  unless ((SBS.length $ URI.serializeURIRef' uri) <= redirectURLMaxLength) $ do
    throwSpar $ SparBadInitiateLoginQueryParams "url-too-long"


authresp :: Maybe ST -> SAML.AuthnResponseBody -> Spar Void
authresp ckyraw = SAML.authresp sparSPIssuer sparResponseURI go
  where
    cky :: Maybe BindCookie
    cky = ckyraw >>= bindCookieFromHeader

    go :: SAML.AuthnResponse -> SAML.AccessVerdict -> Spar Void
    go resp verdict = do
      result :: SAML.ResponseVerdict <- verdictHandler cky resp verdict
      throwError $ SAML.CustomServant result


----------------------------------------------------------------------------
-- IdP API

idpGet :: Maybe UserId -> SAML.IdPId -> Spar IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- SAML.getIdPConfig idpid
  authorizeIdP zusr idp
  pure idp

idpGetAll :: Maybe UserId -> Spar IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- Intra.getZUsrOwnedTeam zusr
  _idplProviders <- wrapMonadClientWithEnv $ Data.getIdPConfigsByTeam teamid
  pure IdPList{..}

idpDelete :: Maybe UserId -> SAML.IdPId -> Spar NoContent
idpDelete zusr idpid = withDebugLog "idpDelete" (const Nothing) $ do
    idp <- SAML.getIdPConfig idpid
    authorizeIdP zusr idp
    wrapMonadClient $ do
        let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
            team = idp ^. SAML.idpExtraInfo
        -- Delete tokens associated with given IdP (we rely on the fact that
        -- each IdP has exactly one team so we can look up all tokens
        -- associated with the team and then filter them)
        tokens <- Data.getScimTokens team
        for_ tokens $ \ScimTokenInfo{..} ->
            when (stiIdP == Just idpid) $ Data.deleteScimToken team stiId
        -- Delete IdP config
        Data.deleteIdPConfig idpid issuer team
    return NoContent

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
idpCreateXML :: Maybe UserId -> SAML.IdPMetadata -> Spar IdP
idpCreateXML zusr idpmeta = withDebugLog "idpCreate" (Just . show . (^. SAML.idpId)) $ do
  teamid <- Intra.getZUsrOwnedTeam zusr
  idp <- validateNewIdP idpmeta teamid
  SAML.storeIdPConfig idp
  pure idp

idpCreateJSON :: Maybe UserId -> IdPMetadataInfo -> Spar IdP
idpCreateJSON zusr (IdPMetadataValue xml) =
  idpCreateXML zusr xml
idpCreateJSON zusr (IdPMetadataURI uri) =
  enforceHttps uri >> doHttp >>= getBody >>= decodeBody >>= idpCreateXML zusr
  where
    doHttp :: Spar (Bilge.Response (Maybe LBS))
    doHttp = either (throwSpar . SparNewIdPBadMetaUrl . cs) Bilge.get $ Bilge.useURIBS uri

    getBody :: Bilge.Response (Maybe LBS) -> Spar LBS
    getBody resp = do
      let bdy = Bilge.responseBody resp
          err = cs $ (show $ Bilge.responseStatus resp) <> "; " <> maybe "" cs bdy
      if Bilge.statusCode resp == 200 && isJust bdy
        then pure $ fromJust bdy
        else throwSpar $ SparNewIdPBadMetaUrl err

    decodeBody :: LBS -> Spar SAML.IdPMetadata
    decodeBody = either (throwSpar . SparNewIdPBadMeta . cs) pure . SAML.decode . cs


withDebugLog :: SAML.SP m => String -> (a -> Maybe String) -> m a -> m a
withDebugLog msg showval action = do
  SAML.logger SAML.Debug $ "entering " ++ msg
  val <- action
  let mshowedval = showval val
  SAML.logger SAML.Debug $ "leaving " ++ msg ++ mconcat [": " ++ fromJust mshowedval | isJust mshowedval]
  pure val

-- | Called by get, put, delete handlers.
authorizeIdP :: (HasCallStack, MonadError SparError m, SAML.SP m, Intra.MonadSparToBrig m)
             => Maybe UserId -> IdP -> m ()
authorizeIdP zusr idp = do
  teamid <- Intra.getZUsrOwnedTeam zusr
  when (teamid /= idp ^. SAML.idpExtraInfo) $ throwSpar SparNotInTeam


-- | Check that issuer is fresh (see longer comment in source) and request URI is https.
--
-- FUTUREWORK: move this to the saml2-web-sso package.  (same probably goes for get, create,
-- update, delete of idps.)
validateNewIdP :: forall m. (HasCallStack, m ~ Spar)
               => SAML.IdPMetadata -> TeamId -> m IdP
validateNewIdP _idpMetadata _idpExtraInfo = do
  _idpId <- SAML.IdPId <$> SAML.createUUID

  let requri = _idpMetadata ^. SAML.edRequestURI
  enforceHttps requri

  wrapMonadClient (Data.getIdPIdByIssuer (_idpMetadata ^. SAML.edIssuer)) >>= \case
    Nothing -> pure ()
    Just _ -> throwSpar SparNewIdPAlreadyInUse
    -- each idp (issuer) can only be created once.  if you want to update (one of) your team's
    -- idp(s), either use put (not implemented), or delete the old one before creating a new one
    -- (already works, even though it may create a brief time window in which users experience
    -- broken login behavior).
    --
    -- rationale: the issuer is how the idp self-identifies.  we can't allow the same idp to serve
    -- two teams because of implicit user creation: if an unknown user arrives, we use the
    -- idp-to-team mapping to decide which team to create the user in.  if we wanted to trust the
    -- idp to decide this for us, we would have to think of a way to prevent rogue idps from
    -- creating users in victim teams.

  pure SAML.IdPConfig {..}

enforceHttps :: URI.URI -> Spar ()
enforceHttps uri = do
  unless ((uri ^. URI.uriSchemeL . URI.schemeBSL) == "https") $ do
    throwSpar . SparNewIdPWantHttps . cs . SAML.renderURI $ uri


----------------------------------------------------------------------------
-- Internal API

internalStatus :: Spar NoContent
internalStatus = pure NoContent

-- | Cleanup handler that is called by Galley whenever a team is about to
-- get deleted.
internalDeleteTeam :: TeamId -> Spar NoContent
internalDeleteTeam team = do
  wrapMonadClient $ Data.deleteTeam team
  pure NoContent
