{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | The API types, handlers, and WAI 'Application' for whole Spar.
--
-- Note: handlers are defined here, but API types are reexported from "Spar.API.Types". The
-- SCIM branch of the API is fully defined in "Spar.Scim".
module Spar.API
  ( -- * Server
    app,
    api,

    -- * API types
    API,
    OutsideWorldAPI,

    -- ** Individual API pieces
    APIAuthReqPrecheck,
    APIAuthReq,
    APIAuthResp,
    IdpGet,
    IdpGetAll,
    IdpCreate,
    IdpDelete,
  )
where

import Control.Lens
import Control.Monad.Except
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Base64 as ES
import Data.ByteString.Builder (toLazyByteString)
import Data.Id
import Data.Proxy
import Data.String.Conversions
import Data.Time
import Imports
import OpenSSL.Random (randBytes)
import qualified SAML2.WebSSO as SAML
import Servant
import qualified Servant.Multipart as Multipart
import Servant.Swagger
import Spar.API.Swagger ()
import Spar.API.Types
import Spar.App
import qualified Spar.Data as Data
import Spar.Error
import qualified Spar.Intra.Brig as Brig
import qualified Spar.Intra.Galley as Galley
import Spar.Orphans ()
import Spar.Scim
import Spar.Scim.Swagger ()
import Spar.Types
import qualified URI.ByteString as URI
import qualified Web.Cookie as Cky

app :: Env -> Application
app ctx =
  SAML.setHttpCachePolicy $
    serve (Proxy @API) (hoistServer (Proxy @API) (SAML.nt @SparError @Spar ctx) (api $ sparCtxOpts ctx) :: Server API)

api :: Opts -> ServerT API Spar
api opts =
  apiSSO opts
    :<|> authreqPrecheck
    :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateBind
    :<|> apiIDP
    :<|> apiScim
    :<|> apiINTERNAL

apiSSO :: Opts -> ServerT APISSO Spar
apiSSO opts =
  pure (toSwagger (Proxy @OutsideWorldAPI))
    :<|> SAML.meta appName sparSPIssuer sparResponseURI
    :<|> authreqPrecheck
    :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateLogin
    :<|> authresp
    :<|> ssoSettings

apiIDP :: ServerT APIIDP Spar
apiIDP =
  idpGet
    :<|> idpGetRaw
    :<|> idpGetAll
    :<|> idpCreate
    :<|> idpUpdate
    :<|> idpDelete

apiINTERNAL :: ServerT APIINTERNAL Spar
apiINTERNAL =
  internalStatus
    :<|> internalDeleteTeam
    :<|> internalPutSsoSettings

appName :: ST
appName = "spar"

----------------------------------------------------------------------------
-- SSO API

authreqPrecheck :: Maybe URI.URI -> Maybe URI.URI -> SAML.IdPId -> Spar NoContent
authreqPrecheck msucc merr idpid =
  validateAuthreqParams msucc merr
    *> SAML.getIdPConfig idpid
    *> return NoContent

authreq ::
  NominalDiffTime ->
  DoInitiate ->
  Maybe UserId ->
  Maybe URI.URI ->
  Maybe URI.URI ->
  SAML.IdPId ->
  Spar (WithSetBindCookie (SAML.FormRedirect SAML.AuthnRequest))
authreq _ DoInitiateLogin (Just _) _ _ _ = throwSpar SparInitLoginWithAuth
authreq _ DoInitiateBind Nothing _ _ _ = throwSpar SparInitBindWithoutAuth
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
  DerivedOpts {derivedOptsBindCookiePath, derivedOptsBindCookieDomain} <-
    asks (derivedOpts . sparCtxOpts)
  msecret <-
    if isJust zusr
      then liftIO $ Just . cs . ES.encode <$> randBytes 32
      else pure Nothing
  let updSetCkyDom (SAML.SimpleSetCookie raw) =
        SAML.SimpleSetCookie
          raw
            { Cky.setCookieDomain = Just derivedOptsBindCookieDomain
            }
  cky <-
    updSetCkyDom
      <$> ( SAML.toggleCookie derivedOptsBindCookiePath $
              (,authreqttl) <$> msecret ::
              Spar SetBindCookie
          )
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
authresp ckyraw arbody = logErrors $ SAML.authresp sparSPIssuer sparResponseURI go arbody
  where
    cky :: Maybe BindCookie
    cky = ckyraw >>= bindCookieFromHeader
    go :: SAML.AuthnResponse -> SAML.AccessVerdict -> Spar Void
    go resp verdict = do
      result :: SAML.ResponseVerdict <- verdictHandler cky resp verdict
      throwError $ SAML.CustomServant result
    logErrors :: Spar Void -> Spar Void
    logErrors = flip catchError $ \case
      e@(SAML.CustomServant _) -> throwError e
      e -> do
        throwError . SAML.CustomServant $
          errorPage
            e
            (Multipart.inputs (SAML.authnResponseBodyRaw arbody))
            ckyraw

ssoSettings :: Spar SsoSettings
ssoSettings = do
  SsoSettings <$> wrapMonadClient Data.getDefaultSsoCode

----------------------------------------------------------------------------
-- IdP API

idpGet :: Maybe UserId -> SAML.IdPId -> Spar IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- SAML.getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  pure idp

idpGetRaw :: Maybe UserId -> SAML.IdPId -> Spar RawIdPMetadata
idpGetRaw zusr idpid = do
  idp <- SAML.getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  wrapMonadClient (Data.getIdPRawMetadata idpid) >>= \case
    Just txt -> pure $ RawIdPMetadata txt
    Nothing -> throwSpar SparNotFound

idpGetAll :: Maybe UserId -> Spar IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- Brig.getZUsrOwnedTeam zusr
  _idplProviders <- wrapMonadClientWithEnv $ Data.getIdPConfigsByTeam teamid
  pure IdPList {..}

-- | Delete empty IdPs, or if @"purge=true"@ in the HTTP query, delete all users
-- *synchronously* on brig and spar, and the IdP once it's empty.
--
-- The @"purge"@ query parameter is as a quick work-around until we have something better.  It
-- may very well time out, but it processes the users under the 'IdP' in chunks of 2000, so no
-- matter what the team size, it shouldn't choke any servers, just the client (which is
-- probably curl running locally on one of the spar instances).
-- https://github.com/zinfra/backend-issues/issues/1314
idpDelete :: Maybe UserId -> SAML.IdPId -> Maybe Bool -> Spar NoContent
idpDelete zusr idpid (fromMaybe False -> purge) = withDebugLog "idpDelete" (const Nothing) $ do
  idp <- SAML.getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
      team = idp ^. SAML.idpExtraInfo . wiTeam
  -- if idp is not empty: fail or purge
  idpIsEmpty <- wrapMonadClient $ isNothing <$> Data.getSAMLAnyUserByIssuer issuer
  let doPurge :: Spar ()
      doPurge = do
        some <- wrapMonadClient (Data.getSAMLSomeUsersByIssuer issuer)
        forM_ some $ \(uref, uid) -> do
          Brig.deleteBrigUser uid
          wrapMonadClient (Data.deleteSAMLUser uref)
        unless (null some) doPurge
  when (not idpIsEmpty) $ do
    if purge
      then doPurge
      else throwSpar SparIdPHasBoundUsers
  updateOldIssuers idp
  updateReplacingIdP idp
  wrapMonadClient $ do
    -- Delete tokens associated with given IdP (we rely on the fact that
    -- each IdP has exactly one team so we can look up all tokens
    -- associated with the team and then filter them)
    tokens <- Data.getScimTokens team
    for_ tokens $ \ScimTokenInfo {..} ->
      when (stiIdP == Just idpid) $ Data.deleteScimToken team stiId
    -- Delete IdP config
    Data.deleteIdPConfig idpid issuer team
    Data.deleteIdPRawMetadata idpid
  return NoContent
  where
    updateOldIssuers :: IdP -> Spar ()
    updateOldIssuers _ = pure ()
    -- we *could* update @idp ^. SAML.idpExtraInfo . wiReplacedBy@ to not keep the idp about
    -- to be deleted in its old issuers list, but it's tricky to avoid race conditions, and
    -- there is little to be gained here: we only use old issuers to find users that have not
    -- been migrated yet, and if an old user points to a deleted idp, it just means that we
    -- won't find any users to migrate.  still, doesn't hurt mucht to look either.  so we
    -- leave old issuers dangling for now.

    updateReplacingIdP :: IdP -> Spar ()
    updateReplacingIdP idp = forM_ (idp ^. SAML.idpExtraInfo . wiOldIssuers) $ \oldIssuer -> do
      wrapMonadClient $ do
        iid <- Data.getIdPIdByIssuer oldIssuer
        mapM_ (Data.clearReplacedBy . Data.Replaced) iid

-- | This handler only does the json parsing, and leaves all authorization checks and
-- application logic to 'idpCreateXML'.
idpCreate :: Maybe UserId -> IdPMetadataInfo -> Maybe SAML.IdPId -> Spar IdP
idpCreate zusr (IdPMetadataValue raw xml) midpid = idpCreateXML zusr raw xml midpid

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
idpCreateXML :: Maybe UserId -> Text -> SAML.IdPMetadata -> Maybe SAML.IdPId -> Spar IdP
idpCreateXML zusr raw idpmeta mReplaces = withDebugLog "idpCreate" (Just . show . (^. SAML.idpId)) $ do
  teamid <- Brig.getZUsrOwnedTeam zusr
  Galley.assertSSOEnabled teamid
  idp <- validateNewIdP idpmeta teamid mReplaces
  wrapMonadClient $ Data.storeIdPRawMetadata (idp ^. SAML.idpId) raw
  SAML.storeIdPConfig idp
  forM_ mReplaces $ \replaces -> wrapMonadClient $ do
    Data.setReplacedBy (Data.Replaced replaces) (Data.Replacing (idp ^. SAML.idpId))
  pure idp

-- | Check that issuer is not used for any team in the system (it is a database keys for
-- finding IdPs), and request URI is https.
--
-- About the @mReplaces@ argument: the information whether the idp is replacing an old one is
-- in query parameter, because the body can be both XML and JSON.  The JSON body could carry
-- the replaced idp id fine, but the XML is defined in the SAML standard and cannot be
-- changed.
--
-- FUTUREWORK: find out if anybody uses the XML body type and drop it if not.
--
-- FUTUREWORK: using the same issuer for two teams may be possible, but only if we stop
-- supporting implicit user creating via SAML.  If unknown users present IdP credentials, the
-- issuer is our only way of finding the team in which the user must be created.
--
-- FUTUREWORK: move this to the saml2-web-sso package.  (same probably goes for get, create,
-- update, delete of idps.)
validateNewIdP ::
  forall m.
  (HasCallStack, m ~ Spar) =>
  SAML.IdPMetadata ->
  TeamId ->
  Maybe SAML.IdPId ->
  m IdP
validateNewIdP _idpMetadata teamId mReplaces = do
  _idpId <- SAML.IdPId <$> SAML.createUUID
  oldIssuers :: [SAML.Issuer] <- case mReplaces of
    Nothing -> pure []
    Just replaces -> do
      idp <- wrapMonadClient (Data.getIdPConfig replaces) >>= maybe (throwSpar SparNotFound) pure
      pure $ (idp ^. SAML.idpMetadata . SAML.edIssuer) : (idp ^. SAML.idpExtraInfo . wiOldIssuers)
  let requri = _idpMetadata ^. SAML.edRequestURI
      _idpExtraInfo = WireIdP teamId oldIssuers Nothing
  enforceHttps requri
  wrapMonadClient (Data.getIdPIdByIssuer (_idpMetadata ^. SAML.edIssuer)) >>= \case
    Nothing -> pure ()
    Just _ -> throwSpar SparNewIdPAlreadyInUse
  pure SAML.IdPConfig {..}

-- | FUTUREWORK: 'idpUpdateXML' is only factored out of this function for symmetry with
-- 'idpCreate', which is not a good reason.  make this one function and pass around
-- 'IdPMetadataInfo' directly where convenient.
idpUpdate :: Maybe UserId -> IdPMetadataInfo -> SAML.IdPId -> Spar IdP
idpUpdate zusr (IdPMetadataValue raw xml) idpid = idpUpdateXML zusr raw xml idpid

idpUpdateXML :: Maybe UserId -> Text -> SAML.IdPMetadata -> SAML.IdPId -> Spar IdP
idpUpdateXML zusr raw idpmeta idpid = withDebugLog "idpUpdate" (Just . show . (^. SAML.idpId)) $ do
  (teamid, idp) <- validateIdPUpdate zusr idpmeta idpid
  Galley.assertSSOEnabled teamid
  wrapMonadClient $ Data.storeIdPRawMetadata (idp ^. SAML.idpId) raw
  -- (if raw metadata is stored and then spar goes out, raw metadata won't match the
  -- structured idp config.  since this will lead to a 5xx response, the client is epected to
  -- try again, which would clean up cassandra state.)
  SAML.storeIdPConfig idp
  pure idp

-- | Check that: idp id is valid; calling user is admin in that idp's home team; team id in
-- new metainfo doesn't change; new issuer (if changed) is not in use anywhere else; request
-- uri is https.  Keep track of old issuer in extra info if issuer has changed.
validateIdPUpdate ::
  forall m.
  (HasCallStack, m ~ Spar) =>
  Maybe UserId ->
  SAML.IdPMetadata ->
  SAML.IdPId ->
  m (TeamId, IdP)
validateIdPUpdate zusr _idpMetadata _idpId = do
  previousIdP <-
    wrapMonadClient (Data.getIdPConfig _idpId) >>= \case
      Nothing -> throwError errUnknownIdPId
      Just idp -> pure idp
  teamId <- authorizeIdP zusr previousIdP
  unless (previousIdP ^. SAML.idpExtraInfo . wiTeam == teamId) $ do
    throwError errUnknownIdP
  _idpExtraInfo <- do
    let previousIssuer = previousIdP ^. SAML.idpMetadata . SAML.edIssuer
        newIssuer = _idpMetadata ^. SAML.edIssuer
    if previousIssuer == newIssuer
      then pure $ previousIdP ^. SAML.idpExtraInfo
      else do
        notInUse <- isNothing <$> wrapMonadClient (Data.getIdPConfigByIssuer newIssuer)
        if notInUse
          then pure $ (previousIdP ^. SAML.idpExtraInfo) & wiOldIssuers %~ (previousIssuer :)
          else throwSpar SparIdPIssuerInUse
  let requri = _idpMetadata ^. SAML.edRequestURI
  enforceHttps requri
  pure (teamId, SAML.IdPConfig {..})
  where
    errUnknownIdP = SAML.UnknownIdP $ enc uri
      where
        enc = cs . toLazyByteString . URI.serializeURIRef
        uri = _idpMetadata ^. SAML.edIssuer . SAML.fromIssuer
    errUnknownIdPId = SAML.UnknownIdP . cs . SAML.idPIdToST $ _idpId

withDebugLog :: SAML.SP m => String -> (a -> Maybe String) -> m a -> m a
withDebugLog msg showval action = do
  SAML.logger SAML.Debug $ "entering " ++ msg
  val <- action
  let mshowedval = showval val
  SAML.logger SAML.Debug $ "leaving " ++ msg ++ mconcat [": " ++ fromJust mshowedval | isJust mshowedval]
  pure val

authorizeIdP ::
  (HasCallStack, MonadError SparError m, SAML.SP m, Galley.MonadSparToGalley m, Brig.MonadSparToBrig m) =>
  Maybe UserId ->
  IdP ->
  m TeamId
authorizeIdP zusr idp = do
  teamid <- Brig.getZUsrOwnedTeam zusr
  when (teamid /= idp ^. SAML.idpExtraInfo . wiTeam) $ throwSpar SparNotInTeam
  pure teamid

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

internalPutSsoSettings :: SsoSettings -> Spar NoContent
internalPutSsoSettings SsoSettings {defaultSsoCode = Nothing} = do
  wrapMonadClient $ Data.deleteDefaultSsoCode
  pure NoContent
internalPutSsoSettings SsoSettings {defaultSsoCode = Just code} = do
  wrapMonadClient (Data.getIdPConfig code) >>= \case
    Nothing ->
      -- this will return a 404, which is not quite right,
      -- but it's an internal endpoint and the message clearly says
      -- "Could not find IdP".
      throwSpar SparNotFound
    Just _ -> do
      wrapMonadClient $ Data.storeDefaultSsoCode code
      pure NoContent
