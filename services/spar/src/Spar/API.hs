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
import Galley.Types.Teams (HiddenPerm (CreateUpdateDeleteIdp, ReadIdp))
import Imports
import OpenSSL.Random (randBytes)
import Polysemy
import qualified SAML2.WebSSO as SAML
import Servant
import qualified Servant.Multipart as Multipart
import Spar.App
import qualified Spar.Data as Data
import Spar.Error
import qualified Spar.Intra.Brig as Brig
import qualified Spar.Intra.Galley as Galley
import Spar.Orphans ()
import Spar.Scim
import qualified Spar.Sem.IdP as IdPEffect
import Spar.Sem.SAMLUser (SAMLUser)
import qualified URI.ByteString as URI
import Wire.API.Cookie
import Wire.API.Routes.Public.Spar
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import qualified Spar.Sem.DefaultSsoCode as DefaultSsoCode
import Spar.Sem.DefaultSsoCode (DefaultSsoCode)
import qualified Spar.Sem.ScimTokenStore as ScimTokenStore
import Spar.Sem.ScimTokenStore (ScimTokenStore)

app :: Env -> Application
app ctx =
  SAML.setHttpCachePolicy $
    serve (Proxy @API) (hoistServer (Proxy @API) (SAML.nt @SparError @(Spar _) ctx) (api $ sparCtxOpts ctx) :: Server API)

api :: Member ScimTokenStore r => Member DefaultSsoCode r => Member IdPEffect.IdP r => Member SAMLUser r => Opts -> ServerT API (Spar r)
api opts =
  apiSSO opts
    :<|> authreqPrecheck
    :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateBind
    :<|> apiIDP
    :<|> apiScim
    :<|> apiINTERNAL

apiSSO :: Member ScimTokenStore r => Member DefaultSsoCode r => Member IdPEffect.IdP r => Member SAMLUser r => Opts -> ServerT APISSO (Spar r)
apiSSO opts =
  SAML.meta appName (sparSPIssuer Nothing) (sparResponseURI Nothing)
    :<|> (\tid -> SAML.meta appName (sparSPIssuer (Just tid)) (sparResponseURI (Just tid)))
    :<|> authreqPrecheck
    :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateLogin
    :<|> authresp Nothing
    :<|> authresp . Just
    :<|> ssoSettings

apiIDP :: Member ScimTokenStore r => Member IdPEffect.IdP r => ServerT APIIDP (Spar r)
apiIDP =
  idpGet
    :<|> idpGetRaw
    :<|> idpGetAll
    :<|> idpCreate
    :<|> idpUpdate
    :<|> idpDelete

apiINTERNAL :: Member DefaultSsoCode r => Member IdPEffect.IdP r => Member SAMLUser r => ServerT APIINTERNAL (Spar r)
apiINTERNAL =
  internalStatus
    :<|> internalDeleteTeam
    :<|> internalPutSsoSettings

appName :: ST
appName = "spar"

----------------------------------------------------------------------------
-- SSO API

authreqPrecheck :: Member IdPEffect.IdP r => Maybe URI.URI -> Maybe URI.URI -> SAML.IdPId -> Spar r NoContent
authreqPrecheck msucc merr idpid =
  validateAuthreqParams msucc merr
    *> SAML.getIdPConfig idpid
    *> return NoContent

authreq ::
  Member IdPEffect.IdP r =>
  NominalDiffTime ->
  DoInitiate ->
  Maybe UserId ->
  Maybe URI.URI ->
  Maybe URI.URI ->
  SAML.IdPId ->
  Spar r (WithSetBindCookie (SAML.FormRedirect SAML.AuthnRequest))
authreq _ DoInitiateLogin (Just _) _ _ _ = throwSpar SparInitLoginWithAuth
authreq _ DoInitiateBind Nothing _ _ _ = throwSpar SparInitBindWithoutAuth
authreq authreqttl _ zusr msucc merr idpid = do
  vformat <- validateAuthreqParams msucc merr
  form@(SAML.FormRedirect _ ((^. SAML.rqID) -> reqid)) <- do
    idp :: IdP <- wrapMonadClient (Data.getIdPConfig idpid) >>= maybe (throwSpar (SparIdPNotFound (cs $ show idpid))) pure
    let mbtid :: Maybe TeamId
        mbtid = case fromMaybe defWireIdPAPIVersion (idp ^. SAML.idpExtraInfo . wiApiVersion) of
          WireIdPAPIV1 -> Nothing
          WireIdPAPIV2 -> Just $ idp ^. SAML.idpExtraInfo . wiTeam
    SAML.authreq authreqttl (sparSPIssuer mbtid) idpid
  wrapMonadClient $ Data.storeVerdictFormat authreqttl reqid vformat
  cky <- initializeBindCookie zusr authreqttl
  SAML.logger SAML.Debug $ "setting bind cookie: " <> show cky
  pure $ addHeader cky form

-- | If the user is already authenticated, create bind cookie with a given life expectancy and our
-- domain, and store it in C*.  If the user is not authenticated, return a deletion 'SetCookie'
-- value that deletes any bind cookies on the client.
initializeBindCookie :: Maybe UserId -> NominalDiffTime -> Spar r SetBindCookie
initializeBindCookie zusr authreqttl = do
  DerivedOpts {derivedOptsBindCookiePath} <- asks (derivedOpts . sparCtxOpts)
  msecret <-
    if isJust zusr
      then liftIO $ Just . cs . ES.encode <$> randBytes 32
      else pure Nothing
  cky <- fmap SetBindCookie . SAML.toggleCookie derivedOptsBindCookiePath $ (,authreqttl) <$> msecret
  forM_ zusr $ \userid -> wrapMonadClientWithEnv $ Data.insertBindCookie cky userid authreqttl
  pure cky

redirectURLMaxLength :: Int
redirectURLMaxLength = 140

validateAuthreqParams :: Maybe URI.URI -> Maybe URI.URI -> Spar r VerdictFormat
validateAuthreqParams msucc merr = case (msucc, merr) of
  (Nothing, Nothing) -> pure VerdictFormatWeb
  (Just ok, Just err) -> do
    validateRedirectURL `mapM_` [ok, err]
    pure $ VerdictFormatMobile ok err
  _ -> throwSpar $ SparBadInitiateLoginQueryParams "need-both-redirect-urls"

validateRedirectURL :: URI.URI -> Spar r ()
validateRedirectURL uri = do
  unless ((SBS.take 4 . URI.schemeBS . URI.uriScheme $ uri) == "wire") $ do
    throwSpar $ SparBadInitiateLoginQueryParams "invalid-schema"
  unless ((SBS.length $ URI.serializeURIRef' uri) <= redirectURLMaxLength) $ do
    throwSpar $ SparBadInitiateLoginQueryParams "url-too-long"

authresp :: forall r. Member ScimTokenStore r => Member IdPEffect.IdP r => Member SAMLUser r => Maybe TeamId -> Maybe ST -> SAML.AuthnResponseBody -> Spar r Void
authresp mbtid ckyraw arbody = logErrors $ SAML.authresp mbtid (sparSPIssuer mbtid) (sparResponseURI mbtid) go arbody
  where
    cky :: Maybe BindCookie
    cky = ckyraw >>= bindCookieFromHeader

    go :: SAML.AuthnResponse -> SAML.AccessVerdict -> Spar r Void
    go resp verdict = do
      result :: SAML.ResponseVerdict <- verdictHandler cky mbtid resp verdict
      throwError $ SAML.CustomServant result

    logErrors :: Spar r Void -> Spar r Void
    logErrors = flip catchError $ \case
      e@(SAML.CustomServant _) -> throwError e
      e -> do
        throwError . SAML.CustomServant $
          errorPage
            e
            (Multipart.inputs (SAML.authnResponseBodyRaw arbody))
            ckyraw

ssoSettings :: Member DefaultSsoCode r => Spar r SsoSettings
ssoSettings = do
  SsoSettings <$> wrapMonadClientSem DefaultSsoCode.get

----------------------------------------------------------------------------
-- IdP API

idpGet :: Member IdPEffect.IdP r => Maybe UserId -> SAML.IdPId -> Spar r IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- SAML.getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  pure idp

idpGetRaw :: Member IdPEffect.IdP r => Maybe UserId -> SAML.IdPId -> Spar r RawIdPMetadata
idpGetRaw zusr idpid = do
  idp <- SAML.getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  wrapMonadClient (Data.getIdPRawMetadata idpid) >>= \case
    Just txt -> pure $ RawIdPMetadata txt
    Nothing -> throwSpar $ SparIdPNotFound (cs $ show idpid)

idpGetAll :: Maybe UserId -> Spar r IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- Brig.getZUsrCheckPerm zusr ReadIdp
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
idpDelete :: forall r. Member ScimTokenStore r => Member IdPEffect.IdP r => Maybe UserId -> SAML.IdPId -> Maybe Bool -> Spar r NoContent
idpDelete zusr idpid (fromMaybe False -> purge) = withDebugLog "idpDelete" (const Nothing) $ do
  idp <- SAML.getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
      team = idp ^. SAML.idpExtraInfo . wiTeam
  -- if idp is not empty: fail or purge
  idpIsEmpty <- wrapMonadClient $ isNothing <$> Data.getSAMLAnyUserByIssuer issuer
  let doPurge :: Spar r ()
      doPurge = do
        some <- wrapMonadClient (Data.getSAMLSomeUsersByIssuer issuer)
        forM_ some $ \(uref, uid) -> do
          Brig.deleteBrigUser uid
          wrapMonadClient (Data.deleteSAMLUser uid uref)
        unless (null some) doPurge
  when (not idpIsEmpty) $ do
    if purge
      then doPurge
      else throwSpar SparIdPHasBoundUsers
  updateOldIssuers idp
  updateReplacingIdP idp
  wrapSpar $ do
    -- Delete tokens associated with given IdP (we rely on the fact that
    -- each IdP has exactly one team so we can look up all tokens
    -- associated with the team and then filter them)
    tokens <- liftSem $ ScimTokenStore.getByTeam team
    for_ tokens $ \ScimTokenInfo {..} ->
      when (stiIdP == Just idpid) $ liftSem $ ScimTokenStore.delete team stiId
    -- Delete IdP config
    liftMonadClient $ do
      Data.deleteIdPConfig idpid issuer team
      Data.deleteIdPRawMetadata idpid
  return NoContent
  where
    updateOldIssuers :: IdP -> Spar r ()
    updateOldIssuers _ = pure ()
    -- we *could* update @idp ^. SAML.idpExtraInfo . wiReplacedBy@ to not keep the idp about
    -- to be deleted in its old issuers list, but it's tricky to avoid race conditions, and
    -- there is little to be gained here: we only use old issuers to find users that have not
    -- been migrated yet, and if an old user points to a deleted idp, it just means that we
    -- won't find any users to migrate.  still, doesn't hurt mucht to look either.  so we
    -- leave old issuers dangling for now.

    updateReplacingIdP :: IdP -> Spar r ()
    updateReplacingIdP idp = forM_ (idp ^. SAML.idpExtraInfo . wiOldIssuers) $ \oldIssuer -> do
      wrapSpar $ do
        getIdPIdByIssuer oldIssuer (idp ^. SAML.idpExtraInfo . wiTeam) >>= \case
          Data.GetIdPFound iid -> liftMonadClient $ Data.clearReplacedBy $ Data.Replaced iid
          Data.GetIdPNotFound -> pure ()
          Data.GetIdPDanglingId _ -> pure ()
          Data.GetIdPNonUnique _ -> pure ()
          Data.GetIdPWrongTeam _ -> pure ()

-- | This handler only does the json parsing, and leaves all authorization checks and
-- application logic to 'idpCreateXML'.
idpCreate :: Member ScimTokenStore r => Member IdPEffect.IdP r => Maybe UserId -> IdPMetadataInfo -> Maybe SAML.IdPId -> Maybe WireIdPAPIVersion -> Spar r IdP
idpCreate zusr (IdPMetadataValue raw xml) midpid apiversion = idpCreateXML zusr raw xml midpid apiversion

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
idpCreateXML :: Member ScimTokenStore r => Member IdPEffect.IdP r => Maybe UserId -> Text -> SAML.IdPMetadata -> Maybe SAML.IdPId -> Maybe WireIdPAPIVersion -> Spar r IdP
idpCreateXML zusr raw idpmeta mReplaces (fromMaybe defWireIdPAPIVersion -> apiversion) = withDebugLog "idpCreate" (Just . show . (^. SAML.idpId)) $ do
  teamid <- Brig.getZUsrCheckPerm zusr CreateUpdateDeleteIdp
  Galley.assertSSOEnabled teamid
  assertNoScimOrNoIdP teamid
  idp <- validateNewIdP apiversion idpmeta teamid mReplaces
  wrapMonadClient $ Data.storeIdPRawMetadata (idp ^. SAML.idpId) raw
  SAML.storeIdPConfig idp
  forM_ mReplaces $ \replaces -> wrapMonadClient $ do
    Data.setReplacedBy (Data.Replaced replaces) (Data.Replacing (idp ^. SAML.idpId))
  pure idp

-- | In teams with a scim access token, only one IdP is allowed.  The reason is that scim user
-- data contains no information about the idp issuer, only the user name, so no valid saml
-- credentials can be created.  To fix this, we need to implement a way to associate scim
-- tokens with IdPs.  https://wearezeta.atlassian.net/browse/SQSERVICES-165
assertNoScimOrNoIdP :: Member ScimTokenStore r => TeamId -> Spar r ()
assertNoScimOrNoIdP teamid = do
  numTokens <- length <$> wrapMonadClientSem (ScimTokenStore.getByTeam teamid)
  numIdps <- length <$> wrapMonadClientWithEnv (Data.getIdPConfigsByTeam teamid)
  when (numTokens > 0 && numIdps > 0) $ do
    throwSpar $
      SparProvisioningMoreThanOneIdP
        "Teams with SCIM tokens can only have at most one IdP"

-- | Check that issuer is not used anywhere in the system ('WireIdPAPIV1', here it is a
-- database keys for finding IdPs), or anywhere in this team ('WireIdPAPIV2'), that request
-- URI is https, that the replacement IdPId, if present, points to our team, and possibly
-- other things (see source code for the definitive answer).
--
-- About the @mReplaces@ argument: the information whether the idp is replacing an old one is
-- in query parameter, because the body can be both XML and JSON.  The JSON body could carry
-- the replaced idp id fine, but the XML is defined in the SAML standard and cannot be
-- changed.  NB: if you want to replace an IdP by one with the same issuer, you probably
-- want to use `PUT` instead of `POST`.
--
-- FUTUREWORK: find out if anybody uses the XML body type and drop it if not.
--
-- FUTUREWORK: using the same issuer for two teams even in `WireIdPAPIV1` may be possible, but
-- only if we stop supporting implicit user creating via SAML.  If unknown users present IdP
-- credentials, the issuer is our only way of finding the team in which the user must be
-- created.
--
-- FUTUREWORK: move this to the saml2-web-sso package.  (same probably goes for get, create,
-- update, delete of idps.)
validateNewIdP ::
  forall m r.
  (HasCallStack, m ~ Spar r) =>
  Member IdPEffect.IdP r =>
  WireIdPAPIVersion ->
  SAML.IdPMetadata ->
  TeamId ->
  Maybe SAML.IdPId ->
  m IdP
validateNewIdP apiversion _idpMetadata teamId mReplaces = withDebugLog "validateNewIdP" (Just . show . (^. SAML.idpId)) $ do
  _idpId <- SAML.IdPId <$> SAML.createUUID
  oldIssuers :: [SAML.Issuer] <- case mReplaces of
    Nothing -> pure []
    Just replaces -> do
      idp <- wrapMonadClient (Data.getIdPConfig replaces) >>= maybe (throwSpar (SparIdPNotFound (cs $ show mReplaces))) pure
      pure $ (idp ^. SAML.idpMetadata . SAML.edIssuer) : (idp ^. SAML.idpExtraInfo . wiOldIssuers)
  let requri = _idpMetadata ^. SAML.edRequestURI
      _idpExtraInfo = WireIdP teamId (Just apiversion) oldIssuers Nothing
  enforceHttps requri
  idp <- wrapSpar $ getIdPConfigByIssuer (_idpMetadata ^. SAML.edIssuer) teamId
  SAML.logger SAML.Debug $ show (apiversion, _idpMetadata, teamId, mReplaces)
  SAML.logger SAML.Debug $ show (_idpId, oldIssuers, idp)

  let handleIdPClash :: Either id idp -> m ()
      -- (HINT: using type vars above instead of the actual types constitutes a proof that
      -- we're not using any properties of the arguments in this function.)
      handleIdPClash = case apiversion of
        WireIdPAPIV1 -> const $ do
          throwSpar $ SparNewIdPAlreadyInUse "you can't create an IdP with api_version v1 if the issuer is already in use on the wire instance."
        WireIdPAPIV2 -> \case
          (Right _) -> do
            -- idp' was found by lookup with teamid, so it's in the same team.
            throwSpar $ SparNewIdPAlreadyInUse "if the exisitng IdP is registered for a team, the new one can't have it."
          (Left _) -> do
            -- this idp *id* is from a different team, and we're in the 'WireIdPAPIV2' case, so this is fine.
            pure ()

  case idp of
    Data.GetIdPFound idp' {- same team -} -> handleIdPClash (Right idp')
    Data.GetIdPNotFound -> pure ()
    res@(Data.GetIdPDanglingId _) -> throwSpar . SparIdPNotFound . ("validateNewIdP: " <>) . cs . show $ res -- database inconsistency
    Data.GetIdPNonUnique ids' {- same team didn't yield anything, but there are at least two other teams with this issuer already -} -> handleIdPClash (Left ids')
    Data.GetIdPWrongTeam id' {- different team -} -> handleIdPClash (Left id')

  pure SAML.IdPConfig {..}

-- | FUTUREWORK: 'idpUpdateXML' is only factored out of this function for symmetry with
-- 'idpCreate', which is not a good reason.  make this one function and pass around
-- 'IdPMetadataInfo' directly where convenient.
idpUpdate :: Member IdPEffect.IdP r => Maybe UserId -> IdPMetadataInfo -> SAML.IdPId -> Spar r IdP
idpUpdate zusr (IdPMetadataValue raw xml) idpid = idpUpdateXML zusr raw xml idpid

idpUpdateXML :: Member IdPEffect.IdP r => Maybe UserId -> Text -> SAML.IdPMetadata -> SAML.IdPId -> Spar r IdP
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
-- new metainfo doesn't change; new issuer (if changed) is not in use anywhere else (except as
-- an earlier IdP under the same ID); request uri is https.  Keep track of old issuer in extra
-- info if issuer has changed.
validateIdPUpdate ::
  forall m r.
  (HasCallStack, m ~ Spar r) =>
  Member IdPEffect.IdP r =>
  Maybe UserId ->
  SAML.IdPMetadata ->
  SAML.IdPId ->
  m (TeamId, IdP)
validateIdPUpdate zusr _idpMetadata _idpId = withDebugLog "validateNewIdP" (Just . show . (_2 %~ (^. SAML.idpId))) $ do
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
        foundConfig <- wrapSpar $ getIdPConfigByIssuerAllowOld newIssuer (Just teamId)
        notInUseByOthers <- case foundConfig of
          Data.GetIdPFound c -> pure $ c ^. SAML.idpId == _idpId
          Data.GetIdPNotFound -> pure True
          res@(Data.GetIdPDanglingId _) -> throwSpar . SparIdPNotFound . ("validateIdPUpdate: " <>) . cs . show $ res -- impossible
          res@(Data.GetIdPNonUnique _) -> throwSpar . SparIdPNotFound . ("validateIdPUpdate: " <>) . cs . show $ res -- impossible (because team id was used in lookup)
          Data.GetIdPWrongTeam _ -> pure False
        if notInUseByOthers
          then pure $ (previousIdP ^. SAML.idpExtraInfo) & wiOldIssuers %~ nub . (previousIssuer :)
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
authorizeIdP Nothing _ = throwSpar (SparNoPermission (cs $ show CreateUpdateDeleteIdp))
authorizeIdP (Just zusr) idp = do
  let teamid = idp ^. SAML.idpExtraInfo . wiTeam
  Galley.assertHasPermission teamid CreateUpdateDeleteIdp zusr
  pure teamid

enforceHttps :: URI.URI -> Spar r ()
enforceHttps uri = do
  unless ((uri ^. URI.uriSchemeL . URI.schemeBSL) == "https") $ do
    throwSpar . SparNewIdPWantHttps . cs . SAML.renderURI $ uri

----------------------------------------------------------------------------
-- Internal API

internalStatus :: Spar r NoContent
internalStatus = pure NoContent

-- | Cleanup handler that is called by Galley whenever a team is about to
-- get deleted.
internalDeleteTeam :: Member IdPEffect.IdP r => Member SAMLUser r => TeamId -> Spar r NoContent
internalDeleteTeam team = do
  wrapSpar $ deleteTeam team
  pure NoContent

internalPutSsoSettings :: Member DefaultSsoCode r => SsoSettings -> Spar r NoContent
internalPutSsoSettings SsoSettings {defaultSsoCode = Nothing} = do
  wrapMonadClientSem $ DefaultSsoCode.delete
  pure NoContent
internalPutSsoSettings SsoSettings {defaultSsoCode = Just code} = do
  wrapMonadClient (Data.getIdPConfig code) >>= \case
    Nothing ->
      -- this will return a 404, which is not quite right,
      -- but it's an internal endpoint and the message clearly says
      -- "Could not find IdP".
      throwSpar $ SparIdPNotFound mempty
    Just _ -> do
      wrapMonadClientSem $ DefaultSsoCode.store code
      pure NoContent
