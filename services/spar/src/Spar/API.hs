{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified SAML2.WebSSO as SAML
import Servant
import qualified Servant.Multipart as Multipart
import Spar.App
import Spar.CanonicalInterpreter
import Spar.Error
import qualified Spar.Intra.BrigApp as Brig
import Spar.Orphans ()
import Spar.Scim
import Spar.Sem.AReqIDStore (AReqIDStore)
import Spar.Sem.AssIDStore (AssIDStore)
import Spar.Sem.BindCookieStore (BindCookieStore)
import qualified Spar.Sem.BindCookieStore as BindCookieStore
import Spar.Sem.BrigAccess (BrigAccess)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.DefaultSsoCode (DefaultSsoCode)
import qualified Spar.Sem.DefaultSsoCode as DefaultSsoCode
import Spar.Sem.GalleyAccess (GalleyAccess)
import qualified Spar.Sem.GalleyAccess as GalleyAccess
import Spar.Sem.IdPConfigStore (GetIdPResult (..), IdPConfigStore, Replaced (..), Replacing (..))
import qualified Spar.Sem.IdPConfigStore as IdPConfigStore
import Spar.Sem.IdPRawMetadataStore (IdPRawMetadataStore)
import qualified Spar.Sem.IdPRawMetadataStore as IdPRawMetadataStore
import Spar.Sem.Reporter (Reporter)
import Spar.Sem.SAML2 (SAML2)
import qualified Spar.Sem.SAML2 as SAML2
import Spar.Sem.SAMLUserStore (SAMLUserStore)
import qualified Spar.Sem.SAMLUserStore as SAMLUserStore
import Spar.Sem.SamlProtocolSettings (SamlProtocolSettings)
import qualified Spar.Sem.SamlProtocolSettings as SamlProtocolSettings
import Spar.Sem.ScimExternalIdStore (ScimExternalIdStore)
import Spar.Sem.ScimTokenStore (ScimTokenStore)
import qualified Spar.Sem.ScimTokenStore as ScimTokenStore
import Spar.Sem.ScimUserTimesStore (ScimUserTimesStore)
import Spar.Sem.VerdictFormatStore (VerdictFormatStore)
import qualified Spar.Sem.VerdictFormatStore as VerdictFormatStore
import System.Logger (Msg)
import qualified URI.ByteString as URI
import Wire.API.Cookie
import Wire.API.Routes.Public.Spar
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.Sem.Logger (Logger)
import qualified Wire.Sem.Logger as Logger
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import qualified Wire.Sem.Random as Random

app :: Env -> Application
app ctx =
  SAML.setHttpCachePolicy $
    serve (Proxy @API) (hoistServer (Proxy @API) (runSparToHandler ctx) (api $ sparCtxOpts ctx) :: Server API)

api ::
  Members
    '[ GalleyAccess,
       BrigAccess,
       Input Opts,
       BindCookieStore,
       AssIDStore,
       AReqIDStore,
       VerdictFormatStore,
       ScimExternalIdStore,
       ScimUserTimesStore,
       ScimTokenStore,
       DefaultSsoCode,
       IdPConfigStore,
       IdPRawMetadataStore,
       SAMLUserStore,
       Random,
       Error SparError,
       SAML2,
       Now,
       SamlProtocolSettings,
       Logger String,
       Reporter,
       -- TODO(sandy): Only necessary for 'fromExceptionSem' in 'apiScim'
       Final IO,
       Logger (Msg -> Msg)
     ]
    r =>
  Opts ->
  ServerT API (Sem r)
api opts =
  apiSSO opts
    :<|> authreqPrecheck
    :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateBind
    :<|> apiIDP
    :<|> apiScim
    :<|> apiINTERNAL

apiSSO ::
  Members
    '[ GalleyAccess,
       Logger String,
       Input Opts,
       BrigAccess,
       BindCookieStore,
       AssIDStore,
       VerdictFormatStore,
       AReqIDStore,
       ScimTokenStore,
       DefaultSsoCode,
       IdPConfigStore,
       Random,
       Error SparError,
       SAML2,
       SamlProtocolSettings,
       Reporter,
       SAMLUserStore
     ]
    r =>
  Opts ->
  ServerT APISSO (Sem r)
apiSSO opts =
  (SAML2.meta appName (SamlProtocolSettings.spIssuer Nothing) (SamlProtocolSettings.responseURI Nothing))
    :<|> (\tid -> SAML2.meta appName (SamlProtocolSettings.spIssuer (Just tid)) (SamlProtocolSettings.responseURI (Just tid)))
    :<|> authreqPrecheck
    :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateLogin
    :<|> authresp Nothing
    :<|> authresp . Just
    :<|> ssoSettings

apiIDP ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       IdPConfigStore,
       IdPRawMetadataStore,
       SAMLUserStore,
       Error SparError
     ]
    r =>
  ServerT APIIDP (Sem r)
apiIDP =
  idpGet
    :<|> idpGetRaw
    :<|> idpGetAll
    :<|> idpCreate
    :<|> idpUpdate
    :<|> idpDelete

apiINTERNAL ::
  Members
    '[ ScimTokenStore,
       DefaultSsoCode,
       IdPConfigStore,
       Error SparError,
       SAMLUserStore
     ]
    r =>
  ServerT APIINTERNAL (Sem r)
apiINTERNAL =
  internalStatus
    :<|> internalDeleteTeam
    :<|> internalPutSsoSettings

appName :: ST
appName = "spar"

----------------------------------------------------------------------------
-- SSO API

authreqPrecheck ::
  Members
    '[ IdPConfigStore,
       Error SparError
     ]
    r =>
  Maybe URI.URI ->
  Maybe URI.URI ->
  SAML.IdPId ->
  Sem r NoContent
authreqPrecheck msucc merr idpid =
  validateAuthreqParams msucc merr
    *> getIdPConfig idpid
    *> return NoContent

authreq ::
  Members
    '[ Random,
       Input Opts,
       Logger String,
       BindCookieStore,
       AssIDStore,
       VerdictFormatStore,
       AReqIDStore,
       SAML2,
       SamlProtocolSettings,
       Error SparError,
       IdPConfigStore
     ]
    r =>
  NominalDiffTime ->
  DoInitiate ->
  Maybe UserId ->
  Maybe URI.URI ->
  Maybe URI.URI ->
  SAML.IdPId ->
  Sem r (WithSetBindCookie (SAML.FormRedirect SAML.AuthnRequest))
authreq _ DoInitiateLogin (Just _) _ _ _ = throwSparSem SparInitLoginWithAuth
authreq _ DoInitiateBind Nothing _ _ _ = throwSparSem SparInitBindWithoutAuth
authreq authreqttl _ zusr msucc merr idpid = do
  vformat <- validateAuthreqParams msucc merr
  form@(SAML.FormRedirect _ ((^. SAML.rqID) -> reqid)) <- do
    idp :: IdP <- IdPConfigStore.getConfig idpid >>= maybe (throwSparSem (SparIdPNotFound (cs $ show idpid))) pure
    let mbtid :: Maybe TeamId
        mbtid = case fromMaybe defWireIdPAPIVersion (idp ^. SAML.idpExtraInfo . wiApiVersion) of
          WireIdPAPIV1 -> Nothing
          WireIdPAPIV2 -> Just $ idp ^. SAML.idpExtraInfo . wiTeam
    SAML2.authReq authreqttl (SamlProtocolSettings.spIssuer mbtid) idpid
  VerdictFormatStore.store authreqttl reqid vformat
  cky <- initializeBindCookie zusr authreqttl
  Logger.log Logger.Debug $ "setting bind cookie: " <> show cky
  pure $ addHeader cky form

-- | If the user is already authenticated, create bind cookie with a given life expectancy and our
-- domain, and store it in C*.  If the user is not authenticated, return a deletion 'SetCookie'
-- value that deletes any bind cookies on the client.
initializeBindCookie ::
  Members
    '[ Random,
       SAML2,
       Input Opts,
       Logger String,
       BindCookieStore
     ]
    r =>
  Maybe UserId ->
  NominalDiffTime ->
  Sem r SetBindCookie
initializeBindCookie zusr authreqttl = do
  DerivedOpts {derivedOptsBindCookiePath} <- inputs derivedOpts
  msecret <-
    if isJust zusr
      then Just . cs . ES.encode <$> Random.bytes 32
      else pure Nothing
  cky <- fmap SetBindCookie . SAML2.toggleCookie derivedOptsBindCookiePath $ (,authreqttl) <$> msecret
  forM_ zusr $ \userid -> BindCookieStore.insert cky userid authreqttl
  pure cky

redirectURLMaxLength :: Int
redirectURLMaxLength = 140

validateAuthreqParams :: Member (Error SparError) r => Maybe URI.URI -> Maybe URI.URI -> Sem r VerdictFormat
validateAuthreqParams msucc merr = case (msucc, merr) of
  (Nothing, Nothing) -> pure VerdictFormatWeb
  (Just ok, Just err) -> do
    validateRedirectURL `mapM_` [ok, err]
    pure $ VerdictFormatMobile ok err
  _ -> throwSparSem $ SparBadInitiateLoginQueryParams "need-both-redirect-urls"

validateRedirectURL :: Member (Error SparError) r => URI.URI -> Sem r ()
validateRedirectURL uri = do
  unless ((SBS.take 4 . URI.schemeBS . URI.uriScheme $ uri) == "wire") $ do
    throwSparSem $ SparBadInitiateLoginQueryParams "invalid-schema"
  unless ((SBS.length $ URI.serializeURIRef' uri) <= redirectURLMaxLength) $ do
    throwSparSem $ SparBadInitiateLoginQueryParams "url-too-long"

authresp ::
  forall r.
  Members
    '[ Random,
       Logger String,
       Input Opts,
       GalleyAccess,
       BrigAccess,
       BindCookieStore,
       AssIDStore,
       VerdictFormatStore,
       AReqIDStore,
       ScimTokenStore,
       IdPConfigStore,
       SAML2,
       SamlProtocolSettings,
       Error SparError,
       Reporter,
       SAMLUserStore
     ]
    r =>
  Maybe TeamId ->
  Maybe ST ->
  SAML.AuthnResponseBody ->
  Sem r Void
authresp mbtid ckyraw arbody = logErrors $ SAML2.authResp mbtid (SamlProtocolSettings.spIssuer mbtid) (SamlProtocolSettings.responseURI mbtid) go arbody
  where
    cky :: Maybe BindCookie
    cky = ckyraw >>= bindCookieFromHeader

    go :: SAML.AuthnResponse -> SAML.AccessVerdict -> Sem r Void
    go resp verdict = do
      result :: SAML.ResponseVerdict <- verdictHandler cky mbtid resp verdict
      throw @SparError $ SAML.CustomServant result

    logErrors :: Sem r Void -> Sem r Void
    logErrors action = catch @SparError action $ \case
      e@(SAML.CustomServant _) -> throw e
      e -> do
        throw @SparError . SAML.CustomServant $
          errorPage
            e
            (Multipart.inputs (SAML.authnResponseBodyRaw arbody))
            ckyraw

ssoSettings :: Member DefaultSsoCode r => Sem r SsoSettings
ssoSettings = do
  SsoSettings <$> DefaultSsoCode.get

----------------------------------------------------------------------------
-- IdPConfigStore API

idpGet ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPConfigStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  SAML.IdPId ->
  Sem r IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  pure idp

idpGetRaw ::
  Members
    '[ GalleyAccess,
       BrigAccess,
       IdPConfigStore,
       IdPRawMetadataStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  SAML.IdPId ->
  Sem r RawIdPMetadata
idpGetRaw zusr idpid = do
  idp <- getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  IdPRawMetadataStore.get idpid >>= \case
    Just txt -> pure $ RawIdPMetadata txt
    Nothing -> throwSparSem $ SparIdPNotFound (cs $ show idpid)

idpGetAll ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPConfigStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  Sem r IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- Brig.getZUsrCheckPerm zusr ReadIdp
  _idplProviders <- IdPConfigStore.getConfigsByTeam teamid
  pure IdPList {..}

-- | Delete empty IdPs, or if @"purge=true"@ in the HTTP query, delete all users
-- *synchronously* on brig and spar, and the IdP once it's empty.
--
-- The @"purge"@ query parameter is as a quick work-around until we have something better.  It
-- may very well time out, but it processes the users under the 'IdP' in chunks of 2000, so no
-- matter what the team size, it shouldn't choke any servers, just the client (which is
-- probably curl running locally on one of the spar instances).
-- https://github.com/zinfra/backend-issues/issues/1314
idpDelete ::
  forall r.
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       SAMLUserStore,
       IdPConfigStore,
       IdPRawMetadataStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  SAML.IdPId ->
  Maybe Bool ->
  Sem r NoContent
idpDelete zusr idpid (fromMaybe False -> purge) = withDebugLog "idpDelete" (const Nothing) $ do
  idp <- getIdPConfig idpid
  _ <- authorizeIdP zusr idp
  let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
      team = idp ^. SAML.idpExtraInfo . wiTeam
  -- if idp is not empty: fail or purge
  idpIsEmpty <- isNothing <$> SAMLUserStore.getAnyByIssuer issuer
  let doPurge :: Sem r ()
      doPurge = do
        some <- SAMLUserStore.getSomeByIssuer issuer
        forM_ some $ \(uref, uid) -> do
          BrigAccess.delete uid
          SAMLUserStore.delete uid uref
        unless (null some) doPurge
  when (not idpIsEmpty) $ do
    if purge
      then doPurge
      else throwSparSem SparIdPHasBoundUsers
  updateOldIssuers idp
  updateReplacingIdP idp
  -- Delete tokens associated with given IdP (we rely on the fact that
  -- each IdP has exactly one team so we can look up all tokens
  -- associated with the team and then filter them)
  tokens <- ScimTokenStore.lookupByTeam team
  for_ tokens $ \ScimTokenInfo {..} ->
    when (stiIdP == Just idpid) $ ScimTokenStore.delete team stiId
  -- Delete IdP config
  do
    IdPConfigStore.deleteConfig idp
    IdPRawMetadataStore.delete idpid
  return NoContent
  where
    updateOldIssuers :: IdP -> Sem r ()
    updateOldIssuers _ = pure ()
    -- we *could* update @idp ^. SAML.idpExtraInfo . wiReplacedBy@ to not keep the idp about
    -- to be deleted in its old issuers list, but it's tricky to avoid race conditions, and
    -- there is little to be gained here: we only use old issuers to find users that have not
    -- been migrated yet, and if an old user points to a deleted idp, it just means that we
    -- won't find any users to migrate.  still, doesn't hurt mucht to look either.  so we
    -- leave old issuers dangling for now.

    updateReplacingIdP :: IdP -> Sem r ()
    updateReplacingIdP idp = forM_ (idp ^. SAML.idpExtraInfo . wiOldIssuers) $ \oldIssuer -> do
      getIdPIdByIssuer oldIssuer (idp ^. SAML.idpExtraInfo . wiTeam) >>= \case
        GetIdPFound iid -> IdPConfigStore.clearReplacedBy $ Replaced iid
        GetIdPNotFound -> pure ()
        GetIdPDanglingId _ -> pure ()
        GetIdPNonUnique _ -> pure ()
        GetIdPWrongTeam _ -> pure ()

-- | This handler only does the json parsing, and leaves all authorization checks and
-- application logic to 'idpCreateXML'.
idpCreate ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       IdPRawMetadataStore,
       IdPConfigStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  IdPMetadataInfo ->
  Maybe SAML.IdPId ->
  Maybe WireIdPAPIVersion ->
  Sem r IdP
idpCreate zusr (IdPMetadataValue raw xml) midpid apiversion = idpCreateXML zusr raw xml midpid apiversion

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
idpCreateXML ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       IdPConfigStore,
       IdPRawMetadataStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  Text ->
  SAML.IdPMetadata ->
  Maybe SAML.IdPId ->
  Maybe WireIdPAPIVersion ->
  Sem r IdP
idpCreateXML zusr raw idpmeta mReplaces (fromMaybe defWireIdPAPIVersion -> apiversion) = withDebugLog "idpCreate" (Just . show . (^. SAML.idpId)) $ do
  teamid <- Brig.getZUsrCheckPerm zusr CreateUpdateDeleteIdp
  GalleyAccess.assertSSOEnabled teamid
  assertNoScimOrNoIdP teamid
  idp <- validateNewIdP apiversion idpmeta teamid mReplaces
  IdPRawMetadataStore.store (idp ^. SAML.idpId) raw
  storeIdPConfig idp
  forM_ mReplaces $ \replaces -> do
    IdPConfigStore.setReplacedBy (Replaced replaces) (Replacing (idp ^. SAML.idpId))
  pure idp

-- | In teams with a scim access token, only one IdP is allowed.  The reason is that scim user
-- data contains no information about the idp issuer, only the user name, so no valid saml
-- credentials can be created.  To fix this, we need to implement a way to associate scim
-- tokens with IdPs.  https://wearezeta.atlassian.net/browse/SQSERVICES-165
assertNoScimOrNoIdP ::
  Members
    '[ ScimTokenStore,
       Error SparError,
       IdPConfigStore
     ]
    r =>
  TeamId ->
  Sem r ()
assertNoScimOrNoIdP teamid = do
  numTokens <- length <$> ScimTokenStore.lookupByTeam teamid
  numIdps <- length <$> IdPConfigStore.getConfigsByTeam teamid
  when (numTokens > 0 && numIdps > 0) $ do
    throwSparSem $
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
  (HasCallStack, m ~ Sem r) =>
  Members
    '[ Random,
       Logger String,
       IdPConfigStore,
       Error SparError
     ]
    r =>
  WireIdPAPIVersion ->
  SAML.IdPMetadata ->
  TeamId ->
  Maybe SAML.IdPId ->
  m IdP
validateNewIdP apiversion _idpMetadata teamId mReplaces = withDebugLog "validateNewIdP" (Just . show . (^. SAML.idpId)) $ do
  _idpId <- SAML.IdPId <$> Random.uuid
  oldIssuers :: [SAML.Issuer] <- case mReplaces of
    Nothing -> pure []
    Just replaces -> do
      idp <- IdPConfigStore.getConfig replaces >>= maybe (throwSparSem (SparIdPNotFound (cs $ show mReplaces))) pure
      pure $ (idp ^. SAML.idpMetadata . SAML.edIssuer) : (idp ^. SAML.idpExtraInfo . wiOldIssuers)
  let requri = _idpMetadata ^. SAML.edRequestURI
      _idpExtraInfo = WireIdP teamId (Just apiversion) oldIssuers Nothing
  enforceHttps requri
  idp <- getIdPConfigByIssuer (_idpMetadata ^. SAML.edIssuer) teamId
  Logger.log Logger.Debug $ show (apiversion, _idpMetadata, teamId, mReplaces)
  Logger.log Logger.Debug $ show (_idpId, oldIssuers, idp)

  let handleIdPClash :: Either id idp -> m ()
      -- (HINT: using type vars above instead of the actual types constitutes a proof that
      -- we're not using any properties of the arguments in this function.)
      handleIdPClash = case apiversion of
        WireIdPAPIV1 -> const $ do
          throwSparSem $ SparNewIdPAlreadyInUse "you can't create an IdP with api_version v1 if the issuer is already in use on the wire instance."
        WireIdPAPIV2 -> \case
          (Right _) -> do
            -- idp' was found by lookup with teamid, so it's in the same team.
            throwSparSem $ SparNewIdPAlreadyInUse "if the exisitng IdP is registered for a team, the new one can't have it."
          (Left _) -> do
            -- this idp *id* is from a different team, and we're in the 'WireIdPAPIV2' case, so this is fine.
            pure ()

  case idp of
    GetIdPFound idp' {- same team -} -> handleIdPClash (Right idp')
    GetIdPNotFound -> pure ()
    res@(GetIdPDanglingId _) -> throwSparSem . SparIdPNotFound . ("validateNewIdP: " <>) . cs . show $ res -- database inconsistency
    GetIdPNonUnique ids' {- same team didn't yield anything, but there are at least two other teams with this issuer already -} -> handleIdPClash (Left ids')
    GetIdPWrongTeam id' {- different team -} -> handleIdPClash (Left id')

  pure SAML.IdPConfig {..}

-- | FUTUREWORK: 'idpUpdateXML' is only factored out of this function for symmetry with
-- 'idpCreate', which is not a good reason.  make this one function and pass around
-- 'IdPMetadataInfo' directly where convenient.
idpUpdate ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPConfigStore,
       IdPRawMetadataStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  IdPMetadataInfo ->
  SAML.IdPId ->
  Sem r IdP
idpUpdate zusr (IdPMetadataValue raw xml) idpid = idpUpdateXML zusr raw xml idpid

idpUpdateXML ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPConfigStore,
       IdPRawMetadataStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  Text ->
  SAML.IdPMetadata ->
  SAML.IdPId ->
  Sem r IdP
idpUpdateXML zusr raw idpmeta idpid = withDebugLog "idpUpdate" (Just . show . (^. SAML.idpId)) $ do
  (teamid, idp) <- validateIdPUpdate zusr idpmeta idpid
  GalleyAccess.assertSSOEnabled teamid
  IdPRawMetadataStore.store (idp ^. SAML.idpId) raw
  -- (if raw metadata is stored and then spar goes out, raw metadata won't match the
  -- structured idp config.  since this will lead to a 5xx response, the client is expected to
  -- try again, which would clean up cassandra state.)
  storeIdPConfig idp
  -- if the IdP issuer is updated, the old issuer must be removed explicitly.
  -- if this step is ommitted (due to a crash) resending the update request should fix the inconsistent state.
  forM_ (idp ^. SAML.idpExtraInfo . wiOldIssuers) IdPConfigStore.deleteIssuer
  pure idp

-- | Check that: idp id is valid; calling user is admin in that idp's home team; team id in
-- new metainfo doesn't change; new issuer (if changed) is not in use anywhere else (except as
-- an earlier IdP under the same ID); request uri is https.  Keep track of old issuer in extra
-- info if issuer has changed.
validateIdPUpdate ::
  forall m r.
  (HasCallStack, m ~ Sem r) =>
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPConfigStore,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  SAML.IdPMetadata ->
  SAML.IdPId ->
  m (TeamId, IdP)
validateIdPUpdate zusr _idpMetadata _idpId = withDebugLog "validateNewIdP" (Just . show . (_2 %~ (^. SAML.idpId))) $ do
  previousIdP <-
    IdPConfigStore.getConfig _idpId >>= \case
      Nothing -> throw errUnknownIdPId
      Just idp -> pure idp
  teamId <- authorizeIdP zusr previousIdP
  unless (previousIdP ^. SAML.idpExtraInfo . wiTeam == teamId) $ do
    throw errUnknownIdP
  _idpExtraInfo <- do
    let previousIssuer = previousIdP ^. SAML.idpMetadata . SAML.edIssuer
        newIssuer = _idpMetadata ^. SAML.edIssuer
    if previousIssuer == newIssuer
      then pure $ previousIdP ^. SAML.idpExtraInfo
      else do
        foundConfig <- getIdPConfigByIssuerAllowOld newIssuer (Just teamId)
        notInUseByOthers <- case foundConfig of
          GetIdPFound c -> pure $ c ^. SAML.idpId == _idpId
          GetIdPNotFound -> pure True
          res@(GetIdPDanglingId _) -> throwSparSem . SparIdPNotFound . ("validateIdPUpdate: " <>) . cs . show $ res -- impossible
          res@(GetIdPNonUnique _) -> throwSparSem . SparIdPNotFound . ("validateIdPUpdate: " <>) . cs . show $ res -- impossible (because team id was used in lookup)
          GetIdPWrongTeam _ -> pure False
        if notInUseByOthers
          then pure $ (previousIdP ^. SAML.idpExtraInfo) & wiOldIssuers %~ nub . (previousIssuer :)
          else throwSparSem SparIdPIssuerInUse
  let requri = _idpMetadata ^. SAML.edRequestURI
  enforceHttps requri
  pure (teamId, SAML.IdPConfig {..})
  where
    errUnknownIdP = SAML.UnknownIdP $ enc uri
      where
        enc = cs . toLazyByteString . URI.serializeURIRef
        uri = _idpMetadata ^. SAML.edIssuer . SAML.fromIssuer
    errUnknownIdPId = SAML.UnknownIdP . cs . SAML.idPIdToST $ _idpId

withDebugLog :: Member (Logger String) r => String -> (a -> Maybe String) -> Sem r a -> Sem r a
withDebugLog msg showval action = do
  Logger.log Logger.Debug $ "entering " ++ msg
  val <- action
  let mshowedval = showval val
  Logger.log Logger.Debug $ "leaving " ++ msg ++ mconcat [": " ++ fromJust mshowedval | isJust mshowedval]
  pure val

authorizeIdP ::
  (HasCallStack, Members '[GalleyAccess, BrigAccess, Error SparError] r) =>
  Maybe UserId ->
  IdP ->
  Sem r TeamId
authorizeIdP Nothing _ = throw (SAML.CustomError $ SparNoPermission (cs $ show CreateUpdateDeleteIdp))
authorizeIdP (Just zusr) idp = do
  let teamid = idp ^. SAML.idpExtraInfo . wiTeam
  GalleyAccess.assertHasPermission teamid CreateUpdateDeleteIdp zusr
  pure teamid

enforceHttps :: Member (Error SparError) r => URI.URI -> Sem r ()
enforceHttps uri = do
  unless ((uri ^. URI.uriSchemeL . URI.schemeBSL) == "https") $ do
    throwSparSem . SparNewIdPWantHttps . cs . SAML.renderURI $ uri

----------------------------------------------------------------------------
-- Internal API

internalStatus :: Sem r NoContent
internalStatus = pure NoContent

-- | Cleanup handler that is called by Galley whenever a team is about to
-- get deleted.
internalDeleteTeam :: Members '[ScimTokenStore, IdPConfigStore, SAMLUserStore] r => TeamId -> Sem r NoContent
internalDeleteTeam team = do
  deleteTeam team
  pure NoContent

internalPutSsoSettings ::
  Members
    '[ DefaultSsoCode,
       Error SparError,
       IdPConfigStore
     ]
    r =>
  SsoSettings ->
  Sem r NoContent
internalPutSsoSettings SsoSettings {defaultSsoCode = Nothing} = do
  DefaultSsoCode.delete
  pure NoContent
internalPutSsoSettings SsoSettings {defaultSsoCode = Just code} = do
  IdPConfigStore.getConfig code >>= \case
    Nothing ->
      -- this will return a 404, which is not quite right,
      -- but it's an internal endpoint and the message clearly says
      -- "Could not find IdP".
      throwSparSem $ SparIdPNotFound mempty
    Just _ -> do
      DefaultSsoCode.store code
      pure NoContent
