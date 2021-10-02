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
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified SAML2.WebSSO as SAML
import Servant
import qualified Servant.Multipart as Multipart
import Spar.App
import Spar.CanonicalInterpreter
import qualified Spar.Data as Data (GetIdPResult (..), Replaced (..), Replacing (..))
import Spar.Error
import qualified Spar.Intra.BrigApp as Brig
import Spar.Orphans ()
import Spar.Scim
import Spar.Sem.AReqIDStore (AReqIDStore)
import qualified Spar.Sem.AReqIDStore as AReqIDStore
import Spar.Sem.AssIDStore (AssIDStore)
import Spar.Sem.BindCookieStore (BindCookieStore)
import qualified Spar.Sem.BindCookieStore as BindCookieStore
import Spar.Sem.BrigAccess (BrigAccess)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.DefaultSsoCode (DefaultSsoCode)
import qualified Spar.Sem.DefaultSsoCode as DefaultSsoCode
import Spar.Sem.GalleyAccess (GalleyAccess)
import qualified Spar.Sem.GalleyAccess as GalleyAccess
import qualified Spar.Sem.IdP as IdPEffect
import Spar.Sem.Logger (Logger)
import qualified Spar.Sem.Logger as Logger
import Spar.Sem.Now (Now)
import Spar.Sem.Random (Random)
import qualified Spar.Sem.Random as Random
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
import System.Logger (Msg)
import qualified URI.ByteString as URI
import Wire.API.Cookie
import Wire.API.Routes.Public.Spar
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml

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
       ScimExternalIdStore,
       ScimUserTimesStore,
       ScimTokenStore,
       DefaultSsoCode,
       IdPEffect.IdP,
       SAMLUserStore,
       Random,
       Error SparError,
       SAML2,
       Now,
       SamlProtocolSettings,
       Logger String,
       Logger (Msg -> Msg),
       -- TODO(sandy): Remove me when we get rid of runSparInSem
       Final IO
     ]
    r =>
  Opts ->
  ServerT API (Spar r)
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
       AReqIDStore,
       ScimTokenStore,
       DefaultSsoCode,
       IdPEffect.IdP,
       Random,
       Error SparError,
       SAML2,
       SamlProtocolSettings,
       SAMLUserStore,
       -- TODO(sandy): Remove me when we get rid of runSparInSem
       Final IO
     ]
    r =>
  Opts ->
  ServerT APISSO (Spar r)
apiSSO opts =
  (liftSem $ SAML2.meta appName (SamlProtocolSettings.spIssuer Nothing) (SamlProtocolSettings.responseURI Nothing))
    :<|> (\tid -> liftSem $ SAML2.meta appName (SamlProtocolSettings.spIssuer (Just tid)) (SamlProtocolSettings.responseURI (Just tid)))
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
       IdPEffect.IdP,
       SAMLUserStore,
       Error SparError
     ]
    r =>
  ServerT APIIDP (Spar r)
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
       IdPEffect.IdP,
       SAMLUserStore
     ]
    r =>
  ServerT APIINTERNAL (Spar r)
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
    *> getIdPConfig idpid
    *> return NoContent

authreq ::
  Members
    '[ Random,
       Input Opts,
       Logger String,
       BindCookieStore,
       AssIDStore,
       AReqIDStore,
       SAML2,
       SamlProtocolSettings,
       IdPEffect.IdP
     ]
    r =>
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
    idp :: IdP <- liftSem (IdPEffect.getConfig idpid) >>= maybe (throwSpar (SparIdPNotFound (cs $ show idpid))) pure
    let mbtid :: Maybe TeamId
        mbtid = case fromMaybe defWireIdPAPIVersion (idp ^. SAML.idpExtraInfo . wiApiVersion) of
          WireIdPAPIV1 -> Nothing
          WireIdPAPIV2 -> Just $ idp ^. SAML.idpExtraInfo . wiTeam
    liftSem $ SAML2.authReq authreqttl (SamlProtocolSettings.spIssuer mbtid) idpid
  liftSem $ AReqIDStore.storeVerdictFormat authreqttl reqid vformat
  cky <- initializeBindCookie zusr authreqttl
  liftSem $ Logger.log SAML.Debug $ "setting bind cookie: " <> show cky
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
  Spar r SetBindCookie
initializeBindCookie zusr authreqttl = do
  DerivedOpts {derivedOptsBindCookiePath} <- liftSem $ inputs derivedOpts
  msecret <-
    if isJust zusr
      then liftSem $ Just . cs . ES.encode <$> Random.bytes 32
      else pure Nothing
  cky <- fmap SetBindCookie . liftSem . SAML2.toggleCookie derivedOptsBindCookiePath $ (,authreqttl) <$> msecret
  forM_ zusr $ \userid -> liftSem $ BindCookieStore.insert cky userid authreqttl
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
       AReqIDStore,
       ScimTokenStore,
       IdPEffect.IdP,
       SAML2,
       SamlProtocolSettings,
       Error SparError,
       SAMLUserStore,
       -- TODO(sandy): Remove me when we get rid of runSparInSem
       Final IO
     ]
    r =>
  Maybe TeamId ->
  Maybe ST ->
  SAML.AuthnResponseBody ->
  Spar r Void
authresp mbtid ckyraw arbody = logErrors $ liftSem $ SAML2.authResp mbtid (SamlProtocolSettings.spIssuer mbtid) (SamlProtocolSettings.responseURI mbtid) go arbody
  where
    cky :: Maybe BindCookie
    cky = ckyraw >>= bindCookieFromHeader

    go :: SAML.AuthnResponse -> SAML.AccessVerdict -> Sem r Void
    go resp verdict = do
      result :: SAML.ResponseVerdict <- runSparInSem $ verdictHandler cky mbtid resp verdict
      throw @SparError $ SAML.CustomServant result

    logErrors :: Spar r Void -> Spar r Void
    logErrors action = liftSem . catch @SparError (runSparInSem action) $ \case
      e@(SAML.CustomServant _) -> throw e
      e -> do
        throw @SparError . SAML.CustomServant $
          errorPage
            e
            (Multipart.inputs (SAML.authnResponseBodyRaw arbody))
            ckyraw

ssoSettings :: Member DefaultSsoCode r => Spar r SsoSettings
ssoSettings = do
  SsoSettings <$> liftSem DefaultSsoCode.get

----------------------------------------------------------------------------
-- IdP API

idpGet ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPEffect.IdP,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  SAML.IdPId ->
  Spar r IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- getIdPConfig idpid
  _ <- liftSem $ authorizeIdP zusr idp
  pure idp

idpGetRaw ::
  Members '[GalleyAccess, BrigAccess, IdPEffect.IdP, Error SparError] r =>
  Maybe UserId ->
  SAML.IdPId ->
  Spar r RawIdPMetadata
idpGetRaw zusr idpid = do
  idp <- getIdPConfig idpid
  _ <- liftSem $ authorizeIdP zusr idp
  liftSem (IdPEffect.getRawMetadata idpid) >>= \case
    Just txt -> pure $ RawIdPMetadata txt
    Nothing -> throwSpar $ SparIdPNotFound (cs $ show idpid)

idpGetAll ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPEffect.IdP,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  Spar r IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- liftSem $ Brig.getZUsrCheckPerm zusr ReadIdp
  _idplProviders <- liftSem $ IdPEffect.getConfigsByTeam teamid
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
       IdPEffect.IdP,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  SAML.IdPId ->
  Maybe Bool ->
  Spar r NoContent
idpDelete zusr idpid (fromMaybe False -> purge) = withDebugLog "idpDelete" (const Nothing) $ do
  idp <- getIdPConfig idpid
  _ <- liftSem $ authorizeIdP zusr idp
  let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
      team = idp ^. SAML.idpExtraInfo . wiTeam
  -- if idp is not empty: fail or purge
  idpIsEmpty <- liftSem $ isNothing <$> SAMLUserStore.getAnyByIssuer issuer
  let doPurge :: Spar r ()
      doPurge = do
        some <- liftSem (SAMLUserStore.getSomeByIssuer issuer)
        forM_ some $ \(uref, uid) -> do
          liftSem $ BrigAccess.delete uid
          liftSem (SAMLUserStore.delete uid uref)
        unless (null some) doPurge
  when (not idpIsEmpty) $ do
    if purge
      then doPurge
      else throwSpar SparIdPHasBoundUsers
  updateOldIssuers idp
  updateReplacingIdP idp
  -- Delete tokens associated with given IdP (we rely on the fact that
  -- each IdP has exactly one team so we can look up all tokens
  -- associated with the team and then filter them)
  tokens <- liftSem $ ScimTokenStore.getByTeam team
  for_ tokens $ \ScimTokenInfo {..} ->
    when (stiIdP == Just idpid) $ liftSem $ ScimTokenStore.delete team stiId
  -- Delete IdP config
  liftSem $ do
    IdPEffect.deleteConfig idpid issuer team
    IdPEffect.deleteRawMetadata idpid
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
      getIdPIdByIssuer oldIssuer (idp ^. SAML.idpExtraInfo . wiTeam) >>= \case
        Data.GetIdPFound iid -> liftSem $ IdPEffect.clearReplacedBy $ Data.Replaced iid
        Data.GetIdPNotFound -> pure ()
        Data.GetIdPDanglingId _ -> pure ()
        Data.GetIdPNonUnique _ -> pure ()
        Data.GetIdPWrongTeam _ -> pure ()

-- | This handler only does the json parsing, and leaves all authorization checks and
-- application logic to 'idpCreateXML'.
idpCreate ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       IdPEffect.IdP,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  IdPMetadataInfo ->
  Maybe SAML.IdPId ->
  Maybe WireIdPAPIVersion ->
  Spar r IdP
idpCreate zusr (IdPMetadataValue raw xml) midpid apiversion = idpCreateXML zusr raw xml midpid apiversion

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
idpCreateXML ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       IdPEffect.IdP,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  Text ->
  SAML.IdPMetadata ->
  Maybe SAML.IdPId ->
  Maybe WireIdPAPIVersion ->
  Spar r IdP
idpCreateXML zusr raw idpmeta mReplaces (fromMaybe defWireIdPAPIVersion -> apiversion) = withDebugLog "idpCreate" (Just . show . (^. SAML.idpId)) $ do
  teamid <- liftSem $ Brig.getZUsrCheckPerm zusr CreateUpdateDeleteIdp
  liftSem $ GalleyAccess.assertSSOEnabled teamid
  assertNoScimOrNoIdP teamid
  idp <- validateNewIdP apiversion idpmeta teamid mReplaces
  liftSem $ IdPEffect.storeRawMetadata (idp ^. SAML.idpId) raw
  storeIdPConfig idp
  forM_ mReplaces $ \replaces -> liftSem $ do
    IdPEffect.setReplacedBy (Data.Replaced replaces) (Data.Replacing (idp ^. SAML.idpId))
  pure idp

-- | In teams with a scim access token, only one IdP is allowed.  The reason is that scim user
-- data contains no information about the idp issuer, only the user name, so no valid saml
-- credentials can be created.  To fix this, we need to implement a way to associate scim
-- tokens with IdPs.  https://wearezeta.atlassian.net/browse/SQSERVICES-165
assertNoScimOrNoIdP :: Members '[ScimTokenStore, IdPEffect.IdP] r => TeamId -> Spar r ()
assertNoScimOrNoIdP teamid = do
  numTokens <- length <$> liftSem (ScimTokenStore.getByTeam teamid)
  numIdps <- length <$> liftSem (IdPEffect.getConfigsByTeam teamid)
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
  Members '[Random, Logger String, IdPEffect.IdP] r =>
  WireIdPAPIVersion ->
  SAML.IdPMetadata ->
  TeamId ->
  Maybe SAML.IdPId ->
  m IdP
validateNewIdP apiversion _idpMetadata teamId mReplaces = withDebugLog "validateNewIdP" (Just . show . (^. SAML.idpId)) $ do
  _idpId <- SAML.IdPId <$> liftSem Random.uuid
  oldIssuers :: [SAML.Issuer] <- case mReplaces of
    Nothing -> pure []
    Just replaces -> do
      idp <- liftSem (IdPEffect.getConfig replaces) >>= maybe (throwSpar (SparIdPNotFound (cs $ show mReplaces))) pure
      pure $ (idp ^. SAML.idpMetadata . SAML.edIssuer) : (idp ^. SAML.idpExtraInfo . wiOldIssuers)
  let requri = _idpMetadata ^. SAML.edRequestURI
      _idpExtraInfo = WireIdP teamId (Just apiversion) oldIssuers Nothing
  enforceHttps requri
  idp <- getIdPConfigByIssuer (_idpMetadata ^. SAML.edIssuer) teamId
  liftSem $ Logger.log SAML.Debug $ show (apiversion, _idpMetadata, teamId, mReplaces)
  liftSem $ Logger.log SAML.Debug $ show (_idpId, oldIssuers, idp)

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
idpUpdate ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPEffect.IdP,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  IdPMetadataInfo ->
  SAML.IdPId ->
  Spar r IdP
idpUpdate zusr (IdPMetadataValue raw xml) idpid = idpUpdateXML zusr raw xml idpid

idpUpdateXML ::
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPEffect.IdP,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  Text ->
  SAML.IdPMetadata ->
  SAML.IdPId ->
  Spar r IdP
idpUpdateXML zusr raw idpmeta idpid = withDebugLog "idpUpdate" (Just . show . (^. SAML.idpId)) $ do
  (teamid, idp) <- validateIdPUpdate zusr idpmeta idpid
  liftSem $ GalleyAccess.assertSSOEnabled teamid
  liftSem $ IdPEffect.storeRawMetadata (idp ^. SAML.idpId) raw
  -- (if raw metadata is stored and then spar goes out, raw metadata won't match the
  -- structured idp config.  since this will lead to a 5xx response, the client is epected to
  -- try again, which would clean up cassandra state.)
  storeIdPConfig idp
  pure idp

-- | Check that: idp id is valid; calling user is admin in that idp's home team; team id in
-- new metainfo doesn't change; new issuer (if changed) is not in use anywhere else (except as
-- an earlier IdP under the same ID); request uri is https.  Keep track of old issuer in extra
-- info if issuer has changed.
validateIdPUpdate ::
  forall m r.
  (HasCallStack, m ~ Spar r) =>
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       IdPEffect.IdP,
       Error SparError
     ]
    r =>
  Maybe UserId ->
  SAML.IdPMetadata ->
  SAML.IdPId ->
  m (TeamId, IdP)
validateIdPUpdate zusr _idpMetadata _idpId = withDebugLog "validateNewIdP" (Just . show . (_2 %~ (^. SAML.idpId))) $ do
  previousIdP <-
    liftSem (IdPEffect.getConfig _idpId) >>= \case
      Nothing -> throwError errUnknownIdPId
      Just idp -> pure idp
  teamId <- liftSem $ authorizeIdP zusr previousIdP
  unless (previousIdP ^. SAML.idpExtraInfo . wiTeam == teamId) $ do
    throwError errUnknownIdP
  _idpExtraInfo <- do
    let previousIssuer = previousIdP ^. SAML.idpMetadata . SAML.edIssuer
        newIssuer = _idpMetadata ^. SAML.edIssuer
    if previousIssuer == newIssuer
      then pure $ previousIdP ^. SAML.idpExtraInfo
      else do
        foundConfig <- getIdPConfigByIssuerAllowOld newIssuer (Just teamId)
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

withDebugLog :: Member (Logger String) r => String -> (a -> Maybe String) -> Spar r a -> Spar r a
withDebugLog msg showval action = do
  liftSem $ Logger.log SAML.Debug $ "entering " ++ msg
  val <- action
  let mshowedval = showval val
  liftSem $ Logger.log SAML.Debug $ "leaving " ++ msg ++ mconcat [": " ++ fromJust mshowedval | isJust mshowedval]
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
internalDeleteTeam :: Members '[ScimTokenStore, IdPEffect.IdP, SAMLUserStore] r => TeamId -> Spar r NoContent
internalDeleteTeam team = do
  deleteTeam team
  pure NoContent

internalPutSsoSettings :: Members '[DefaultSsoCode, IdPEffect.IdP] r => SsoSettings -> Spar r NoContent
internalPutSsoSettings SsoSettings {defaultSsoCode = Nothing} = do
  liftSem $ DefaultSsoCode.delete
  pure NoContent
internalPutSsoSettings SsoSettings {defaultSsoCode = Just code} = do
  liftSem (IdPEffect.getConfig code) >>= \case
    Nothing ->
      -- this will return a 404, which is not quite right,
      -- but it's an internal endpoint and the message clearly says
      -- "Could not find IdP".
      throwSpar $ SparIdPNotFound mempty
    Just _ -> do
      liftSem $ DefaultSsoCode.store code
      pure NoContent
