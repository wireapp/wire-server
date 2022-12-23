{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# HLINT ignore "Use $>" #-}

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

import Brig.Types.Intra
import Brig.Types.User (HavePendingInvitations (NoPendingInvitations))
import Cassandra as Cas
import Control.Lens
import Control.Monad.Except
import qualified Data.ByteString as SBS
import Data.ByteString.Builder (toLazyByteString)
import Data.Id
import Data.Proxy
import Data.Range
import qualified Data.Set as Set
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
import Spar.Options
import Spar.Orphans ()
import Spar.Scim
import Spar.Sem.AReqIDStore (AReqIDStore)
import Spar.Sem.AssIDStore (AssIDStore)
import Spar.Sem.BrigAccess (BrigAccess)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.DefaultSsoCode (DefaultSsoCode)
import qualified Spar.Sem.DefaultSsoCode as DefaultSsoCode
import Spar.Sem.GalleyAccess (GalleyAccess)
import qualified Spar.Sem.GalleyAccess as GalleyAccess
import Spar.Sem.IdPConfigStore (IdPConfigStore, Replaced (..), Replacing (..))
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
import qualified Spar.Sem.ScimUserTimesStore as ScimUserTimesStore
import Spar.Sem.VerdictFormatStore (VerdictFormatStore)
import qualified Spar.Sem.VerdictFormatStore as VerdictFormatStore
import System.Logger (Msg)
import qualified URI.ByteString as URI
import Wire.API.Routes.Public.Spar
import Wire.API.User
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
    :<|> apiIDP
    :<|> apiScim
    :<|> apiINTERNAL

apiSSO ::
  Members
    '[ GalleyAccess,
       Logger String,
       Input Opts,
       BrigAccess,
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
  SAML2.meta appName (SamlProtocolSettings.spIssuer Nothing) (SamlProtocolSettings.responseURI Nothing)
    :<|> (\tid -> SAML2.meta appName (SamlProtocolSettings.spIssuer (Just tid)) (SamlProtocolSettings.responseURI (Just tid)))
    :<|> authreqPrecheck
    :<|> authreq (maxttlAuthreqDiffTime opts)
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
       SAMLUserStore,
       ScimUserTimesStore
     ]
    r =>
  ServerT APIINTERNAL (Sem r)
apiINTERNAL =
  internalStatus
    :<|> internalDeleteTeam
    :<|> internalPutSsoSettings
    :<|> internalGetScimUserInfo

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
    *> IdPConfigStore.getConfig idpid
    $> NoContent

authreq ::
  Members
    '[ Random,
       Input Opts,
       Logger String,
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
  Maybe URI.URI ->
  Maybe URI.URI ->
  SAML.IdPId ->
  Sem r (SAML.FormRedirect SAML.AuthnRequest)
authreq authreqttl msucc merr idpid = do
  vformat <- validateAuthreqParams msucc merr
  form@(SAML.FormRedirect _ ((^. SAML.rqID) -> reqid)) <- do
    idp :: IdP <- IdPConfigStore.getConfig idpid
    let mbtid :: Maybe TeamId
        mbtid = case fromMaybe defWireIdPAPIVersion (idp ^. SAML.idpExtraInfo . wiApiVersion) of
          WireIdPAPIV1 -> Nothing
          WireIdPAPIV2 -> Just $ idp ^. SAML.idpExtraInfo . wiTeam
    SAML2.authReq authreqttl (SamlProtocolSettings.spIssuer mbtid) idpid
  VerdictFormatStore.store authreqttl reqid vformat
  pure form

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
  unless (SBS.length (URI.serializeURIRef' uri) <= redirectURLMaxLength) $ do
    throwSparSem $ SparBadInitiateLoginQueryParams "url-too-long"

authresp ::
  forall r.
  Members
    '[ Random,
       Logger String,
       Input Opts,
       GalleyAccess,
       BrigAccess,
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
  SAML.AuthnResponseBody ->
  Sem r Void
authresp mbtid arbody = logErrors $ SAML2.authResp mbtid (SamlProtocolSettings.spIssuer mbtid) (SamlProtocolSettings.responseURI mbtid) go arbody
  where
    go :: SAML.AuthnResponse -> IdP -> SAML.AccessVerdict -> Sem r Void
    go resp verdict idp = do
      result :: SAML.ResponseVerdict <- verdictHandler resp idp verdict
      throw @SparError $ SAML.CustomServant result

    logErrors :: Sem r Void -> Sem r Void
    logErrors action = catch @SparError action $ \case
      e@(SAML.CustomServant _) -> throw e
      e -> do
        throw @SparError . SAML.CustomServant $
          errorPage
            e
            (Multipart.inputs (SAML.authnResponseBodyRaw arbody))

ssoSettings :: Member DefaultSsoCode r => Sem r SsoSettings
ssoSettings =
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
  idp <- IdPConfigStore.getConfig idpid
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
  idp <- IdPConfigStore.getConfig idpid
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
idpDelete mbzusr idpid (fromMaybe False -> purge) = withDebugLog "idpDelete" (const Nothing) $ do
  idp <- IdPConfigStore.getConfig idpid
  (zusr, team) <- authorizeIdP mbzusr idp
  let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
  whenM (idpDoesAuthSelf idp zusr) $ throwSparSem SparIdPCannotDeleteOwnIdp
  SAMLUserStore.getAllByIssuerPaginated issuer >>= assertEmptyOrPurge team
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
  pure NoContent
  where
    assertEmptyOrPurge :: TeamId -> Cas.Page (SAML.UserRef, UserId) -> Sem r ()
    assertEmptyOrPurge team page = do
      forM_ (Cas.result page) $ \(uref, uid) -> do
        mAccount <- BrigAccess.getAccount NoPendingInvitations uid
        let mUserTeam = userTeam . accountUser =<< mAccount
        when (mUserTeam == Just team) $ do
          if purge
            then do
              SAMLUserStore.delete uid uref
              void $ BrigAccess.deleteUser uid
            else do
              throwSparSem SparIdPHasBoundUsers
      when (Cas.hasMore page) $
        SAMLUserStore.nextPage page >>= assertEmptyOrPurge team

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
      iid <-
        view SAML.idpId <$> case fromMaybe defWireIdPAPIVersion $ idp ^. SAML.idpExtraInfo . wiApiVersion of
          WireIdPAPIV1 -> IdPConfigStore.getIdPByIssuerV1 oldIssuer
          WireIdPAPIV2 -> IdPConfigStore.getIdPByIssuerV2 oldIssuer (idp ^. SAML.idpExtraInfo . wiTeam)
      IdPConfigStore.clearReplacedBy $ Replaced iid

    idpDoesAuthSelf :: IdP -> UserId -> Sem r Bool
    idpDoesAuthSelf idp uid = do
      let idpIssuer = idp ^. SAML.idpMetadata . SAML.edIssuer
      mUserIssuer <- (>>= userIssuer) <$> Brig.getBrigUser NoPendingInvitations uid
      pure $ mUserIssuer == Just idpIssuer

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
  Maybe (Range 1 32 Text) ->
  Sem r IdP
idpCreate zusr (IdPMetadataValue raw xml) = idpCreateXML zusr raw xml

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
  Maybe (Range 1 32 Text) ->
  Sem r IdP
idpCreateXML zusr raw idpmeta mReplaces (fromMaybe defWireIdPAPIVersion -> apiversion) mHandle = withDebugLog "idpCreateXML" (Just . show . (^. SAML.idpId)) $ do
  teamid <- Brig.getZUsrCheckPerm zusr CreateUpdateDeleteIdp
  GalleyAccess.assertSSOEnabled teamid
  assertNoScimOrNoIdP teamid
  handle <- maybe (IdPConfigStore.newHandle teamid) (pure . IdPHandle . fromRange) mHandle
  idp <- validateNewIdP apiversion idpmeta teamid mReplaces handle
  IdPRawMetadataStore.store (idp ^. SAML.idpId) raw
  IdPConfigStore.insertConfig idp
  forM_ mReplaces $ \replaces ->
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
  when (numTokens > 0 && numIdps > 0) $
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
  IdPHandle ->
  m IdP
validateNewIdP apiversion _idpMetadata teamId mReplaces handle = withDebugLog "validateNewIdP" (Just . show . (^. SAML.idpId)) $ do
  _idpId <- SAML.IdPId <$> Random.uuid
  oldIssuers :: [SAML.Issuer] <- case mReplaces of
    Nothing -> pure []
    Just replaces -> do
      idp <- IdPConfigStore.getConfig replaces
      pure $ (idp ^. SAML.idpMetadata . SAML.edIssuer) : (idp ^. SAML.idpExtraInfo . wiOldIssuers)
  let requri = _idpMetadata ^. SAML.edRequestURI
      _idpExtraInfo = WireIdP teamId (Just apiversion) oldIssuers Nothing handle
  enforceHttps requri
  mbIdp <- case apiversion of
    WireIdPAPIV1 -> IdPConfigStore.getIdPByIssuerV1Maybe (_idpMetadata ^. SAML.edIssuer)
    WireIdPAPIV2 -> IdPConfigStore.getIdPByIssuerV2Maybe (_idpMetadata ^. SAML.edIssuer) teamId
  Logger.log Logger.Debug $ show (apiversion, _idpMetadata, teamId, mReplaces)
  Logger.log Logger.Debug $ show (_idpId, oldIssuers, mbIdp)

  let failWithIdPClash :: m ()
      failWithIdPClash = throwSparSem . SparNewIdPAlreadyInUse $ case apiversion of
        WireIdPAPIV1 ->
          "you can't create an IdP with api_version v1 if the issuer is already in use on the wire instance."
        WireIdPAPIV2 ->
          -- idp was found by lookup with teamid, so it's in the same team.
          "you can't create an IdP with api_version v1 if the issuer is already in use in your team."

  unless (isNothing mbIdp) failWithIdPClash

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
  Maybe (Range 1 32 Text) ->
  Sem r IdP
idpUpdate zusr (IdPMetadataValue raw xml) = idpUpdateXML zusr raw xml

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
  Maybe (Range 1 32 Text) ->
  Sem r IdP
idpUpdateXML zusr raw idpmeta idpid mHandle = withDebugLog "idpUpdateXML" (Just . show . (^. SAML.idpId)) $ do
  (teamid, idp) <- validateIdPUpdate zusr idpmeta idpid
  GalleyAccess.assertSSOEnabled teamid
  IdPRawMetadataStore.store (idp ^. SAML.idpId) raw
  let idp' :: IdP = case mHandle of
        Just idpHandle -> idp & (SAML.idpExtraInfo . wiHandle) .~ IdPHandle (fromRange idpHandle)
        Nothing -> idp
  -- (if raw metadata is stored and then spar goes out, raw metadata won't match the
  -- structured idp config.  since this will lead to a 5xx response, the client is expected to
  -- try again, which would clean up cassandra state.)
  IdPConfigStore.insertConfig idp'
  -- if the IdP issuer is updated, the old issuer must be removed explicitly.
  -- if this step is ommitted (due to a crash) resending the update request should fix the inconsistent state.
  let mbteamid = case fromMaybe defWireIdPAPIVersion $ idp' ^. SAML.idpExtraInfo . wiApiVersion of
        WireIdPAPIV1 -> Nothing
        WireIdPAPIV2 -> Just teamid
  forM_ (idp' ^. SAML.idpExtraInfo . wiOldIssuers) (flip IdPConfigStore.deleteIssuer mbteamid)
  pure idp'

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
validateIdPUpdate zusr _idpMetadata _idpId = withDebugLog "validateIdPUpdate" (Just . show . (_2 %~ (^. SAML.idpId))) $ do
  previousIdP <- IdPConfigStore.getConfig _idpId
  (_, teamId) <- authorizeIdP zusr previousIdP
  unless (previousIdP ^. SAML.idpExtraInfo . wiTeam == teamId) $
    throw errUnknownIdP
  _idpExtraInfo <- do
    let previousIssuer = previousIdP ^. SAML.idpMetadata . SAML.edIssuer
        newIssuer = _idpMetadata ^. SAML.edIssuer
    if previousIssuer == newIssuer
      then do
        -- idempotency
        pure $ previousIdP ^. SAML.idpExtraInfo
      else do
        idpIssuerInUse <-
          ( case fromMaybe defWireIdPAPIVersion $ previousIdP ^. SAML.idpExtraInfo . wiApiVersion of
              WireIdPAPIV1 -> IdPConfigStore.getIdPByIssuerV1Maybe newIssuer
              WireIdPAPIV2 -> IdPConfigStore.getIdPByIssuerV2Maybe newIssuer teamId
            )
            <&> ( \case
                    Just idpFound -> idpFound ^. SAML.idpId /= _idpId
                    Nothing -> False
                )
        if idpIssuerInUse
          then throwSparSem SparIdPIssuerInUse
          else pure $ previousIdP ^. SAML.idpExtraInfo & wiOldIssuers %~ nub . (previousIssuer :)

  let requri = _idpMetadata ^. SAML.edRequestURI
  enforceHttps requri
  pure (teamId, SAML.IdPConfig {..})
  where
    errUnknownIdP = SAML.UnknownIdP $ enc uri
      where
        enc = cs . toLazyByteString . URI.serializeURIRef
        uri = _idpMetadata ^. SAML.edIssuer . SAML.fromIssuer

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
  Sem r (UserId, TeamId)
authorizeIdP Nothing _ = throw (SAML.CustomError $ SparNoPermission (cs $ show CreateUpdateDeleteIdp))
authorizeIdP (Just zusr) idp = do
  let teamid = idp ^. SAML.idpExtraInfo . wiTeam
  GalleyAccess.assertHasPermission teamid CreateUpdateDeleteIdp zusr
  pure (zusr, teamid)

enforceHttps :: Member (Error SparError) r => URI.URI -> Sem r ()
enforceHttps uri =
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
internalPutSsoSettings SsoSettings {defaultSsoCode = Just code} =
  -- this can throw a 404, which is not quite right,
  -- but it's an internal endpoint and the message clearly says
  -- "Could not find IdP".
  IdPConfigStore.getConfig code
    *> DefaultSsoCode.store code
    $> NoContent

internalGetScimUserInfo :: Members '[ScimUserTimesStore] r => UserSet -> Sem r ScimUserInfos
internalGetScimUserInfo (UserSet uids) = do
  results <- ScimUserTimesStore.readMulti (Set.toList uids)
  let scimUserInfos = results <&> (\(uid, t, _) -> ScimUserInfo uid (Just t))
  pure $ ScimUserInfos scimUserInfos
