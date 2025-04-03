{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use $>" #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
    SparAPI,

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
import Cassandra as Cas
import Control.Lens hiding ((.=))
import qualified Data.ByteString as SBS
import Data.ByteString.Builder (toLazyByteString)
import Data.Domain
import Data.HavePendingInvitations
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Range
import Data.Text.Encoding.Error
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Data.Time
import Imports
import Network.Wai.Utilities.Request
import Network.Wai.Utilities.Server (defaultRequestIdHeaderName)
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
import Spar.Scim hiding (handle)
import Spar.Sem.AReqIDStore (AReqIDStore)
import Spar.Sem.AssIDStore (AssIDStore)
import Spar.Sem.BrigAccess (BrigAccess, getAccount)
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
import Wire.API.Routes.Internal.Spar
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Spar
import Wire.API.Team.Member (HiddenPerm (CreateUpdateDeleteIdp, ReadIdp))
import Wire.API.User
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.Sem.Logger (Logger)
import qualified Wire.Sem.Logger as Logger
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import qualified Wire.Sem.Random as Random

app :: Env -> Application
app ctx0 req cont = do
  let rid = getRequestId defaultRequestIdHeaderName req
  let ctx = ctx0 {sparCtxRequestId = rid}
  SAML.setHttpCachePolicy
    ( serve
        (Proxy @SparAPI)
        (hoistServer (Proxy @SparAPI) (runSparToHandler ctx) (api $ sparCtxOpts ctx) :: Server SparAPI)
    )
    req
    cont

api ::
  ( Member GalleyAccess r,
    Member BrigAccess r,
    Member (Input Opts) r,
    Member AssIDStore r,
    Member AReqIDStore r,
    Member VerdictFormatStore r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r,
    Member ScimTokenStore r,
    Member DefaultSsoCode r,
    Member IdPConfigStore r,
    Member IdPRawMetadataStore r,
    Member SAMLUserStore r,
    Member Random r,
    Member (Error SparError) r,
    Member SAML2 r,
    Member Now r,
    Member SamlProtocolSettings r,
    Member (Logger String) r,
    Member Reporter r,
    Member
      ( -- TODO(sandy): Only necessary for 'fromExceptionSem' in 'apiScim'
        Final IO
      )
      r,
    Member (Logger (Msg -> Msg)) r
  ) =>
  Opts ->
  ServerT SparAPI (Sem r)
api opts =
  apiSSO opts
    :<|> apiIDP
    :<|> apiScim
    :<|> apiINTERNAL

apiSSO ::
  ( Member GalleyAccess r,
    Member (Logger String) r,
    Member (Input Opts) r,
    Member BrigAccess r,
    Member AssIDStore r,
    Member VerdictFormatStore r,
    Member AReqIDStore r,
    Member ScimTokenStore r,
    Member DefaultSsoCode r,
    Member IdPConfigStore r,
    Member Random r,
    Member (Error SparError) r,
    Member SAML2 r,
    Member SamlProtocolSettings r,
    Member Reporter r,
    Member SAMLUserStore r
  ) =>
  Opts ->
  ServerT APISSO (Sem r)
apiSSO opts =
  Named @"sso-metadata" (getMetadata Nothing)
    :<|> Named @"sso-team-metadata" (\mbHost tid -> getMetadata (Just tid) mbHost)
    :<|> Named @"auth-req-precheck" authreqPrecheck
    :<|> Named @"auth-req" (authreq (maxttlAuthreqDiffTime opts))
    :<|> Named @"auth-resp-legacy" (authresp Nothing)
    :<|> Named @"auth-resp" (authresp . Just)
    :<|> Named @"sso-settings" ssoSettings

apiIDP ::
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member IdPRawMetadataStore r,
    Member SAMLUserStore r,
    Member (Error SparError) r
  ) =>
  ServerT APIIDP (Sem r)
apiIDP =
  Named @"idp-get" idpGet -- get, json, captures idp id
    :<|> Named @"idp-get-raw" idpGetRaw -- get, raw xml, capture idp id
    :<|> Named @"idp-get-all" idpGetAll -- get, json
    :<|> Named @"idp-create@v7" idpCreateV7
    :<|> Named @"idp-create" idpCreate -- post, created
    :<|> Named @"idp-update" idpUpdate -- put, okay
    :<|> Named @"idp-delete" idpDelete -- delete, no content

apiINTERNAL ::
  ( Member ScimTokenStore r,
    Member DefaultSsoCode r,
    Member IdPConfigStore r,
    Member (Error SparError) r,
    Member SAMLUserStore r,
    Member ScimUserTimesStore r,
    Member (Logger String) r,
    Member Random r,
    Member GalleyAccess r,
    Member BrigAccess r
  ) =>
  ServerT InternalAPI (Sem r)
apiINTERNAL =
  Named @"i_status" internalStatus
    :<|> Named @"i_delete_team" internalDeleteTeam
    :<|> Named @"i_put_sso_settings" internalPutSsoSettings
    :<|> Named @"i_post_scim_user_info" internalGetScimUserInfo
    :<|> Named @"i_get_identity_providers" idpGetAllByTeamId

appName :: Text
appName = "spar"

----------------------------------------------------------------------------
-- SSO API

-- | NB: not providing a team id here (route `Named "sso-metadata"`) is deprecated.  Some IdPs
-- do not allow setting different IdP issuer urls for different SPs, but only support one
-- global issuer url.  Adding the team id here allows us to scope this global issuer url
-- inside the team, ie. use it once per team, not once per instance.
getMetadata ::
  forall r.
  ( Member SAML2 r,
    Member SamlProtocolSettings r,
    Member (Error SparError) r
  ) =>
  Maybe TeamId ->
  Maybe Text ->
  Sem r SAML.SPMetadata
getMetadata mbTid mbHost = do
  let err :: Sem r any
      err = throwSparSem (SparSPNotFound "")

  mbHostDom <- (\host -> mkDomain host & either (const err) pure) `mapM` mbHost

  let iss :: Sem r SAML.Issuer
      iss = SamlProtocolSettings.spIssuer mbTid mbHostDom >>= maybe err pure

      rsp :: Sem r URI.URI
      rsp = SamlProtocolSettings.responseURI mbTid mbHostDom >>= maybe err pure

      contactList :: Sem r [SAML.ContactPerson]
      contactList = SamlProtocolSettings.contactPersons mbHostDom

  SAML2.meta appName iss rsp contactList

authreqPrecheck ::
  ( Member IdPConfigStore r,
    Member (Error SparError) r
  ) =>
  Maybe URI.URI ->
  Maybe URI.URI ->
  SAML.IdPId ->
  Sem r NoContent
authreqPrecheck msucc merr idpid =
  validateAuthreqParams msucc merr
    *> IdPConfigStore.getConfig idpid
      $> NoContent

authreq ::
  forall r.
  ( Member Random r,
    Member (Input Opts) r,
    Member (Logger String) r,
    Member AssIDStore r,
    Member VerdictFormatStore r,
    Member AReqIDStore r,
    Member SAML2 r,
    Member SamlProtocolSettings r,
    Member (Error SparError) r,
    Member IdPConfigStore r
  ) =>
  NominalDiffTime ->
  Maybe URI.URI ->
  Maybe URI.URI ->
  SAML.IdPId ->
  Maybe Text ->
  Sem r (SAML.FormRedirect SAML.AuthnRequest)
authreq authreqttl msucc merr idpid mbHost = do
  vformat <- validateAuthreqParams msucc merr
  form@(SAML.FormRedirect _ ((^. SAML.rqID) -> reqid)) <- do
    idp :: IdP <- IdPConfigStore.getConfig idpid

    let err :: Sem r any
        err = throwSparSem (SparSPNotFound "")
    mbHostDom <- (\host -> mkDomain host & either (const err) pure) `mapM` mbHost

    let mbtid :: Maybe TeamId
        mbtid = case fromMaybe defWireIdPAPIVersion (idp ^. SAML.idpExtraInfo . apiVersion) of
          WireIdPAPIV1 -> Nothing
          WireIdPAPIV2 -> Just $ idp ^. SAML.idpExtraInfo . team
        iss :: Sem r SAML.Issuer
        iss = SamlProtocolSettings.spIssuer mbtid mbHostDom >>= maybe err pure
    SAML2.authReq authreqttl iss idpid
  VerdictFormatStore.store authreqttl reqid vformat
  pure form

redirectURLMaxLength :: Int
redirectURLMaxLength = 140

validateAuthreqParams :: (Member (Error SparError) r) => Maybe URI.URI -> Maybe URI.URI -> Sem r VerdictFormat
validateAuthreqParams msucc merr = case (msucc, merr) of
  (Nothing, Nothing) -> pure VerdictFormatWeb
  (Just ok, Just err) -> do
    validateRedirectURL `mapM_` [ok, err]
    pure $ VerdictFormatMobile ok err
  _ -> throwSparSem $ SparBadInitiateLoginQueryParams "need-both-redirect-urls"

validateRedirectURL :: (Member (Error SparError) r) => URI.URI -> Sem r ()
validateRedirectURL uri = do
  unless ((SBS.take 4 . URI.schemeBS . URI.uriScheme $ uri) == "wire") $ do
    throwSparSem $ SparBadInitiateLoginQueryParams "invalid-schema"
  unless (SBS.length (URI.serializeURIRef' uri) <= redirectURLMaxLength) $ do
    throwSparSem $ SparBadInitiateLoginQueryParams "url-too-long"

authresp ::
  forall r.
  ( Member Random r,
    Member (Logger String) r,
    Member (Input Opts) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member AssIDStore r,
    Member VerdictFormatStore r,
    Member AReqIDStore r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member SAML2 r,
    Member SamlProtocolSettings r,
    Member (Error SparError) r,
    Member Reporter r,
    Member SAMLUserStore r
  ) =>
  Maybe TeamId ->
  SAML.AuthnResponseBody ->
  Maybe Text ->
  Sem r Void
authresp mbtid arbody mbHost = do
  let err :: Sem r any
      err = throwSparSem (SparSPNotFound "")

  mbHostDom <- (\host -> mkDomain host & either (const err) pure) `mapM` mbHost

  let iss :: Sem r SAML.Issuer
      iss = SamlProtocolSettings.spIssuer mbtid mbHostDom >>= maybe err pure

      rsp :: Sem r URI.URI
      rsp = SamlProtocolSettings.responseURI mbtid mbHostDom >>= maybe err pure

  logErrors $ SAML2.authResp mbtid iss rsp go arbody
  where
    go :: NonEmpty SAML.Assertion -> IdP -> SAML.AccessVerdict -> Sem r Void
    go _assertions idp (SAML.AccessDenied (shouldRedirectToInit -> True)) = do
      -- redirect back to idp for idp-initiated login.
      redirectToInit idp
    go assertions verdict idp = do
      -- handle the verdict
      SAML.ResponseVerdict result <- verdictHandler assertions idp verdict
      throw @SparError $ SAML.CustomServant result

    -- Whenever at least one of the denied reasons is `DeniedNoInResponseTo`, try again.
    -- there may be other reasons that will make the redirect less likely to result in a
    -- positive authentication, but better be lenient wrt what the IdP sends us here, it is
    -- about to get thrown away anyway.
    shouldRedirectToInit :: [SAML.DeniedReason] -> Bool
    shouldRedirectToInit = any (\case SAML.DeniedNoInResponseTo _ -> True; _ -> False)

    redirectToInit :: IdP -> Sem r Void
    redirectToInit idp = do
      throw (SAML.CustomError (SparRequestMissingTryIdpInitiatedLogin initiateLoginEndPointText))
      where
        -- FUTUREWORK: success_url and error_url (used to distinguish between mobile and web)
        -- are missing in this flow.  maybe the mobile browser can start app.wire.com, and
        -- app.wire.com finds the app for us?  server-side solutions probably exist, but are
        -- likely to make everything more complicated, both now and in the long run.  see
        -- `APIAuthReq` route.
        success_url = Nothing
        error_url = Nothing

        initiateLoginEndPoint :: URI
        initiateLoginEndPoint =
          ( safeLink'
              linkURI
              (Proxy @SparAPI)
              (Proxy @("sso" :> "initiate-login" :> APIAuthReq))
          )
            success_url
            error_url
            (idp ^. SAML.idpId)

        initiateLoginEndPointText :: T.Text
        initiateLoginEndPointText =
          T.pack ("/" <> uriPath initiateLoginEndPoint <> uriQuery initiateLoginEndPoint)

    logErrors :: Sem r Void -> Sem r Void
    logErrors action = catch @SparError action $ \case
      e@(SAML.CustomServant _) ->
        -- this is where the "you're ok, please come in" redirect response is coming from
        throw e
      e@(SAML.CustomError (SparRequestMissingTryIdpInitiatedLogin _)) -> do
        -- redirect to /sso/initiate-login
        throw e
      e -> do
        -- something went wrong, log and fail
        throw @SparError
          . SAML.CustomServant
          $ errorPage
            e
            (Multipart.inputs (SAML.authnResponseBodyRaw arbody))

ssoSettings :: (Member DefaultSsoCode r) => Sem r SsoSettings
ssoSettings =
  SsoSettings <$> DefaultSsoCode.get

----------------------------------------------------------------------------
-- IdPConfigStore API

idpGet ::
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member IdPConfigStore r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  SAML.IdPId ->
  Sem r IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- IdPConfigStore.getConfig idpid
  _ <- authorizeIdP zusr idp
  pure idp

idpGetRaw ::
  ( Member GalleyAccess r,
    Member BrigAccess r,
    Member IdPConfigStore r,
    Member IdPRawMetadataStore r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  SAML.IdPId ->
  Sem r RawIdPMetadata
idpGetRaw zusr idpid = do
  idp <- IdPConfigStore.getConfig idpid
  _ <- authorizeIdP zusr idp
  IdPRawMetadataStore.get idpid >>= \case
    Just txt -> pure $ RawIdPMetadata txt
    Nothing -> throwSparSem $ SparIdPNotFound (T.pack $ show idpid)

idpGetAll ::
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member IdPConfigStore r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  Sem r IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- Brig.getZUsrCheckPerm zusr ReadIdp
  idpGetAllByTeamId teamid

idpGetAllByTeamId ::
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member IdPConfigStore r,
    Member (Error SparError) r
  ) =>
  TeamId ->
  Sem r IdPList
idpGetAllByTeamId tid = do
  providers <- IdPConfigStore.getConfigsByTeam tid
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
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member SAMLUserStore r,
    Member IdPConfigStore r,
    Member IdPRawMetadataStore r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  SAML.IdPId ->
  Maybe Bool ->
  Sem r NoContent
idpDelete mbzusr idpid (fromMaybe False -> purge) = withDebugLog "idpDelete" (const Nothing) $ do
  idp <- IdPConfigStore.getConfig idpid
  (zusr, teamId) <- authorizeIdP mbzusr idp
  let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
  whenM (idpDoesAuthSelf idp zusr) $ throwSparSem SparIdPCannotDeleteOwnIdp
  SAMLUserStore.getAllByIssuerPaginated issuer >>= assertEmptyOrPurge teamId
  updateOldIssuers idp
  updateReplacingIdP idp
  -- Delete tokens associated with given IdP (we rely on the fact that
  -- each IdP has exactly one team so we can look up all tokens
  -- associated with the team and then filter them)
  tokens <- ScimTokenStore.lookupByTeam teamId
  for_ tokens $ \ScimTokenInfo {..} ->
    when (stiIdP == Just idpid) $ ScimTokenStore.delete teamId stiId
  -- Delete IdP config
  do
    IdPConfigStore.deleteConfig idp
    IdPRawMetadataStore.delete idpid
  pure NoContent
  where
    assertEmptyOrPurge :: TeamId -> Cas.Page (SAML.UserRef, UserId) -> Sem r ()
    assertEmptyOrPurge teamId page = do
      forM_ (Cas.result page) $ \(uref, uid) -> do
        mAccount <- BrigAccess.getAccount NoPendingInvitations uid
        let mUserTeam = userTeam =<< mAccount
        when (mUserTeam == Just teamId) $ do
          if purge
            then do
              SAMLUserStore.delete uid uref
              void $ BrigAccess.deleteUser uid
            else do
              throwSparSem SparIdPHasBoundUsers
      when (Cas.hasMore page) $
        SAMLUserStore.nextPage page
          >>= assertEmptyOrPurge teamId

    updateOldIssuers :: IdP -> Sem r ()
    updateOldIssuers _ = pure ()
    -- we *could* update @idp ^. SAML.idpExtraInfo . wiReplacedBy@ to not keep the idp about
    -- to be deleted in its old issuers list, but it's tricky to avoid race conditions, and
    -- there is little to be gained here: we only use old issuers to find users that have not
    -- been migrated yet, and if an old user points to a deleted idp, it just means that we
    -- won't find any users to migrate.  still, doesn't hurt mucht to look either.  so we
    -- leave old issuers dangling for now.

    updateReplacingIdP :: IdP -> Sem r ()
    updateReplacingIdP idp = forM_ (idp ^. SAML.idpExtraInfo . oldIssuers) $ \oldIssuer -> do
      iid <-
        view SAML.idpId <$> case fromMaybe defWireIdPAPIVersion $ idp ^. SAML.idpExtraInfo . apiVersion of
          WireIdPAPIV1 -> IdPConfigStore.getIdPByIssuerV1 oldIssuer
          WireIdPAPIV2 -> IdPConfigStore.getIdPByIssuerV2 oldIssuer (idp ^. SAML.idpExtraInfo . team)
      IdPConfigStore.clearReplacedBy $ Replaced iid

    idpDoesAuthSelf :: IdP -> UserId -> Sem r Bool
    idpDoesAuthSelf idp uid = do
      let idpIssuer = idp ^. SAML.idpMetadata . SAML.edIssuer
      mUserIssuer <- (>>= userIssuer) <$> getAccount NoPendingInvitations uid
      pure $ mUserIssuer == Just idpIssuer

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
--
-- The human-readable name argument `mHandle` is guaranteed to be unique for historical
-- reasons.  At some point, we wanted to use it to refer to IdPs in the backend API.  The new
-- idea is to use the IdP ID instead, and use names only for UI purposes (`ES branch` is
-- easier to remember than `6a410704-b147-11ef-9cb0-33193c475ba4`).
--
-- Related docs:
-- (on associating scim peers with idps) https://docs.wire.com/understand/single-sign-on/understand/main.html#associating-scim-tokens-with-saml-idps-for-authentication
-- (internal) https://wearezeta.atlassian.net/wiki/spaces/PAD/pages/1107001440/2024-03-27+scim+user+provisioning+and+saml2+sso+associating+scim+peers+and+saml2+idps
idpCreate ::
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member IdPRawMetadataStore r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  IdPMetadataInfo ->
  Maybe SAML.IdPId ->
  Maybe WireIdPAPIVersion ->
  Maybe (Range 1 32 Text) ->
  Sem r IdP
idpCreate zusr (IdPMetadataValue rawIdpMetadata idpmeta) mReplaces (fromMaybe defWireIdPAPIVersion -> apiversion) mHandle = withDebugLog "idpCreateXML" (Just . show . (^. SAML.idpId)) $ do
  teamid <- Brig.getZUsrCheckPerm zusr CreateUpdateDeleteIdp
  GalleyAccess.assertSSOEnabled teamid
  idp <-
    maybe (IdPConfigStore.newHandle teamid) (pure . IdPHandle . fromRange) mHandle
      >>= validateNewIdP apiversion idpmeta teamid mReplaces
  IdPRawMetadataStore.store (idp ^. SAML.idpId) rawIdpMetadata
  IdPConfigStore.insertConfig idp
  forM_ mReplaces $ \replaces ->
    IdPConfigStore.setReplacedBy (Replaced replaces) (Replacing (idp ^. SAML.idpId))
  pure idp

idpCreateV7 ::
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member IdPRawMetadataStore r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  IdPMetadataInfo ->
  Maybe SAML.IdPId ->
  Maybe WireIdPAPIVersion ->
  Maybe (Range 1 32 Text) ->
  Sem r IdP
idpCreateV7 zusr idpmeta mReplaces mApiversion mHandle = do
  teamid <- Brig.getZUsrCheckPerm zusr CreateUpdateDeleteIdp
  assertNoScimOrNoIdP teamid
  idpCreate zusr idpmeta mReplaces mApiversion mHandle
  where
    -- In teams with a scim access token, only one IdP is allowed.  The reason is that scim user
    -- data contains no information about the idp issuer, only the user name, so no valid saml
    -- credentials can be created. Only relevant for api versions 0..6.
    assertNoScimOrNoIdP ::
      ( Member ScimTokenStore r,
        Member (Error SparError) r,
        Member IdPConfigStore r
      ) =>
      TeamId ->
      Sem r ()
    assertNoScimOrNoIdP teamid = do
      numTokens <- length <$> ScimTokenStore.lookupByTeam teamid
      numIdps <- length <$> IdPConfigStore.getConfigsByTeam teamid
      when (numTokens > 0 && numIdps > 0) $
        throwSparSem $
          SparProvisioningMoreThanOneIdP ScimTokenAndSecondIdpForbidden

-- | Check that issuer is not used anywhere in the system ('WireIdPAPIV1', here it is a
-- database key for finding IdPs), or anywhere in this team ('WireIdPAPIV2'), that request
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
  ( Member Random r,
    Member (Logger String) r,
    Member IdPConfigStore r,
    Member (Error SparError) r
  ) =>
  WireIdPAPIVersion ->
  SAML.IdPMetadata ->
  TeamId ->
  Maybe SAML.IdPId ->
  IdPHandle ->
  m IdP
validateNewIdP apiversion _idpMetadata teamId mReplaces idHandle = withDebugLog "validateNewIdP" (Just . show . (^. SAML.idpId)) $ do
  _idpId <- SAML.IdPId <$> Random.uuid
  oldIssuersList :: [SAML.Issuer] <- case mReplaces of
    Nothing -> pure []
    Just replaces -> do
      idp <- IdPConfigStore.getConfig replaces
      pure $ (idp ^. SAML.idpMetadata . SAML.edIssuer) : (idp ^. SAML.idpExtraInfo . oldIssuers)
  let requri = _idpMetadata ^. SAML.edRequestURI
      _idpExtraInfo = WireIdP teamId (Just apiversion) oldIssuersList Nothing idHandle
  enforceHttps requri
  mbIdp <- case apiversion of
    WireIdPAPIV1 -> IdPConfigStore.getIdPByIssuerV1Maybe (_idpMetadata ^. SAML.edIssuer)
    WireIdPAPIV2 -> IdPConfigStore.getIdPByIssuerV2Maybe (_idpMetadata ^. SAML.edIssuer) teamId
  Logger.log Logger.Debug $ show (apiversion, _idpMetadata, teamId, mReplaces)
  Logger.log Logger.Debug $ show (_idpId, oldIssuersList, mbIdp)

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
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member IdPConfigStore r,
    Member IdPRawMetadataStore r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  IdPMetadataInfo ->
  SAML.IdPId ->
  Maybe (Range 1 32 Text) ->
  Sem r IdP
idpUpdate zusr (IdPMetadataValue raw xml) = idpUpdateXML zusr raw xml

idpUpdateXML ::
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member IdPConfigStore r,
    Member IdPRawMetadataStore r,
    Member (Error SparError) r
  ) =>
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
        Just idpHandle -> idp & (SAML.idpExtraInfo . handle) .~ IdPHandle (fromRange idpHandle)
        Nothing -> idp
  -- (if raw metadata is stored and then spar goes out, raw metadata won't match the
  -- structured idp config.  since this will lead to a 5xx response, the client is expected to
  -- try again, which would clean up cassandra state.)
  IdPConfigStore.insertConfig idp'
  -- if the IdP issuer is updated, the old issuer must be removed explicitly.
  -- if this step is ommitted (due to a crash) resending the update request should fix the inconsistent state.
  let mbteamid = case fromMaybe defWireIdPAPIVersion $ idp' ^. SAML.idpExtraInfo . apiVersion of
        WireIdPAPIV1 -> Nothing
        WireIdPAPIV2 -> Just teamid
  forM_ (idp' ^. SAML.idpExtraInfo . oldIssuers) (flip IdPConfigStore.deleteIssuer mbteamid)
  pure idp'

-- | Check that: idp id is valid; calling user is admin in that idp's home team; team id in
-- new metainfo doesn't change; new issuer (if changed) is not in use anywhere else (except as
-- an earlier IdP under the same ID); request uri is https.  Keep track of old issuer in extra
-- info if issuer has changed.
validateIdPUpdate ::
  forall m r.
  (HasCallStack, m ~ Sem r) =>
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member IdPConfigStore r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  SAML.IdPMetadata ->
  SAML.IdPId ->
  m (TeamId, IdP)
validateIdPUpdate zusr _idpMetadata _idpId = withDebugLog "validateIdPUpdate" (Just . show . (_2 %~ (^. SAML.idpId))) $ do
  previousIdP <- IdPConfigStore.getConfig _idpId
  (_, teamId) <- authorizeIdP zusr previousIdP
  unless (previousIdP ^. SAML.idpExtraInfo . team == teamId) $
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
          ( case fromMaybe defWireIdPAPIVersion $ previousIdP ^. SAML.idpExtraInfo . apiVersion of
              WireIdPAPIV1 -> IdPConfigStore.getIdPByIssuerV1Maybe newIssuer
              WireIdPAPIV2 -> IdPConfigStore.getIdPByIssuerV2Maybe newIssuer teamId
            )
            <&> ( \case
                    Just idpFound -> idpFound ^. SAML.idpId /= _idpId
                    Nothing -> False
                )
        if idpIssuerInUse
          then throwSparSem SparIdPIssuerInUse
          else
            pure $
              previousIdP ^. SAML.idpExtraInfo
                & oldIssuers
                  %~ (filterNotNewIssuer newIssuer) . nub . (previousIssuer :)

  let requri = _idpMetadata ^. SAML.edRequestURI
  enforceHttps requri
  pure (teamId, SAML.IdPConfig {..})
  where
    -- If the new issuer was previously used, it has to be removed from the list of old issuers,
    -- to prevent it from getting deleted in a later step
    filterNotNewIssuer :: SAML.Issuer -> [SAML.Issuer] -> [SAML.Issuer]
    filterNotNewIssuer newIssuer = filter (/= newIssuer)

    errUnknownIdP = SAML.UnknownIdP $ enc uri
      where
        enc =
          decodeUtf8With lenientDecode
            . toLazyByteString
            . URI.serializeURIRef
        uri = _idpMetadata ^. SAML.edIssuer . SAML.fromIssuer

withDebugLog :: (Member (Logger String) r) => String -> (a -> Maybe String) -> Sem r a -> Sem r a
withDebugLog msg showval action = do
  Logger.log Logger.Debug $ "entering " ++ msg
  val <- action
  let mshowedval = showval val
  Logger.log Logger.Debug $ "leaving " ++ msg ++ mconcat [": " ++ fromJust mshowedval | isJust mshowedval]
  pure val

authorizeIdP ::
  ( HasCallStack,
    ( Member GalleyAccess r,
      Member BrigAccess r,
      Member (Error SparError) r
    )
  ) =>
  Maybe UserId ->
  IdP ->
  Sem r (UserId, TeamId)
authorizeIdP Nothing _ =
  throw
    ( SAML.CustomError $
        SparNoPermission (T.pack $ show CreateUpdateDeleteIdp)
    )
authorizeIdP (Just zusr) idp = do
  let teamid = idp ^. SAML.idpExtraInfo . team
  GalleyAccess.assertHasPermission teamid CreateUpdateDeleteIdp zusr
  pure (zusr, teamid)

enforceHttps :: (Member (Error SparError) r) => URI.URI -> Sem r ()
enforceHttps uri =
  unless ((uri ^. URI.uriSchemeL . URI.schemeBSL) == "https") $ do
    throwSparSem . SparNewIdPWantHttps . T.fromStrict . SAML.renderURI $ uri

----------------------------------------------------------------------------
-- Internal API

internalStatus :: Sem r NoContent
internalStatus = pure NoContent

-- | Cleanup handler that is called by Galley whenever a team is about to
-- get deleted.
internalDeleteTeam ::
  ( Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member SAMLUserStore r
  ) =>
  TeamId ->
  Sem r NoContent
internalDeleteTeam teamId = do
  deleteTeam teamId
  pure NoContent

internalPutSsoSettings ::
  ( Member DefaultSsoCode r,
    Member (Error SparError) r,
    Member IdPConfigStore r
  ) =>
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

internalGetScimUserInfo :: (Member ScimUserTimesStore r) => UserId -> Sem r ScimUserInfo
internalGetScimUserInfo uid = do
  t <- fmap fst <$> ScimUserTimesStore.read uid
  pure $ ScimUserInfo uid t
