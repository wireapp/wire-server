{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Brig.API.Public
  ( servantSitemap,
    docsAPI,
    DocsAPI,
  )
where

import Brig.API.Auth
import Brig.API.Client qualified as API
import Brig.API.Connection qualified as API
import Brig.API.Error
import Brig.API.Handler
import Brig.API.MLS.KeyPackages
import Brig.API.OAuth (oauthAPI)
import Brig.API.Public.Swagger
import Brig.API.Types
import Brig.API.User qualified as API
import Brig.API.Util
import Brig.App
import Brig.Calling.API qualified as Calling
import Brig.Data.Connection qualified as Data
import Brig.Data.Nonce as Nonce
import Brig.Data.User qualified as Data
import Brig.Effects.ConnectionStore
import Brig.Effects.JwtTools (JwtTools)
import Brig.Effects.PublicKeyBundle (PublicKeyBundle)
import Brig.Effects.SFT
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Options hiding (internalEvents)
import Brig.Provider.API
import Brig.Team.API qualified as Team
import Brig.Team.Email qualified as Team
import Brig.Types.Activation (ActivationPair)
import Brig.Types.Intra (UserAccount (UserAccount, accountUser))
import Brig.User.API.Handle qualified as Handle
import Brig.User.Auth.Cookie qualified as Auth
import Cassandra qualified as C
import Cassandra qualified as Data
import Control.Error hiding (bool, note)
import Control.Lens ((.~), (?~))
import Control.Monad.Catch (throwM)
import Control.Monad.Except
import Data.Aeson hiding (json)
import Data.ByteString (fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Code qualified as Code
import Data.CommaSeparatedList
import Data.Default
import Data.Domain
import Data.FileEmbed
import Data.Handle (Handle)
import Data.Handle qualified as Handle
import Data.HavePendingInvitations
import Data.Id
import Data.Id qualified as Id
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict qualified as Map
import Data.Nonce (Nonce, randomNonce)
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Range
import Data.Schema ()
import Data.Text.Encoding qualified as Text
import Data.Time.Clock
import Data.ZAuth.Token qualified as ZAuth
import FileEmbedLzma
import Imports hiding (head)
import Network.Socket (PortNumber)
import Network.Wai.Utilities (CacheControl (..), (!>>))
import Network.Wai.Utilities qualified as Utilities
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant qualified
import Servant.OpenApi.Internal.Orphans ()
import Servant.Swagger.UI
import System.Logger.Class qualified as Log
import Util.Logging (logFunction, logHandle, logTeam, logUser)
import Wire.API.Connection qualified as Public
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig qualified as BrigFederationAPI
import Wire.API.Federation.API.Cargohold qualified as CargoholdFederationAPI
import Wire.API.Federation.API.Galley qualified as GalleyFederationAPI
import Wire.API.Federation.Error
import Wire.API.Properties qualified as Public
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Brig qualified as BrigInternalAPI
import Wire.API.Routes.Internal.Cannon qualified as CannonInternalAPI
import Wire.API.Routes.Internal.Cargohold qualified as CargoholdInternalAPI
import Wire.API.Routes.Internal.Galley qualified as GalleyInternalAPI
import Wire.API.Routes.Internal.Gundeck qualified as GundeckInternalAPI
import Wire.API.Routes.Internal.Spar qualified as SparInternalAPI
import Wire.API.Routes.MultiTablePaging qualified as Public
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Brig
import Wire.API.Routes.Public.Brig.OAuth
import Wire.API.Routes.Public.Cannon
import Wire.API.Routes.Public.Cargohold
import Wire.API.Routes.Public.Galley
import Wire.API.Routes.Public.Gundeck
import Wire.API.Routes.Public.Proxy
import Wire.API.Routes.Public.Spar
import Wire.API.Routes.Public.Util
import Wire.API.Routes.Version
import Wire.API.SwaggerHelper (cleanupSwagger)
import Wire.API.SystemSettings
import Wire.API.Team qualified as Public
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.Team.Member (HiddenPerm (..), hasPermission)
import Wire.API.User (RegisterError (RegisterErrorAllowlistError))
import Wire.API.User qualified as Public
import Wire.API.User.Activation qualified as Public
import Wire.API.User.Auth qualified as Public
import Wire.API.User.Client qualified as Public
import Wire.API.User.Client.DPoPAccessToken
import Wire.API.User.Client.Prekey qualified as Public
import Wire.API.User.Handle qualified as Public
import Wire.API.User.Password qualified as Public
import Wire.API.User.RichInfo qualified as Public
import Wire.API.User.Search qualified as Public
import Wire.API.UserMap qualified as Public
import Wire.API.Wrapped qualified as Public
import Wire.AuthenticationSubsystem (AuthenticationSubsystem, createPasswordResetCode, resetPassword)
import Wire.BlockListStore (BlockListStore)
import Wire.DeleteQueue
import Wire.EmailSending (EmailSending)
import Wire.EmailSubsystem
import Wire.EmailSubsystem.Template
import Wire.Error
import Wire.Events (Events)
import Wire.FederationConfigStore (FederationConfigStore)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.IndexedUserStore (IndexedUserStore)
import Wire.InvitationCodeStore
import Wire.NotificationSubsystem
import Wire.PasswordResetCodeStore (PasswordResetCodeStore)
import Wire.PasswordStore (PasswordStore, lookupHashedPassword)
import Wire.PropertySubsystem
import Wire.Sem.Concurrency
import Wire.Sem.Jwk (Jwk)
import Wire.Sem.Now (Now)
import Wire.Sem.Paging.Cassandra
import Wire.TeamInvitationSubsystem
import Wire.UserKeyStore
import Wire.UserSearch.Types
import Wire.UserStore (UserStore)
import Wire.UserSubsystem hiding (checkHandle, checkHandles)
import Wire.UserSubsystem qualified as User
import Wire.UserSubsystem.Error
import Wire.VerificationCode
import Wire.VerificationCodeGen
import Wire.VerificationCodeSubsystem

-- User API -----------------------------------------------------------

docsAPI :: Servant.Server DocsAPI
docsAPI =
  versionedSwaggerDocsAPI
    :<|> pure eventNotificationSchemas
    :<|> internalEndpointsSwaggerDocsAPIs
    :<|> federatedEndpointsSwaggerDocsAPIs

federatedEndpointsSwaggerDocsAPIs :: Servant.Server FederationSwaggerDocsAPI
federatedEndpointsSwaggerDocsAPIs =
  swaggerSchemaUIServer (adjustSwaggerForFederationEndpoints "brig" BrigFederationAPI.swaggerDoc)
    :<|> swaggerSchemaUIServer (adjustSwaggerForFederationEndpoints "galley" GalleyFederationAPI.swaggerDoc)
    :<|> swaggerSchemaUIServer (adjustSwaggerForFederationEndpoints "cargohold" CargoholdFederationAPI.swaggerDoc)

internalEndpointsSwaggerDocsAPIs :: Servant.Server InternalEndpointsSwaggerDocsAPI
internalEndpointsSwaggerDocsAPIs =
  internalEndpointsSwaggerDocsAPI @"brig" "brig" 9082 BrigInternalAPI.swaggerDoc
    :<|> internalEndpointsSwaggerDocsAPI @"cannon" "cannon" 9093 CannonInternalAPI.swaggerDoc
    :<|> internalEndpointsSwaggerDocsAPI @"cargohold" "cargohold" 9094 CargoholdInternalAPI.swaggerDoc
    :<|> internalEndpointsSwaggerDocsAPI @"galley" "galley" 9095 GalleyInternalAPI.swaggerDoc
    :<|> internalEndpointsSwaggerDocsAPI @"spar" "spar" 9098 SparInternalAPI.swaggerDoc
    :<|> internalEndpointsSwaggerDocsAPI @"gundeck" "gundeck" 9096 GundeckInternalAPI.swaggerDoc

-- | Serves Swagger docs for public endpoints
--
-- Dual to `internalEndpointsSwaggerDocsAPI`.
versionedSwaggerDocsAPI :: Servant.Server VersionedSwaggerDocsAPI
versionedSwaggerDocsAPI (Just (VersionNumber V7)) =
  swaggerSchemaUIServer $
    ( serviceSwagger @VersionAPITag @'V7
        <> serviceSwagger @BrigAPITag @'V7
        <> serviceSwagger @GalleyAPITag @'V7
        <> serviceSwagger @SparAPITag @'V7
        <> serviceSwagger @CargoholdAPITag @'V7
        <> serviceSwagger @CannonAPITag @'V7
        <> serviceSwagger @GundeckAPITag @'V7
        <> serviceSwagger @ProxyAPITag @'V7
        <> serviceSwagger @OAuthAPITag @'V7
    )
      & S.info . S.title .~ "Wire-Server API"
      & S.info . S.description ?~ $(embedText =<< makeRelativeToProject "docs/swagger.md")
      & S.servers .~ [S.Server ("/" <> toUrlPiece V7) Nothing mempty]
      & cleanupSwagger
versionedSwaggerDocsAPI (Just (VersionNumber V6)) = swaggerPregenUIServer $(pregenSwagger V6)
versionedSwaggerDocsAPI (Just (VersionNumber V5)) = swaggerPregenUIServer $(pregenSwagger V5)
versionedSwaggerDocsAPI (Just (VersionNumber V4)) = swaggerPregenUIServer $(pregenSwagger V4)
versionedSwaggerDocsAPI (Just (VersionNumber V3)) = swaggerPregenUIServer $(pregenSwagger V3)
versionedSwaggerDocsAPI (Just (VersionNumber V2)) = swaggerPregenUIServer $(pregenSwagger V2)
versionedSwaggerDocsAPI (Just (VersionNumber V1)) = swaggerPregenUIServer $(pregenSwagger V1)
versionedSwaggerDocsAPI (Just (VersionNumber V0)) = swaggerPregenUIServer $(pregenSwagger V0)
versionedSwaggerDocsAPI Nothing = allroutes (throwError listAllVersionsResp)
  where
    allroutes ::
      (forall a. Servant.Handler a) ->
      Servant.Server (SwaggerSchemaUI "swagger-ui" "swagger.json")
    allroutes action =
      -- why?  see 'SwaggerSchemaUI' type.
      action :<|> action :<|> action :<|> error (UTF8.toString . toStrict $ listAllVersionsHTML)

    listAllVersionsResp :: ServerError
    listAllVersionsResp = ServerError 200 mempty listAllVersionsHTML [("Content-Type", "text/html;charset=utf-8")]

    listAllVersionsHTML :: LByteString
    listAllVersionsHTML =
      "<html><head></head><body><h2>please pick an api version</h2>"
        <> mconcat
          [ let url = "/" <> toQueryParam v <> "/api/swagger-ui/"
             in "<a href=\""
                  <> (fromStrict . Text.encodeUtf8 $ url)
                  <> "\">"
                  <> (fromStrict . Text.encodeUtf8 $ url)
                  <> "</a><br>"
            | v <- [minBound :: Version ..]
          ]
        <> "</body>"

-- | Serves Swagger docs for internal endpoints.
internalEndpointsSwaggerDocsAPI ::
  forall service.
  String ->
  PortNumber ->
  S.OpenApi ->
  Servant.Server (VersionedSwaggerDocsAPIBase service)
internalEndpointsSwaggerDocsAPI _ _ _ (Just _) = emptySwagger
internalEndpointsSwaggerDocsAPI service examplePort swagger Nothing =
  swaggerSchemaUIServer $
    swagger
      & adjustSwaggerForInternalEndpoint service examplePort
      & cleanupSwagger

servantSitemap ::
  forall r p.
  ( Member (Embed HttpClientIO) r,
    Member (Embed IO) r,
    Member (Error UserSubsystemError) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (UserPendingActivationStore p) r,
    Member AuthenticationSubsystem r,
    Member DeleteQueue r,
    Member EmailSending r,
    Member EmailSubsystem r,
    Member Events r,
    Member FederationConfigStore r,
    Member GalleyAPIAccess r,
    Member InvitationCodeStore r,
    Member Jwk r,
    Member JwtTools r,
    Member NotificationSubsystem r,
    Member Now r,
    Member PasswordResetCodeStore r,
    Member PasswordStore r,
    Member PropertySubsystem r,
    Member PublicKeyBundle r,
    Member SFT r,
    Member TinyLog r,
    Member UserKeyStore r,
    Member UserStore r,
    Member (Input TeamTemplates) r,
    Member UserSubsystem r,
    Member TeamInvitationSubsystem r,
    Member VerificationCodeSubsystem r,
    Member (Concurrency 'Unsafe) r,
    Member BlockListStore r,
    Member (ConnectionStore InternalPaging) r,
    Member IndexedUserStore r
  ) =>
  ServerT BrigAPI (Handler r)
servantSitemap =
  userAPI
    :<|> selfAPI
    :<|> accountAPI
    :<|> clientAPI
    :<|> prekeyAPI
    :<|> userClientAPI
    :<|> connectionAPI
    :<|> propertiesAPI
    :<|> mlsAPI
    :<|> userHandleAPI
    :<|> searchAPI
    :<|> authAPI
    :<|> callingAPI
    :<|> Team.servantAPI
    :<|> systemSettingsAPI
    :<|> oauthAPI
    :<|> botAPI
    :<|> servicesAPI
    :<|> providerAPI
  where
    userAPI :: ServerT UserAPI (Handler r)
    userAPI =
      Named @"get-user-unqualified" (callsFed (exposeAnnotations getUserUnqualifiedH))
        :<|> Named @"get-user-qualified" (callsFed (exposeAnnotations getUserProfileH))
        :<|> Named @"update-user-email" updateUserEmail
        :<|> Named @"get-handle-info-unqualified" (callsFed (exposeAnnotations getHandleInfoUnqualifiedH))
        :<|> Named @"get-user-by-handle-qualified" (callsFed (exposeAnnotations Handle.getHandleInfo))
        :<|> Named @"list-users-by-unqualified-ids-or-handles" (callsFed (exposeAnnotations listUsersByUnqualifiedIdsOrHandles))
        :<|> Named @"list-users-by-ids-or-handles" (callsFed (exposeAnnotations listUsersByIdsOrHandles))
        :<|> Named @"list-users-by-ids-or-handles@V3" (callsFed (exposeAnnotations listUsersByIdsOrHandlesV3))
        :<|> Named @"send-verification-code" sendVerificationCode
        :<|> Named @"get-rich-info" getRichInfo
        :<|> Named @"get-supported-protocols" getSupportedProtocols

    selfAPI :: ServerT SelfAPI (Handler r)
    selfAPI =
      Named @"get-self" getSelf
        :<|> Named @"delete-self" (callsFed (exposeAnnotations deleteSelfUser))
        :<|> Named @"put-self" (callsFed (exposeAnnotations updateUser))
        :<|> Named @"change-phone" changePhone
        :<|> Named @"remove-phone" (callsFed (exposeAnnotations removePhone))
        :<|> Named @"remove-email" (callsFed (exposeAnnotations removeEmail))
        :<|> Named @"check-password-exists" checkPasswordExists
        :<|> Named @"change-password" changePassword
        :<|> Named @"change-locale" (callsFed (exposeAnnotations changeLocale))
        :<|> Named @"change-handle" (callsFed (exposeAnnotations changeHandle))
        :<|> Named @"change-supported-protocols" changeSupportedProtocols

    accountAPI :: ServerT AccountAPI (Handler r)
    accountAPI =
      Named @"upgrade-personal-to-team" upgradePersonalToTeam
        :<|> Named @"register" (callsFed (exposeAnnotations createUser))
        :<|> Named @"verify-delete" (callsFed (exposeAnnotations verifyDeleteUser))
        :<|> Named @"get-activate" (callsFed (exposeAnnotations activate))
        :<|> Named @"post-activate" (callsFed (exposeAnnotations activateKey))
        :<|> Named @"post-activate-send" sendActivationCode
        :<|> Named @"post-password-reset" beginPasswordReset
        :<|> Named @"post-password-reset-complete" completePasswordReset
        :<|> Named @"post-password-reset-key-deprecated" deprecatedCompletePasswordReset
        :<|> Named @"onboarding" deprecatedOnboarding

    clientAPI :: ServerT ClientAPI (Handler r)
    clientAPI =
      Named @"get-user-clients-unqualified" (callsFed (exposeAnnotations getUserClientsUnqualified))
        :<|> Named @"get-user-clients-qualified" (callsFed (exposeAnnotations getUserClientsQualified))
        :<|> Named @"get-user-client-unqualified" (callsFed (exposeAnnotations getUserClientUnqualified))
        :<|> Named @"get-user-client-qualified" (callsFed (exposeAnnotations getUserClientQualified))
        :<|> Named @"list-clients-bulk" (callsFed (exposeAnnotations listClientsBulk))
        :<|> Named @"list-clients-bulk-v2" (callsFed (exposeAnnotations listClientsBulkV2))
        :<|> Named @"list-clients-bulk@v2" (callsFed (exposeAnnotations listClientsBulkV2))

    prekeyAPI :: ServerT PrekeyAPI (Handler r)
    prekeyAPI =
      Named @"get-users-prekeys-client-unqualified" (callsFed (exposeAnnotations getPrekeyUnqualifiedH))
        :<|> Named @"get-users-prekeys-client-qualified" (callsFed (exposeAnnotations getPrekeyH))
        :<|> Named @"get-users-prekey-bundle-unqualified" (callsFed (exposeAnnotations getPrekeyBundleUnqualifiedH))
        :<|> Named @"get-users-prekey-bundle-qualified" (callsFed (exposeAnnotations getPrekeyBundleH))
        :<|> Named @"get-multi-user-prekey-bundle-unqualified" getMultiUserPrekeyBundleUnqualifiedH
        :<|> Named @"get-multi-user-prekey-bundle-qualified@v3" (callsFed (exposeAnnotations getMultiUserPrekeyBundleHV3))
        :<|> Named @"get-multi-user-prekey-bundle-qualified" (callsFed (exposeAnnotations getMultiUserPrekeyBundleH))

    userClientAPI :: ServerT UserClientAPI (Handler r)
    userClientAPI =
      Named @"add-client-v6" (callsFed (exposeAnnotations addClient))
        :<|> Named @"add-client" (callsFed (exposeAnnotations addClient))
        :<|> Named @"update-client" updateClient
        :<|> Named @"delete-client" deleteClient
        :<|> Named @"list-clients-v6" listClients
        :<|> Named @"list-clients" listClients
        :<|> Named @"get-client-v6" getClient
        :<|> Named @"get-client" getClient
        :<|> Named @"get-client-capabilities" getClientCapabilities
        :<|> Named @"get-client-prekeys" getClientPrekeys
        :<|> Named @"head-nonce" newNonce
        :<|> Named @"get-nonce" newNonce
        :<|> Named @"create-access-token" (createAccessToken @UserClientAPI @CreateAccessToken POST)

    connectionAPI :: ServerT ConnectionAPI (Handler r)
    connectionAPI =
      Named @"create-connection-unqualified" (callsFed (exposeAnnotations createConnectionUnqualified))
        :<|> Named @"create-connection" (callsFed (exposeAnnotations createConnection))
        :<|> Named @"list-local-connections" listLocalConnections
        :<|> Named @"list-connections" listConnections
        :<|> Named @"get-connection-unqualified" getLocalConnection
        :<|> Named @"get-connection" getConnection
        :<|> Named @"update-connection-unqualified" (callsFed (exposeAnnotations updateLocalConnection))
        :<|> Named @"update-connection" (callsFed (exposeAnnotations updateConnection))
        :<|> Named @"search-contacts" (callsFed (exposeAnnotations searchUsersHandler))

    propertiesAPI :: ServerT PropertiesAPI (Handler r)
    propertiesAPI =
      ( Named @"set-property" setPropertyH
          :<|> Named @"delete-property" deletePropertyH
          :<|> Named @"clear-properties" clearPropertiesH
          :<|> Named @"get-property" getPropertyH
          :<|> Named @"list-property-keys" listPropertyKeysH
      )
        :<|> Named @"list-properties" listPropertyKeysAndValuesH

    mlsAPI :: ServerT MLSAPI (Handler r)
    mlsAPI =
      Named @"mls-key-packages-upload" uploadKeyPackages
        :<|> Named @"mls-key-packages-replace" replaceKeyPackages
        :<|> Named @"mls-key-packages-claim" claimKeyPackages
        :<|> Named @"mls-key-packages-count" countKeyPackages
        :<|> Named @"mls-key-packages-delete" deleteKeyPackages

    userHandleAPI :: ServerT UserHandleAPI (Handler r)
    userHandleAPI =
      Named @"check-user-handles" checkHandles
        :<|> Named @"check-user-handle" checkHandle

    searchAPI :: ServerT SearchAPI (Handler r)
    searchAPI =
      Named @"browse-team" browseTeamHandler

    authAPI :: ServerT AuthAPI (Handler r)
    authAPI =
      Named @"access" (callsFed (exposeAnnotations accessH))
        :<|> Named @"send-login-code" sendLoginCode
        :<|> Named @"login" (callsFed (exposeAnnotations login))
        :<|> Named @"logout" logoutH
        :<|> Named @"change-self-email" changeSelfEmailH
        :<|> Named @"list-cookies" listCookies
        :<|> Named @"remove-cookies" removeCookies

    callingAPI :: ServerT CallingAPI (Handler r)
    callingAPI =
      Named @"get-calls-config" Calling.getCallsConfig
        :<|> Named @"get-calls-config-v2" Calling.getCallsConfigV2

    systemSettingsAPI :: ServerT SystemSettingsAPI (Handler r)
    systemSettingsAPI =
      Named @"get-system-settings-unauthorized" getSystemSettings
        :<|> Named @"get-system-settings" getSystemSettingsInternal

-- Note [ephemeral user sideeffect]
-- If the user is ephemeral and expired, it will be removed upon calling
-- CheckUserExists[Un]Qualified, see 'Brig.API.User.userGC'.
-- This leads to the following events being sent:
-- - UserDeleted event to contacts of the user
-- - MemberLeave event to members for all conversations the user was in (via galley)

---------------------------------------------------------------------------
-- Handlers

browseTeamHandler ::
  (Member UserSubsystem r) =>
  UserId ->
  TeamId ->
  Maybe Text ->
  Maybe Public.RoleFilter ->
  Maybe Public.TeamUserSearchSortBy ->
  Maybe Public.TeamUserSearchSortOrder ->
  Maybe (Range 1 500 Int) ->
  Maybe Public.PagingState ->
  Handler r (Public.SearchResult Public.TeamContact)
browseTeamHandler uid tid mQuery mRoleFilter mTeamUserSearchSortBy mTeamUserSearchSortOrder mMaxResults mPagingState = do
  let browseTeamFilters = BrowseTeamFilters tid mQuery mRoleFilter mTeamUserSearchSortBy mTeamUserSearchSortOrder
  lift . liftSem $ User.browseTeam uid browseTeamFilters mMaxResults mPagingState

setPropertyH :: (Member PropertySubsystem r) => UserId -> ConnId -> Public.PropertyKey -> Public.RawPropertyValue -> Handler r ()
setPropertyH u c key raw = lift . liftSem $ setProperty u c key raw

deletePropertyH :: (Member PropertySubsystem r) => UserId -> ConnId -> Public.PropertyKey -> Handler r ()
deletePropertyH u c k = lift . liftSem $ deleteProperty u c k

clearPropertiesH :: (Member PropertySubsystem r) => UserId -> ConnId -> Handler r ()
clearPropertiesH u c = lift . liftSem $ clearProperties u c

getPropertyH :: (Member PropertySubsystem r) => UserId -> Public.PropertyKey -> Handler r (Maybe Public.RawPropertyValue)
getPropertyH u k = lift . liftSem $ lookupProperty u k

listPropertyKeysH :: (Member PropertySubsystem r) => UserId -> Handler r [Public.PropertyKey]
listPropertyKeysH u = lift . liftSem $ getPropertyKeys u

listPropertyKeysAndValuesH :: (Member PropertySubsystem r) => UserId -> Handler r Public.PropertyKeysAndValues
listPropertyKeysAndValuesH u = lift . liftSem $ getAllProperties u

getPrekeyUnqualifiedH ::
  (Member DeleteQueue r) =>
  UserId ->
  UserId ->
  ClientId ->
  (Handler r) Public.ClientPrekey
getPrekeyUnqualifiedH zusr user client = do
  domain <- viewFederationDomain
  getPrekeyH zusr (Qualified user domain) client

getPrekeyH ::
  (Member DeleteQueue r) =>
  UserId ->
  Qualified UserId ->
  ClientId ->
  (Handler r) Public.ClientPrekey
getPrekeyH zusr (Qualified user domain) client = do
  mPrekey <- API.claimPrekey (ProtectedUser zusr) user domain client !>> clientError
  ifNothing (notFound "prekey not found") mPrekey

getPrekeyBundleUnqualifiedH :: UserId -> UserId -> (Handler r) Public.PrekeyBundle
getPrekeyBundleUnqualifiedH zusr uid = do
  domain <- viewFederationDomain
  API.claimPrekeyBundle (ProtectedUser zusr) domain uid !>> clientError

getPrekeyBundleH :: UserId -> Qualified UserId -> (Handler r) Public.PrekeyBundle
getPrekeyBundleH zusr (Qualified uid domain) =
  API.claimPrekeyBundle (ProtectedUser zusr) domain uid !>> clientError

getMultiUserPrekeyBundleUnqualifiedH ::
  ( Member (Concurrency 'Unsafe) r,
    Member DeleteQueue r
  ) =>
  UserId ->
  Public.UserClients ->
  Handler r Public.UserClientPrekeyMap
getMultiUserPrekeyBundleUnqualifiedH zusr userClients = do
  maxSize <- fromIntegral <$> asks (.settings.maxConvSize)
  when (Map.size (Public.userClients userClients) > maxSize) $
    throwStd (errorToWai @'E.TooManyClients)
  API.claimLocalMultiPrekeyBundles (ProtectedUser zusr) userClients !>> clientError

getMultiUserPrekeyBundleHInternal ::
  (MonadReader Env m, MonadError HttpError m) =>
  Public.QualifiedUserClients ->
  m ()
getMultiUserPrekeyBundleHInternal qualUserClients = do
  maxSize <- fromIntegral <$> asks (.settings.maxConvSize)
  let Sum (size :: Int) =
        Map.foldMapWithKey
          (\_ v -> Sum . Map.size $ v)
          (Public.qualifiedUserClients qualUserClients)
  when (size > maxSize) $
    throwStd (errorToWai @'E.TooManyClients)

getMultiUserPrekeyBundleHV3 ::
  ( Member (Concurrency 'Unsafe) r,
    Member DeleteQueue r
  ) =>
  UserId ->
  Public.QualifiedUserClients ->
  (Handler r) Public.QualifiedUserClientPrekeyMap
getMultiUserPrekeyBundleHV3 zusr qualUserClients = do
  getMultiUserPrekeyBundleHInternal qualUserClients
  API.claimMultiPrekeyBundlesV3 (ProtectedUser zusr) qualUserClients !>> clientError

getMultiUserPrekeyBundleH ::
  ( Member (Concurrency 'Unsafe) r,
    Member DeleteQueue r
  ) =>
  UserId ->
  Public.QualifiedUserClients ->
  (Handler r) Public.QualifiedUserClientPrekeyMapV4
getMultiUserPrekeyBundleH zusr qualUserClients = do
  getMultiUserPrekeyBundleHInternal qualUserClients
  API.claimMultiPrekeyBundles (ProtectedUser zusr) qualUserClients !>> clientError

addClient ::
  ( Member GalleyAPIAccess r,
    Member DeleteQueue r,
    Member NotificationSubsystem r,
    Member EmailSubsystem r,
    Member VerificationCodeSubsystem r,
    Member Events r,
    Member UserSubsystem r
  ) =>
  Local UserId ->
  ConnId ->
  Public.NewClient ->
  Handler r Public.Client
addClient lusr con new = do
  -- Users can't add legal hold clients
  when (Public.newClientType new == Public.LegalHoldClientType) $
    throwE (clientError ClientLegalHoldCannotBeAdded)
  API.addClient lusr (Just con) new
    !>> clientError

deleteClient ::
  (Member DeleteQueue r) =>
  UserId ->
  ConnId ->
  ClientId ->
  Public.RmClient ->
  (Handler r) ()
deleteClient usr con clt body =
  API.rmClient usr con clt (Public.rmPassword body) !>> clientError

updateClient :: UserId -> ClientId -> Public.UpdateClient -> (Handler r) ()
updateClient usr clt upd = wrapClientE (API.updateClient usr clt upd) !>> clientError

listClients :: UserId -> (Handler r) [Public.Client]
listClients zusr =
  lift $ API.lookupLocalClients zusr

getClient :: UserId -> ClientId -> (Handler r) (Maybe Public.Client)
getClient zusr clientId = lift $ API.lookupLocalClient zusr clientId

getUserClientsUnqualified :: UserId -> (Handler r) [Public.PubClient]
getUserClientsUnqualified uid = do
  localdomain <- viewFederationDomain
  API.lookupPubClients (Qualified uid localdomain) !>> clientError

getUserClientsQualified :: Qualified UserId -> (Handler r) [Public.PubClient]
getUserClientsQualified quid = API.lookupPubClients quid !>> clientError

getUserClientUnqualified :: UserId -> ClientId -> (Handler r) Public.PubClient
getUserClientUnqualified uid cid = do
  localdomain <- viewFederationDomain
  x <- API.lookupPubClient (Qualified uid localdomain) cid !>> clientError
  ifNothing (notFound "client not found") x

listClientsBulk :: UserId -> Range 1 MaxUsersForListClientsBulk [Qualified UserId] -> (Handler r) (Public.QualifiedUserMap (Set Public.PubClient))
listClientsBulk _zusr limitedUids =
  API.lookupPubClientsBulk (fromRange limitedUids) !>> clientError

listClientsBulkV2 :: UserId -> Public.LimitedQualifiedUserIdList MaxUsersForListClientsBulk -> (Handler r) (Public.WrappedQualifiedUserMap (Set Public.PubClient))
listClientsBulkV2 zusr userIds = Public.Wrapped <$> listClientsBulk zusr (Public.qualifiedUsers userIds)

getUserClientQualified :: Qualified UserId -> ClientId -> (Handler r) Public.PubClient
getUserClientQualified quid cid = do
  x <- API.lookupPubClient quid cid !>> clientError
  ifNothing (notFound "client not found") x

getClientCapabilities :: UserId -> ClientId -> (Handler r) Public.ClientCapabilityList
getClientCapabilities uid cid = do
  mclient <- lift (API.lookupLocalClient uid cid)
  maybe (throwStd (errorToWai @'E.ClientNotFound)) (pure . Public.clientCapabilities) mclient

getRichInfo :: (Member UserSubsystem r) => Local UserId -> UserId -> Handler r Public.RichInfoAssocList
getRichInfo lself user = do
  let luser = qualifyAs lself user
  -- Check that both users exist and the requesting user is allowed to see rich info of the
  -- other user
  let fetch luid =
        ifNothing (errorToWai @'E.UserNotFound)
          =<< lift (liftSem $ (.accountUser) <$$> User.getLocalAccountBy NoPendingInvitations luid)
  selfUser <- fetch lself
  otherUser <- fetch luser
  case (Public.userTeam selfUser, Public.userTeam otherUser) of
    (Just t1, Just t2) | t1 == t2 -> pure ()
    _ -> throwStd insufficientTeamPermissions
  -- Query rich info
  wrapClientE $ fromMaybe mempty <$> API.lookupRichInfo (tUnqualified luser)

getSupportedProtocols ::
  (Member UserSubsystem r) =>
  Local UserId ->
  Qualified UserId ->
  Handler r (Set Public.BaseProtocolTag)
getSupportedProtocols lself quid = do
  muser <- (lift . liftSem $ getUserProfile lself quid) !>> fedError
  user <- maybe (throwStd (errorToWai @'E.UserNotFound)) pure muser
  pure (Public.profileSupportedProtocols user)

getClientPrekeys :: UserId -> ClientId -> (Handler r) [Public.PrekeyId]
getClientPrekeys usr clt = lift (wrapClient $ API.lookupPrekeyIds usr clt)

newNonce :: UserId -> ClientId -> (Handler r) (Nonce, CacheControl)
newNonce uid cid = do
  ttl <- nonceTtlSecs <$> asks (.settings)
  nonce <- randomNonce
  lift $ wrapClient $ Nonce.insertNonce ttl uid (Id.clientToText cid) nonce
  pure (nonce, NoStore)

createAccessToken ::
  forall api endpoint r.
  ( Member JwtTools r,
    Member Now r,
    Member PublicKeyBundle r,
    IsElem endpoint api,
    HasLink endpoint,
    MkLink endpoint Link ~ (ClientId -> Link)
  ) =>
  StdMethod ->
  Local UserId ->
  ClientId ->
  Proof ->
  (Handler r) (DPoPAccessTokenResponse, CacheControl)
createAccessToken method luid cid proof = do
  let link = safeLink (Proxy @api) (Proxy @endpoint) cid
  API.createAccessToken luid cid method link proof !>> certEnrollmentError

upgradePersonalToTeam ::
  ( Member (ConnectionStore InternalPaging) r,
    Member (Embed HttpClientIO) r,
    Member EmailSubsystem r,
    Member GalleyAPIAccess r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member UserStore r
  ) =>
  Local UserId ->
  Public.BindingNewTeamUser ->
  Handler r (Either Public.UpgradePersonalToTeamError Public.CreateUserTeam)
upgradePersonalToTeam luid bNewTeam =
  lift . runExceptT $
    API.upgradePersonalToTeam luid bNewTeam

-- | docs/reference/user/registration.md {#RefRegistration}
createUser ::
  ( Member BlockListStore r,
    Member GalleyAPIAccess r,
    Member InvitationCodeStore r,
    Member (UserPendingActivationStore p) r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member UserKeyStore r,
    Member EmailSubsystem r,
    Member Events r,
    Member UserSubsystem r,
    Member PasswordResetCodeStore r,
    Member EmailSending r
  ) =>
  Public.NewUserPublic ->
  Handler r (Either Public.RegisterError Public.RegisterSuccess)
createUser (Public.NewUserPublic new) = lift . runExceptT $ do
  API.checkRestrictedUserCreation new
  for_ (Public.newUserEmail new) $
    mapExceptT wrapHttp . checkAllowlistWithError RegisterErrorAllowlistError

  result <- API.createUser new
  let acc = createdAccount result

  let eac = createdEmailActivation result
  let epair = (,) <$> (activationKey <$> eac) <*> (activationCode <$> eac)
  let newUserLabel = Public.newUserLabel new
  let newUserTeam = Public.newUserTeam new
  let usr = accountUser acc

  let context =
        let invitationCode = case Public.newUserTeam new of
              (Just (Public.NewTeamMember code)) -> Just code
              _ -> Nothing
         in ( logFunction "Brig.API.Public.createUser"
                . logUser (Public.userId usr)
                . maybe id logHandle (Public.userHandle usr)
                . maybe id logTeam (Public.userTeam usr)
                . maybe id logEmail (Public.userEmail usr)
                . maybe id logInvitationCode invitationCode
            )
  lift . Log.info $ context . Log.msg @Text "Sucessfully created user"

  let Public.User {userLocale, userDisplayName} = usr
      userEmail = Public.userEmail usr
      userId = Public.userId usr
  lift $ do
    for_ (liftM2 (,) userEmail epair) $ \(e, p) ->
      sendActivationEmail e userDisplayName p (Just userLocale) newUserTeam
    for_ (liftM3 (,,) userEmail (createdUserTeam result) newUserTeam) $ \(e, ct, ut) ->
      sendWelcomeEmail e ct ut (Just userLocale)
  cok <-
    Auth.toWebCookie =<< case acc of
      UserAccount _ Public.Ephemeral ->
        lift . wrapHttpClient $
          Auth.newCookie @ZAuth.User userId Nothing Public.SessionCookie newUserLabel
      UserAccount _ _ ->
        lift . wrapHttpClient $
          Auth.newCookie @ZAuth.User userId Nothing Public.PersistentCookie newUserLabel
  -- pure $ CreateUserResponse cok userId (Public.SelfProfile usr)
  pure $ Public.RegisterSuccess cok (Public.SelfProfile usr)
  where
    sendActivationEmail :: (Member EmailSubsystem r) => Public.EmailAddress -> Public.Name -> ActivationPair -> Maybe Public.Locale -> Maybe Public.NewTeamUser -> (AppT r) ()
    sendActivationEmail email name (key, code) locale mTeamUser
      | Just teamUser <- mTeamUser,
        Public.NewTeamCreator creator <- teamUser,
        let Public.BindingNewTeamUser team _ = creator =
          liftSem $ sendTeamActivationMail email name key code locale (fromRange $ team.newTeamName)
      | otherwise =
          liftSem $ sendActivationMail email name key code locale

    sendWelcomeEmail :: (Member EmailSending r) => Public.EmailAddress -> Public.CreateUserTeam -> Public.NewTeamUser -> Maybe Public.Locale -> (AppT r) ()
    -- NOTE: Welcome e-mails for the team creator are not dealt by brig anymore
    sendWelcomeEmail e (Public.CreateUserTeam t n) newUser l = case newUser of
      Public.NewTeamCreator _ ->
        pure ()
      Public.NewTeamMember _ ->
        Team.sendMemberWelcomeMail e t n l
      Public.NewTeamMemberSSO _ ->
        Team.sendMemberWelcomeMail e t n l

getSelf :: (Member UserSubsystem r) => Local UserId -> Handler r Public.SelfProfile
getSelf self =
  lift (liftSem (getSelfProfile self))
    >>= ifNothing (errorToWai @'E.UserNotFound)

getUserProfileH ::
  (Member UserSubsystem r) =>
  Local UserId ->
  Qualified UserId ->
  (Handler r) (Maybe Public.UserProfile)
getUserProfileH u us = (lift . liftSem) $ getUserProfile u us

getUserUnqualifiedH ::
  (Member UserSubsystem r) =>
  Local UserId ->
  UserId ->
  (Handler r) (Maybe Public.UserProfile)
getUserUnqualifiedH self uid = do
  let domain = tDomain self
  lift . liftSem $ getUserProfile self (Qualified uid domain)

-- FUTUREWORK: Make servant understand that at least one of these is required
listUsersByUnqualifiedIdsOrHandles ::
  (Member UserSubsystem r, Member UserStore r) =>
  UserId ->
  Maybe (CommaSeparatedList UserId) ->
  Maybe (Range 1 4 (CommaSeparatedList Handle)) ->
  (Handler r) [Public.UserProfile]
listUsersByUnqualifiedIdsOrHandles self mUids mHandles = do
  domain <- viewFederationDomain
  case (mUids, mHandles) of
    (Just uids, _) -> listUsersByIdsOrHandlesV3 self (Public.ListUsersByIds ((`Qualified` domain) <$> fromCommaSeparatedList uids))
    (_, Just handles) ->
      let normalRangedList = fromCommaSeparatedList $ fromRange handles
          qualifiedList = (`Qualified` domain) <$> normalRangedList
          -- Use of unsafeRange here is ok only because we know that 'handles'
          -- is valid for 'Range 1 4'. However, we must not forget to keep this
          -- annotation here otherwise a change in 'Public.ListUsersByHandles'
          -- could cause this code to break.
          qualifiedRangedList :: Range 1 4 [Qualified Handle] = unsafeRange qualifiedList
       in listUsersByIdsOrHandlesV3 self (Public.ListUsersByHandles qualifiedRangedList)
    (Nothing, Nothing) -> throwStd $ badRequest "at least one ids or handles must be provided"

listUsersByIdsOrHandlesGetIds ::
  (Member UserStore r) =>
  [Handle] ->
  Handler r [Qualified UserId]
listUsersByIdsOrHandlesGetIds localHandles = do
  localUsers <- catMaybes <$> traverse (lift . liftSem . API.lookupHandle) localHandles
  domain <- viewFederationDomain
  pure $ map (`Qualified` domain) localUsers

listUsersByIdsOrHandlesGetUsers ::
  (Member UserStore r) =>
  Local x ->
  Range n m [Qualified Handle] ->
  Handler r [Qualified UserId]
listUsersByIdsOrHandlesGetUsers lself hs = do
  let (localHandles, _) = partitionQualified lself (fromRange hs)
  listUsersByIdsOrHandlesGetIds localHandles

listUsersByIdsOrHandlesV3 ::
  forall r.
  (Member UserSubsystem r, Member UserStore r) =>
  UserId ->
  Public.ListUsersQuery ->
  (Handler r) [Public.UserProfile]
listUsersByIdsOrHandlesV3 self q = do
  lself <- qualifyLocal self
  foundUsers <- case q of
    Public.ListUsersByIds us ->
      byIds lself us
    Public.ListUsersByHandles hs -> do
      us <- listUsersByIdsOrHandlesGetUsers lself hs
      Handle.filterHandleResults lself =<< byIds lself us
  case foundUsers of
    [] -> throwStd $ notFound "None of the specified ids or handles match any users"
    _ -> pure foundUsers
  where
    byIds :: Local UserId -> [Qualified UserId] -> (Handler r) [Public.UserProfile]
    byIds lself uids = (lift . liftSem $ getUserProfiles lself uids) !>> fedError

-- Similar to listUsersByIdsOrHandlesV3, except that it allows partial successes
-- using a new return type
listUsersByIdsOrHandles ::
  forall r.
  (Member UserSubsystem r, Member UserStore r) =>
  UserId ->
  Public.ListUsersQuery ->
  Handler r ListUsersById
listUsersByIdsOrHandles self q = do
  lself <- qualifyLocal self
  (errors, foundUsers) <- case q of
    Public.ListUsersByIds us ->
      byIds lself us
    Public.ListUsersByHandles hs -> do
      us <- listUsersByIdsOrHandlesGetUsers lself hs
      (l, r) <- byIds lself us
      r' <- Handle.filterHandleResults lself r
      pure (l, r')
  pure $ ListUsersById foundUsers $ fst <$$> nonEmpty errors
  where
    byIds ::
      Local UserId ->
      [Qualified UserId] ->
      Handler r ([(Qualified UserId, FederationError)], [Public.UserProfile])
    byIds lself uids = lift (liftSem (getUserProfilesWithErrors lself uids))

newtype GetActivationCodeResp
  = GetActivationCodeResp (Public.ActivationKey, Public.ActivationCode)

instance ToJSON GetActivationCodeResp where
  toJSON (GetActivationCodeResp (k, c)) = object ["key" .= k, "code" .= c]

updateUser ::
  (Member UserSubsystem r) =>
  Local UserId ->
  ConnId ->
  Public.UserUpdate ->
  Handler r ()
updateUser uid conn uu = do
  let update =
        def
          { name = uu.uupName,
            pict = uu.uupPict,
            textStatus = uu.uupTextStatus,
            assets = uu.uupAssets,
            accentId = uu.uupAccentId
          }
  lift . liftSem $
    updateUserProfile uid (Just conn) UpdateOriginWireClient update

-- | Phone based functionality is not supported any more, but the handler is
-- kept here so long as client API version 5 is supported.
changePhone ::
  UserId ->
  ConnId ->
  Public.PhoneUpdate ->
  (Handler r) (Maybe Public.ChangePhoneError)
changePhone _ _ _ = pure . Just $ Public.InvalidNewPhone

removePhone :: UserId -> Handler r (Maybe Public.RemoveIdentityError)
removePhone _ = (lift . pure) Nothing

removeEmail ::
  ( Member UserKeyStore r,
    Member UserSubsystem r,
    Member Events r
  ) =>
  UserId ->
  Handler r (Maybe Public.RemoveIdentityError)
removeEmail self = lift . exceptTToMaybe $ API.removeEmail self

checkPasswordExists :: (Member PasswordStore r) => UserId -> (Handler r) Bool
checkPasswordExists = fmap isJust . lift . liftSem . lookupHashedPassword

changePassword :: (Member PasswordStore r, Member UserStore r) => UserId -> Public.PasswordChange -> (Handler r) (Maybe Public.ChangePasswordError)
changePassword u cp = lift . exceptTToMaybe $ API.changePassword u cp

changeLocale ::
  (Member UserSubsystem r) =>
  Local UserId ->
  ConnId ->
  Public.LocaleUpdate ->
  (Handler r) ()
changeLocale lusr conn l =
  lift . liftSem $
    updateUserProfile
      lusr
      (Just conn)
      User.UpdateOriginWireClient
      def {locale = Just l.luLocale}

changeSupportedProtocols ::
  (Member UserSubsystem r) =>
  Local UserId ->
  ConnId ->
  Public.SupportedProtocolUpdate ->
  Handler r ()
changeSupportedProtocols u conn (Public.SupportedProtocolUpdate prots) =
  lift . liftSem $ User.updateUserProfile u (Just conn) UpdateOriginWireClient upd
  where
    upd = def {supportedProtocols = Just prots}

-- | (zusr is ignored by this handler, ie. checking handles is allowed as long as you have
-- *any* account.)
checkHandle :: (Member UserSubsystem r) => UserId -> Text -> Handler r ()
checkHandle _uid hndl =
  lift (liftSem $ User.checkHandle hndl) >>= \case
    API.CheckHandleFound -> pure ()
    API.CheckHandleNotFound -> throwStd (errorToWai @'E.HandleNotFound)

-- | (zusr is ignored by this handler, ie. checking handles is allowed as long as you have
-- *any* account.)
checkHandles :: (Member UserSubsystem r) => UserId -> Public.CheckHandles -> Handler r [Handle]
checkHandles _ (Public.CheckHandles hs num) = do
  let handles = mapMaybe Handle.parseHandle (fromRange hs)
  lift $ liftSem $ API.checkHandles handles (fromRange num)

-- | This endpoint returns UserHandleInfo instead of UserProfile for backwards
-- compatibility, whereas the corresponding qualified endpoint (implemented by
-- 'Handle.getHandleInfo') returns UserProfile to reduce traffic between backends
-- in a federated scenario.
getHandleInfoUnqualifiedH ::
  ( Member UserSubsystem r,
    Member UserStore r
  ) =>
  UserId ->
  Handle ->
  (Handler r) (Maybe Public.UserHandleInfo)
getHandleInfoUnqualifiedH self handle = do
  domain <- viewFederationDomain
  Public.UserHandleInfo . Public.profileQualifiedId
    <$$> Handle.getHandleInfo self (Qualified handle domain)

changeHandle :: (Member UserSubsystem r) => Local UserId -> ConnId -> Public.HandleUpdate -> Handler r ()
changeHandle u conn (Public.HandleUpdate h) = lift $ liftSem do
  User.updateHandle u (Just conn) UpdateOriginWireClient h

beginPasswordReset ::
  (Member AuthenticationSubsystem r) =>
  Public.NewPasswordReset ->
  Handler r ()
beginPasswordReset Public.NewPasswordResetUnsupportedPhone =
  throwStd (errorToWai @'E.InvalidPhone)
beginPasswordReset (Public.NewPasswordReset target) =
  lift (liftSem $ createPasswordResetCode $ mkEmailKey target)

completePasswordReset ::
  ( Member AuthenticationSubsystem r
  ) =>
  Public.CompletePasswordReset ->
  Handler r ()
completePasswordReset req = do
  lift . liftSem $
    resetPassword
      (Public.cpwrIdent req)
      (Public.cpwrCode req)
      (Public.cpwrPassword req)

-- docs/reference/user/activation.md {#RefActivationRequest}
-- docs/reference/user/registration.md {#RefRegistration}
sendActivationCode ::
  ( Member BlockListStore r,
    Member EmailSubsystem r,
    Member GalleyAPIAccess r,
    Member UserKeyStore r
  ) =>
  Public.SendActivationCode ->
  Handler r ()
sendActivationCode ac = do
  let email = ac.emailKey
  customerExtensionCheckBlockedDomains email
  checkAllowlist email
  API.sendActivationCode email (ac.locale) !>> sendActCodeError

searchUsersHandler ::
  (Member UserSubsystem r) =>
  Local UserId ->
  Text ->
  Maybe Domain ->
  Maybe (Range 1 500 Int32) ->
  Handler r (Public.SearchResult Public.Contact)
searchUsersHandler luid term mDomain mMaxResults =
  lift . liftSem $ User.searchUsers luid term mDomain mMaxResults

-- | If the user presents an email address from a blocked domain, throw an error.
--
-- The tautological constraint in the type signature is added so that once we remove the
-- feature, ghc will guide us here.
customerExtensionCheckBlockedDomains :: Public.EmailAddress -> (Handler r) ()
customerExtensionCheckBlockedDomains email = do
  mBlockedDomains <- fmap (.domainsBlockedForRegistration) <$> asks (.settings.customerExtensions)
  for_ mBlockedDomains $ \(DomainsBlockedForRegistration blockedDomains) -> do
    case mkDomain (Text.decodeUtf8 $ Public.domainPart email) of
      Left _ ->
        pure () -- if it doesn't fit the syntax of blocked domains, it is not blocked
      Right domain ->
        when (domain `elem` blockedDomains) $
          throwM $
            customerExtensionBlockedDomain domain

createConnectionUnqualified ::
  ( Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member UserStore r,
    Member UserSubsystem r,
    Member (Embed HttpClientIO) r
  ) =>
  UserId ->
  ConnId ->
  Public.ConnectionRequest ->
  Handler r (ResponseForExistedCreated Public.UserConnection)
createConnectionUnqualified self conn cr = do
  lself <- qualifyLocal self
  target <- qualifyLocal (Public.crUser cr)
  API.createConnectionToLocalUser lself conn target !>> connError

createConnection ::
  ( Member FederationConfigStore r,
    Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member UserStore r,
    Member UserSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r
  ) =>
  UserId ->
  ConnId ->
  Qualified UserId ->
  Handler r (ResponseForExistedCreated Public.UserConnection)
createConnection self conn target = do
  lself <- qualifyLocal self
  API.createConnection lself conn target !>> connError

updateLocalConnection ::
  ( Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r
  ) =>
  UserId ->
  ConnId ->
  UserId ->
  Public.ConnectionUpdate ->
  Handler r (UpdateResult Public.UserConnection)
updateLocalConnection self conn other (Public.cuStatus -> newStatus) = do
  lself <- qualifyLocal self
  lother <- qualifyLocal other
  mkUpdateResult
    <$> API.updateConnectionToLocalUser lself lother newStatus (Just conn) !>> connError

updateConnection ::
  ( Member FederationConfigStore r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  ConnId ->
  Qualified UserId ->
  Public.ConnectionUpdate ->
  Handler r (UpdateResult Public.UserConnection)
updateConnection self conn other (Public.cuStatus -> newStatus) = do
  lself <- qualifyLocal self
  mkUpdateResult
    <$> API.updateConnection lself other newStatus (Just conn) !>> connError

listLocalConnections :: UserId -> Maybe UserId -> Maybe (Range 1 500 Int32) -> (Handler r) Public.UserConnectionList
listLocalConnections uid start msize = do
  let defaultSize = toRange (Proxy @100)
  lift $ API.lookupConnections uid start (fromMaybe defaultSize msize)

-- | Lists connection IDs for the logged in user in a paginated way.
--
-- Pagination requires an order, in this case the order is defined as:
--
-- - First all the local connections are listed ordered by their id
--
-- - After local connections, remote connections are listed ordered
-- - lexicographically by their domain and then by their id.
listConnections :: UserId -> Public.ListConnectionsRequestPaginated -> (Handler r) Public.ConnectionsPage
listConnections uid Public.GetMultiTablePageRequest {..} = do
  self <- qualifyLocal uid
  case gmtprState of
    Just (Public.ConnectionPagingState Public.PagingRemotes stateBS) -> remotesOnly self (mkState <$> stateBS) (fromRange gmtprSize)
    _ -> localsAndRemotes self (fmap mkState . Public.mtpsState =<< gmtprState) gmtprSize
  where
    pageToConnectionsPage :: Public.LocalOrRemoteTable -> Data.PageWithState Public.UserConnection -> Public.ConnectionsPage
    pageToConnectionsPage table page@Data.PageWithState {..} =
      Public.MultiTablePage
        { mtpResults = pwsResults,
          mtpHasMore = C.pwsHasMore page,
          -- FUTUREWORK confusingly, using 'ConversationPagingState' instead of 'ConnectionPagingState' doesn't fail any tests.
          -- Is this type actually useless? Or the tests not good enough?
          mtpPagingState = Public.ConnectionPagingState table (LBS.toStrict . C.unPagingState <$> pwsState)
        }

    mkState :: ByteString -> C.PagingState
    mkState = C.PagingState . LBS.fromStrict

    localsAndRemotes :: Local UserId -> Maybe C.PagingState -> Range 1 500 Int32 -> (Handler r) Public.ConnectionsPage
    localsAndRemotes self pagingState size = do
      localPage <- lift $ pageToConnectionsPage Public.PagingLocals <$> wrapClient (Data.lookupLocalConnectionsPage self pagingState (rcast size))
      let remainingSize = fromRange size - fromIntegral (length (Public.mtpResults localPage))
      if Public.mtpHasMore localPage || remainingSize <= 0
        then pure localPage {Public.mtpHasMore = True} -- We haven't checked the remotes yet, so has_more must always be True here.
        else do
          remotePage <- remotesOnly self Nothing remainingSize
          pure remotePage {Public.mtpResults = Public.mtpResults localPage <> Public.mtpResults remotePage}

    remotesOnly :: Local UserId -> Maybe C.PagingState -> Int32 -> (Handler r) Public.ConnectionsPage
    remotesOnly self pagingState size =
      lift . wrapClient $
        pageToConnectionsPage Public.PagingRemotes <$> Data.lookupRemoteConnectionsPage self pagingState size

getLocalConnection :: UserId -> UserId -> (Handler r) (Maybe Public.UserConnection)
getLocalConnection self other = do
  lother <- qualifyLocal other
  getConnection self (tUntagged lother)

getConnection :: UserId -> Qualified UserId -> (Handler r) (Maybe Public.UserConnection)
getConnection self other = do
  lself <- qualifyLocal self
  lift . wrapClient $ Data.lookupConnection lself other

deleteSelfUser ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member UserKeyStore r,
    Member NotificationSubsystem r,
    Member UserStore r,
    Member PasswordStore r,
    Member EmailSubsystem r,
    Member UserSubsystem r,
    Member VerificationCodeSubsystem r,
    Member PropertySubsystem r,
    Member Events r
  ) =>
  Local UserId ->
  Public.DeleteUser ->
  (Handler r) (Maybe Code.Timeout)
deleteSelfUser lu body = do
  API.deleteSelfUser lu (Public.deleteUserPassword body) !>> deleteUserError

verifyDeleteUser ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member UserStore r,
    Member TinyLog r,
    Member UserKeyStore r,
    Member VerificationCodeSubsystem r,
    Member PropertySubsystem r,
    Member UserSubsystem r,
    Member Events r
  ) =>
  Public.VerifyDeleteUser ->
  Handler r ()
verifyDeleteUser body = API.verifyDeleteUser body !>> deleteUserError

updateUserEmail ::
  forall r.
  ( Member BlockListStore r,
    Member UserKeyStore r,
    Member GalleyAPIAccess r,
    Member EmailSubsystem r,
    Member UserSubsystem r
  ) =>
  UserId ->
  UserId ->
  Public.EmailUpdate ->
  (Handler r) ()
updateUserEmail zuserId emailOwnerId (Public.EmailUpdate email) = do
  maybeZuserTeamId <- lift $ wrapClient $ Data.lookupUserTeam zuserId
  whenM (not <$> assertHasPerm maybeZuserTeamId) $ throwStd insufficientTeamPermissions
  maybeEmailOwnerTeamId <- lift $ wrapClient $ Data.lookupUserTeam emailOwnerId
  checkSameTeam maybeZuserTeamId maybeEmailOwnerTeamId
  void $ API.changeSelfEmail emailOwnerId email UpdateOriginWireClient
  where
    checkSameTeam :: Maybe TeamId -> Maybe TeamId -> (Handler r) ()
    checkSameTeam (Just zuserTeamId) maybeEmailOwnerTeamId =
      when (Just zuserTeamId /= maybeEmailOwnerTeamId) $ throwStd $ notFound "user not found"
    checkSameTeam Nothing _ = throwStd insufficientTeamPermissions

    assertHasPerm :: Maybe TeamId -> (Handler r) Bool
    assertHasPerm maybeTeamId = fromMaybe False <$> check
      where
        check = runMaybeT $ do
          teamId <- hoistMaybe maybeTeamId
          teamMember <- MaybeT $ lift $ liftSem $ GalleyAPIAccess.getTeamMember zuserId teamId
          pure $ teamMember `hasPermission` ChangeTeamMemberProfiles

-- activation

activate ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member Events r,
    Member PasswordResetCodeStore r
  ) =>
  Public.ActivationKey ->
  Public.ActivationCode ->
  (Handler r) ActivationRespWithStatus
activate k c = do
  let activationRequest = Public.Activate (Public.ActivateKey k) c False
  activateKey activationRequest

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKey ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member Events r,
    Member UserSubsystem r,
    Member PasswordResetCodeStore r
  ) =>
  Public.Activate ->
  (Handler r) ActivationRespWithStatus
activateKey (Public.Activate tgt code dryrun)
  | dryrun = do
      wrapClientE (API.preverify tgt code) !>> actError
      pure ActivationRespDryRun
  | otherwise = do
      result <- API.activate tgt code Nothing !>> actError
      pure $ case result of
        ActivationSuccess ident x -> respond ident x
        ActivationPass -> ActivationRespPass
  where
    respond (Just ident) x = ActivationResp $ Public.ActivationResponse ident x
    respond Nothing _ = ActivationRespSuccessNoIdent

sendVerificationCode ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member (Input (Local ())) r,
    Member EmailSubsystem r,
    Member UserSubsystem r,
    Member VerificationCodeSubsystem r
  ) =>
  Public.SendVerificationCode ->
  (Handler r) ()
sendVerificationCode req = do
  let email = Public.svcEmail req
  let action = Public.svcAction req
  mbAccount <- getAccount email
  featureEnabled <- getFeatureStatus mbAccount
  case (mbAccount, featureEnabled) of
    (Just account, True) -> do
      let gen = mk6DigitVerificationCodeGen email
      timeout <- verificationTimeout <$> asks (.settings)
      code <-
        lift . liftSem $
          createCodeOverwritePrevious
            gen
            (scopeFromAction action)
            (Retries 3)
            timeout
            (Just $ toUUID $ Public.userId $ accountUser account)
      sendMail email code.codeValue (Just $ Public.userLocale $ accountUser account) action
    _ -> pure ()
  where
    getAccount :: Public.EmailAddress -> (Handler r) (Maybe UserAccount)
    getAccount email = lift . liftSem $ do
      mbUserId <- lookupKey $ mkEmailKey email
      mbLUserId <- qualifyLocal' `traverse` mbUserId
      join <$> User.getAccountNoFilter `traverse` mbLUserId

    sendMail :: Public.EmailAddress -> Code.Value -> Maybe Public.Locale -> Public.VerificationAction -> (Handler r) ()
    sendMail email value mbLocale =
      lift . liftSem . \case
        Public.CreateScimToken -> sendCreateScimTokenVerificationMail email value mbLocale
        Public.Login -> sendLoginVerificationMail email value mbLocale
        Public.DeleteTeam -> sendTeamDeletionVerificationMail email value mbLocale

    getFeatureStatus :: Maybe UserAccount -> (Handler r) Bool
    getFeatureStatus mbAccount = do
      mbStatusEnabled <- lift $ liftSem $ GalleyAPIAccess.getVerificationCodeEnabled `traverse` (Public.userTeam <$> accountUser =<< mbAccount)
      pure $ fromMaybe False mbStatusEnabled

getSystemSettings :: (Handler r) SystemSettingsPublic
getSystemSettings = do
  optSettings <- asks (.settings)
  pure $
    SystemSettingsPublic $
      fromMaybe False optSettings.restrictUserCreation

getSystemSettingsInternal :: UserId -> (Handler r) SystemSettings
getSystemSettingsInternal _ = do
  optSettings <- asks (.settings)
  let pSettings = SystemSettingsPublic $ fromMaybe False optSettings.restrictUserCreation
  let iSettings = SystemSettingsInternal $ fromMaybe False optSettings.enableMLS
  pure $ SystemSettings pSettings iSettings

-- Deprecated

deprecatedOnboarding :: UserId -> JsonValue -> (Handler r) DeprecatedMatchingResult
deprecatedOnboarding _ _ = pure DeprecatedMatchingResult

deprecatedCompletePasswordReset ::
  ( Member AuthenticationSubsystem r
  ) =>
  Public.PasswordResetKey ->
  Public.PasswordReset ->
  (Handler r) ()
deprecatedCompletePasswordReset k pwr = do
  lift . liftSem $
    resetPassword
      (Public.PasswordResetIdentityKey k)
      (Public.pwrCode pwr)
      (Public.pwrPassword pwr)

-- Utilities

ifNothing :: Utilities.Error -> Maybe a -> (Handler r) a
ifNothing e = maybe (throwStd e) pure
