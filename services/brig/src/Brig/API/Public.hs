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
import Brig.API.Properties qualified as API
import Brig.API.Public.Swagger
import Brig.API.Types
import Brig.API.User qualified as API
import Brig.API.Util
import Brig.API.Util qualified as API
import Brig.App
import Brig.Calling.API qualified as Calling
import Brig.Code qualified as Code
import Brig.Data.Connection qualified as Data
import Brig.Data.Nonce as Nonce
import Brig.Data.User qualified as Data
import Brig.Data.UserKey qualified as UserKey
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.CodeStore (CodeStore)
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.FederationConfigStore (FederationConfigStore)
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.GalleyProvider qualified as GalleyProvider
import Brig.Effects.JwtTools (JwtTools)
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.PublicKeyBundle (PublicKeyBundle)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Options hiding (internalEvents, sesQueue)
import Brig.Provider.API
import Brig.Team.API qualified as Team
import Brig.Team.Email qualified as Team
import Brig.Types.Activation (ActivationPair)
import Brig.Types.Intra (UserAccount (UserAccount, accountUser))
import Brig.Types.User (HavePendingInvitations (..))
import Brig.User.API.Handle qualified as Handle
import Brig.User.API.Search (teamUserSearch)
import Brig.User.API.Search qualified as Search
import Brig.User.Auth.Cookie qualified as Auth
import Brig.User.Email
import Brig.User.Phone
import Cassandra qualified as C
import Cassandra qualified as Data
import Control.Error hiding (bool)
import Control.Lens (view, (.~), (?~), (^.))
import Control.Monad.Catch (throwM)
import Control.Monad.Except
import Data.Aeson hiding (json)
import Data.Bifunctor
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.CommaSeparatedList
import Data.Domain
import Data.FileEmbed
import Data.Handle (Handle, parseHandle)
import Data.Id
import Data.Id qualified as Id
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict qualified as Map
import Data.Nonce (Nonce, randomNonce)
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Range
import Data.Schema ()
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy (pack)
import Data.Time.Clock (UTCTime)
import Data.ZAuth.Token qualified as ZAuth
import FileEmbedLzma
import Galley.Types.Teams (HiddenPerm (..), hasPermission)
import Imports hiding (head)
import Network.Socket (PortNumber)
import Network.Wai.Utilities as Utilities
import Polysemy
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
import Wire.API.UserMap qualified as Public
import Wire.API.Wrapped qualified as Public
import Wire.NotificationSubsystem
import Wire.Sem.Concurrency
import Wire.Sem.Jwk (Jwk)
import Wire.Sem.Now (Now)
import Wire.Sem.Paging.Cassandra (InternalPaging)

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

-- | Serves Swagger docs for public endpoints
--
-- Dual to `internalEndpointsSwaggerDocsAPI`.
versionedSwaggerDocsAPI :: Servant.Server VersionedSwaggerDocsAPI
versionedSwaggerDocsAPI (Just (VersionNumber V6)) =
  swaggerSchemaUIServer $
    ( serviceSwagger @VersionAPITag @'V6
        <> serviceSwagger @BrigAPITag @'V6
        <> serviceSwagger @GalleyAPITag @'V6
        <> serviceSwagger @SparAPITag @'V6
        <> serviceSwagger @CargoholdAPITag @'V6
        <> serviceSwagger @CannonAPITag @'V6
        <> serviceSwagger @GundeckAPITag @'V6
        <> serviceSwagger @ProxyAPITag @'V6
        <> serviceSwagger @OAuthAPITag @'V6
    )
      & S.info . S.title .~ "Wire-Server API"
      & S.info . S.description ?~ $(embedText =<< makeRelativeToProject "docs/swagger.md")
      & S.servers .~ [S.Server ("/" <> toUrlPiece V6) Nothing mempty]
      & cleanupSwagger
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
      action :<|> action :<|> action :<|> error (cs listAllVersionsHTML)

    listAllVersionsResp :: ServerError
    listAllVersionsResp = ServerError 200 mempty listAllVersionsHTML [("Content-Type", "text/html;charset=utf-8")]

    listAllVersionsHTML :: LByteString
    listAllVersionsHTML =
      "<html><head></head><body><h2>please pick an api version</h2>"
        <> mconcat
          [ let url = "/" <> toQueryParam v <> "/api/swagger-ui/"
             in "<a href=\"" <> cs url <> "\">" <> cs url <> "</a><br>"
            | v <- [minBound :: Version ..]
          ]
        <> "</body>"

-- | Serves Swagger docs for internal endpoints
--
-- Dual to `versionedSwaggerDocsAPI`. Swagger docs for old versions are (almost)
-- empty. It would have been too tedious to create them. Please add
-- pre-generated docs on version increase as it's done in
-- `versionedSwaggerDocsAPI`.
--
-- If you're having issues with this function not typechecking when it should,
-- be sure to supply the type argument explicitly
internalEndpointsSwaggerDocsAPI ::
  forall service.
  String ->
  PortNumber ->
  S.OpenApi ->
  Servant.Server (VersionedSwaggerDocsAPIBase service)
internalEndpointsSwaggerDocsAPI service examplePort swagger (Just (VersionNumber V6)) =
  swaggerSchemaUIServer $
    swagger
      & adjustSwaggerForInternalEndpoint service examplePort
      & cleanupSwagger
internalEndpointsSwaggerDocsAPI service examplePort swagger (Just (VersionNumber V5)) =
  swaggerSchemaUIServer $
    swagger
      & adjustSwaggerForInternalEndpoint service examplePort
      & cleanupSwagger
internalEndpointsSwaggerDocsAPI _ _ _ (Just (VersionNumber V4)) = emptySwagger
internalEndpointsSwaggerDocsAPI _ _ _ (Just (VersionNumber V3)) = emptySwagger
internalEndpointsSwaggerDocsAPI _ _ _ (Just (VersionNumber V2)) = emptySwagger
internalEndpointsSwaggerDocsAPI _ _ _ (Just (VersionNumber V1)) = emptySwagger
internalEndpointsSwaggerDocsAPI _ _ _ (Just (VersionNumber V0)) = emptySwagger
internalEndpointsSwaggerDocsAPI service examplePort swagger Nothing =
  internalEndpointsSwaggerDocsAPI service examplePort swagger (Just maxBound)

servantSitemap ::
  forall r p.
  ( Member BlacklistPhonePrefixStore r,
    Member BlacklistStore r,
    Member CodeStore r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyProvider r,
    Member JwtTools r,
    Member Now r,
    Member PasswordResetStore r,
    Member PublicKeyBundle r,
    Member (UserPendingActivationStore p) r,
    Member Jwk r,
    Member FederationConfigStore r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
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
        :<|> Named @"get-user-qualified" (callsFed (exposeAnnotations getUser))
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
      Named @"register" (callsFed (exposeAnnotations createUser))
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
      Named @"add-client" (callsFed (exposeAnnotations addClient))
        :<|> Named @"update-client" updateClient
        :<|> Named @"delete-client" deleteClient
        :<|> Named @"list-clients" listClients
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
        :<|> Named @"search-contacts" (callsFed (exposeAnnotations Search.search))

    propertiesAPI :: ServerT PropertiesAPI (Handler r)
    propertiesAPI =
      ( Named @"set-property" setProperty
          :<|> Named @"delete-property" deleteProperty
          :<|> Named @"clear-properties" clearProperties
          :<|> Named @"get-property" getProperty
          :<|> Named @"list-property-keys" listPropertyKeys
      )
        :<|> Named @"list-properties" listPropertyKeysAndValues

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
      Named @"browse-team" teamUserSearch

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

setProperty :: (Member NotificationSubsystem r) => UserId -> ConnId -> Public.PropertyKey -> Public.RawPropertyValue -> Handler r ()
setProperty u c key raw = do
  checkPropertyKey key
  val <- safeParsePropertyValue raw
  API.setProperty u c key val !>> propDataError

checkPropertyKey :: Public.PropertyKey -> Handler r ()
checkPropertyKey k = do
  maxKeyLen <- fromMaybe defMaxKeyLen <$> view (settings . propertyMaxKeyLen)
  let keyText = Ascii.toText (Public.propertyKeyName k)
  when (Text.compareLength keyText (fromIntegral maxKeyLen) == GT) $
    throwStd propertyKeyTooLarge

-- | Parse a 'PropertyValue' from a bytestring.  This is different from 'FromJSON' in that
-- checks the byte size of the input, and fails *without consuming all of it* if that size
-- exceeds the settings.
safeParsePropertyValue :: Public.RawPropertyValue -> Handler r Public.PropertyValue
safeParsePropertyValue raw = do
  maxValueLen <- fromMaybe defMaxValueLen <$> view (settings . propertyMaxValueLen)
  let lbs = Lazy.take (maxValueLen + 1) (Public.rawPropertyBytes raw)
  unless (Lazy.length lbs <= maxValueLen) $
    throwStd propertyValueTooLarge
  hoistEither $ first (StdError . badRequest . pack) (propertyValueFromRaw raw)

propertyValueFromRaw :: Public.RawPropertyValue -> Either String Public.PropertyValue
propertyValueFromRaw raw =
  Public.PropertyValue raw
    <$> eitherDecode (Public.rawPropertyBytes raw)

parseStoredPropertyValue :: Public.RawPropertyValue -> Handler r Public.PropertyValue
parseStoredPropertyValue raw = case propertyValueFromRaw raw of
  Right value -> pure value
  Left e -> do
    Log.err $
      Log.msg (Log.val "Failed to parse a stored property value")
        . Log.field "raw_value" (Public.rawPropertyBytes raw)
        . Log.field "parse_error" e
    throwStd internalServerError

deleteProperty :: (Member NotificationSubsystem r) => UserId -> ConnId -> Public.PropertyKey -> Handler r ()
deleteProperty u c k = lift (API.deleteProperty u c k)

clearProperties :: (Member NotificationSubsystem r) => UserId -> ConnId -> Handler r ()
clearProperties u c = lift (API.clearProperties u c)

getProperty :: UserId -> Public.PropertyKey -> Handler r (Maybe Public.RawPropertyValue)
getProperty u k = lift . wrapClient $ API.lookupProperty u k

listPropertyKeys :: UserId -> Handler r [Public.PropertyKey]
listPropertyKeys u = lift $ wrapClient (API.lookupPropertyKeys u)

listPropertyKeysAndValues :: UserId -> Handler r Public.PropertyKeysAndValues
listPropertyKeysAndValues u = do
  keysAndVals <- fmap Map.fromList . lift $ wrapClient (API.lookupPropertyKeysAndValues u)
  Public.PropertyKeysAndValues <$> traverse parseStoredPropertyValue keysAndVals

getPrekeyUnqualifiedH :: UserId -> UserId -> ClientId -> (Handler r) Public.ClientPrekey
getPrekeyUnqualifiedH zusr user client = do
  domain <- viewFederationDomain
  getPrekeyH zusr (Qualified user domain) client

getPrekeyH :: UserId -> Qualified UserId -> ClientId -> (Handler r) Public.ClientPrekey
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
  Member (Concurrency 'Unsafe) r =>
  UserId ->
  Public.UserClients ->
  Handler r Public.UserClientPrekeyMap
getMultiUserPrekeyBundleUnqualifiedH zusr userClients = do
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  when (Map.size (Public.userClients userClients) > maxSize) $
    throwStd (errorToWai @'E.TooManyClients)
  API.claimLocalMultiPrekeyBundles (ProtectedUser zusr) userClients !>> clientError

getMultiUserPrekeyBundleHInternal ::
  (MonadReader Env m, MonadError Brig.API.Error.Error m) =>
  Public.QualifiedUserClients ->
  m ()
getMultiUserPrekeyBundleHInternal qualUserClients = do
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  let Sum (size :: Int) =
        Map.foldMapWithKey
          (\_ v -> Sum . Map.size $ v)
          (Public.qualifiedUserClients qualUserClients)
  when (size > maxSize) $
    throwStd (errorToWai @'E.TooManyClients)

getMultiUserPrekeyBundleHV3 ::
  Member (Concurrency 'Unsafe) r =>
  UserId ->
  Public.QualifiedUserClients ->
  (Handler r) Public.QualifiedUserClientPrekeyMap
getMultiUserPrekeyBundleHV3 zusr qualUserClients = do
  getMultiUserPrekeyBundleHInternal qualUserClients
  API.claimMultiPrekeyBundlesV3 (ProtectedUser zusr) qualUserClients !>> clientError

getMultiUserPrekeyBundleH ::
  Member (Concurrency 'Unsafe) r =>
  UserId ->
  Public.QualifiedUserClients ->
  (Handler r) Public.QualifiedUserClientPrekeyMapV4
getMultiUserPrekeyBundleH zusr qualUserClients = do
  getMultiUserPrekeyBundleHInternal qualUserClients
  API.claimMultiPrekeyBundles (ProtectedUser zusr) qualUserClients !>> clientError

addClient ::
  ( Member GalleyProvider r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  ConnId ->
  Public.NewClient ->
  (Handler r) NewClientResponse
addClient usr con new = do
  -- Users can't add legal hold clients
  when (Public.newClientType new == Public.LegalHoldClientType) $
    throwE (clientError ClientLegalHoldCannotBeAdded)
  clientResponse
    <$> API.addClient usr (Just con) new
      !>> clientError
  where
    clientResponse :: Public.Client -> NewClientResponse
    clientResponse client = Servant.addHeader (Public.clientId client) client

deleteClient :: UserId -> ConnId -> ClientId -> Public.RmClient -> (Handler r) ()
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

getRichInfo :: UserId -> UserId -> Handler r Public.RichInfoAssocList
getRichInfo self user = do
  -- Check that both users exist and the requesting user is allowed to see rich info of the
  -- other user
  selfUser <-
    ifNothing (errorToWai @'E.UserNotFound)
      =<< lift (wrapClient $ Data.lookupUser NoPendingInvitations self)
  otherUser <-
    ifNothing (errorToWai @'E.UserNotFound)
      =<< lift (wrapClient $ Data.lookupUser NoPendingInvitations user)
  case (Public.userTeam selfUser, Public.userTeam otherUser) of
    (Just t1, Just t2) | t1 == t2 -> pure ()
    _ -> throwStd insufficientTeamPermissions
  -- Query rich info
  wrapClientE $ fromMaybe mempty <$> API.lookupRichInfo user

getSupportedProtocols ::
  Member GalleyProvider r =>
  Local UserId ->
  Qualified UserId ->
  Handler r (Set Public.BaseProtocolTag)
getSupportedProtocols lself quid = do
  muser <- API.lookupProfile lself quid !>> fedError
  user <- maybe (throwStd (errorToWai @'E.UserNotFound)) pure muser
  pure (Public.profileSupportedProtocols user)

getClientPrekeys :: UserId -> ClientId -> (Handler r) [Public.PrekeyId]
getClientPrekeys usr clt = lift (wrapClient $ API.lookupPrekeyIds usr clt)

newNonce :: UserId -> ClientId -> (Handler r) (Nonce, CacheControl)
newNonce uid cid = do
  ttl <- setNonceTtlSecs <$> view settings
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

-- | docs/reference/user/registration.md {#RefRegistration}
createUser ::
  ( Member BlacklistStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  Public.NewUserPublic ->
  (Handler r) (Either Public.RegisterError Public.RegisterSuccess)
createUser (Public.NewUserPublic new) = lift . runExceptT $ do
  API.checkRestrictedUserCreation new
  for_ (Public.newUserEmail new) $ mapExceptT wrapHttp . checkAllowlistWithError RegisterErrorAllowlistError . Left
  for_ (Public.newUserPhone new) $ mapExceptT wrapHttp . checkAllowlistWithError RegisterErrorAllowlistError . Right
  result <- API.createUser new
  let acc = createdAccount result

  let eac = createdEmailActivation result
  let pac = createdPhoneActivation result
  let epair = (,) <$> (activationKey <$> eac) <*> (activationCode <$> eac)
  let ppair = (,) <$> (activationKey <$> pac) <*> (activationCode <$> pac)
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

  let Public.User {userLocale, userDisplayName, userId} = usr
  let userEmail = Public.userEmail usr
  let userPhone = Public.userPhone usr
  lift $ do
    for_ (liftM2 (,) userEmail epair) $ \(e, p) ->
      sendActivationEmail e userDisplayName p (Just userLocale) newUserTeam
    for_ (liftM2 (,) userPhone ppair) $ \(p, c) ->
      wrapClient $ sendActivationSms p c (Just userLocale)
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
    sendActivationEmail :: Public.Email -> Public.Name -> ActivationPair -> Maybe Public.Locale -> Maybe Public.NewTeamUser -> (AppT r) ()
    sendActivationEmail e u p l mTeamUser
      | Just teamUser <- mTeamUser,
        Public.NewTeamCreator creator <- teamUser,
        let Public.BindingNewTeamUser (Public.BindingNewTeam team) _ = creator =
          sendTeamActivationMail e u p l (fromRange $ team ^. Public.newTeamName)
      | otherwise =
          sendActivationMail e u p l Nothing

    sendWelcomeEmail :: Public.Email -> CreateUserTeam -> Public.NewTeamUser -> Maybe Public.Locale -> (AppT r) ()
    -- NOTE: Welcome e-mails for the team creator are not dealt by brig anymore
    sendWelcomeEmail e (CreateUserTeam t n) newUser l = case newUser of
      Public.NewTeamCreator _ ->
        pure ()
      Public.NewTeamMember _ ->
        Team.sendMemberWelcomeMail e t n l
      Public.NewTeamMemberSSO _ ->
        Team.sendMemberWelcomeMail e t n l

getSelf :: Member GalleyProvider r => UserId -> (Handler r) Public.SelfProfile
getSelf self =
  lift (API.lookupSelfProfile self)
    >>= ifNothing (errorToWai @'E.UserNotFound)
    >>= lift . liftSem . API.hackForBlockingHandleChangeForE2EIdTeams

getUserUnqualifiedH ::
  (Member GalleyProvider r) =>
  UserId ->
  UserId ->
  (Handler r) (Maybe Public.UserProfile)
getUserUnqualifiedH self uid = do
  domain <- viewFederationDomain
  getUser self (Qualified uid domain)

getUser ::
  (Member GalleyProvider r) =>
  UserId ->
  Qualified UserId ->
  (Handler r) (Maybe Public.UserProfile)
getUser self qualifiedUserId = do
  lself <- qualifyLocal self
  API.lookupProfile lself qualifiedUserId !>> fedError

-- FUTUREWORK: Make servant understand that at least one of these is required
listUsersByUnqualifiedIdsOrHandles ::
  ( Member GalleyProvider r,
    Member (Concurrency 'Unsafe) r
  ) =>
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

listUsersByIdsOrHandlesGetIds :: [Handle] -> (Handler r) [Qualified UserId]
listUsersByIdsOrHandlesGetIds localHandles = do
  localUsers <- catMaybes <$> traverse (lift . wrapClient . API.lookupHandle) localHandles
  domain <- viewFederationDomain
  pure $ map (`Qualified` domain) localUsers

listUsersByIdsOrHandlesGetUsers :: Local x -> Range n m [Qualified Handle] -> Handler r [Qualified UserId]
listUsersByIdsOrHandlesGetUsers lself hs = do
  let (localHandles, _) = partitionQualified lself (fromRange hs)
  listUsersByIdsOrHandlesGetIds localHandles

listUsersByIdsOrHandlesV3 ::
  forall r.
  ( Member GalleyProvider r,
    Member (Concurrency 'Unsafe) r
  ) =>
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
    byIds lself uids = API.lookupProfiles lself uids !>> fedError

-- Similar to listUsersByIdsOrHandlesV3, except that it allows partial successes
-- using a new return type
listUsersByIdsOrHandles ::
  forall r.
  ( Member GalleyProvider r,
    Member (Concurrency 'Unsafe) r
  ) =>
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
    byIds lself uids = lift (API.lookupProfilesV3 lself uids) !>> fedError

newtype GetActivationCodeResp
  = GetActivationCodeResp (Public.ActivationKey, Public.ActivationCode)

instance ToJSON GetActivationCodeResp where
  toJSON (GetActivationCodeResp (k, c)) = object ["key" .= k, "code" .= c]

updateUser ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member GalleyProvider r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  ConnId ->
  Public.UserUpdate ->
  (Handler r) (Maybe Public.UpdateProfileError)
updateUser uid conn uu = do
  eithErr <- lift $ runExceptT $ API.updateUser uid (Just conn) uu API.ForbidSCIMUpdates
  pure $ either Just (const Nothing) eithErr

changePhone ::
  ( Member BlacklistStore r,
    Member BlacklistPhonePrefixStore r
  ) =>
  UserId ->
  ConnId ->
  Public.PhoneUpdate ->
  (Handler r) (Maybe Public.ChangePhoneError)
changePhone u _ (Public.puPhone -> phone) = lift . exceptTToMaybe $ do
  (adata, pn) <- API.changePhone u phone
  loc <- lift $ wrapClient $ API.lookupLocale u
  let apair = (activationKey adata, activationCode adata)
  lift . wrapClient $ sendActivationSms pn apair loc

removePhone ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  ConnId ->
  (Handler r) (Maybe Public.RemoveIdentityError)
removePhone self conn =
  lift . exceptTToMaybe $ API.removePhone self conn

removeEmail ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  ConnId ->
  (Handler r) (Maybe Public.RemoveIdentityError)
removeEmail self conn =
  lift . exceptTToMaybe $ API.removeEmail self conn

checkPasswordExists :: UserId -> (Handler r) Bool
checkPasswordExists = fmap isJust . lift . wrapClient . API.lookupPassword

changePassword :: UserId -> Public.PasswordChange -> (Handler r) (Maybe Public.ChangePasswordError)
changePassword u cp = lift . exceptTToMaybe $ API.changePassword u cp

changeLocale ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  ConnId ->
  Public.LocaleUpdate ->
  (Handler r) ()
changeLocale u conn l = lift $ API.changeLocale u conn l

changeSupportedProtocols ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  Local UserId ->
  ConnId ->
  Public.SupportedProtocolUpdate ->
  Handler r ()
changeSupportedProtocols (tUnqualified -> u) conn (Public.SupportedProtocolUpdate prots) =
  lift $ API.changeSupportedProtocols u conn prots

-- | (zusr is ignored by this handler, ie. checking handles is allowed as long as you have
-- *any* account.)
checkHandle :: UserId -> Text -> Handler r ()
checkHandle _uid hndl =
  API.checkHandle hndl >>= \case
    API.CheckHandleInvalid -> throwStd (errorToWai @'E.InvalidHandle)
    API.CheckHandleFound -> pure ()
    API.CheckHandleNotFound -> throwStd (errorToWai @'E.HandleNotFound)

-- | (zusr is ignored by this handler, ie. checking handles is allowed as long as you have
-- *any* account.)
checkHandles :: UserId -> Public.CheckHandles -> Handler r [Handle]
checkHandles _ (Public.CheckHandles hs num) = do
  let handles = mapMaybe parseHandle (fromRange hs)
  lift $ wrapHttpClient $ API.checkHandles handles (fromRange num)

-- | This endpoint returns UserHandleInfo instead of UserProfile for backwards
-- compatibility, whereas the corresponding qualified endpoint (implemented by
-- 'Handle.getHandleInfo') returns UserProfile to reduce traffic between backends
-- in a federated scenario.
getHandleInfoUnqualifiedH ::
  ( Member GalleyProvider r
  ) =>
  UserId ->
  Handle ->
  (Handler r) (Maybe Public.UserHandleInfo)
getHandleInfoUnqualifiedH self handle = do
  domain <- viewFederationDomain
  Public.UserHandleInfo . Public.profileQualifiedId
    <$$> Handle.getHandleInfo self (Qualified handle domain)

changeHandle ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member GalleyProvider r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  ConnId ->
  Public.HandleUpdate ->
  (Handler r) (Maybe Public.ChangeHandleError)
changeHandle u conn (Public.HandleUpdate h) = lift . exceptTToMaybe $ do
  handle <- maybe (throwError Public.ChangeHandleInvalid) pure $ parseHandle h
  API.changeHandle u (Just conn) handle API.ForbidSCIMUpdates

beginPasswordReset ::
  (Member PasswordResetStore r, Member TinyLog r) =>
  Public.NewPasswordReset ->
  (Handler r) ()
beginPasswordReset (Public.NewPasswordReset target) = do
  checkAllowlist target
  (u, pair) <- API.beginPasswordReset target !>> pwResetError
  loc <- lift $ wrapClient $ API.lookupLocale u
  lift $ case target of
    Left email -> sendPasswordResetMail email pair loc
    Right phone -> wrapClient $ sendPasswordResetSms phone pair loc

completePasswordReset ::
  ( Member CodeStore r,
    Member PasswordResetStore r,
    Member TinyLog r
  ) =>
  Public.CompletePasswordReset ->
  (Handler r) ()
completePasswordReset req = do
  API.completePasswordReset (Public.cpwrIdent req) (Public.cpwrCode req) (Public.cpwrPassword req) !>> pwResetError

-- docs/reference/user/activation.md {#RefActivationRequest}
-- docs/reference/user/registration.md {#RefRegistration}
sendActivationCode ::
  ( Member BlacklistStore r,
    Member BlacklistPhonePrefixStore r,
    Member GalleyProvider r
  ) =>
  Public.SendActivationCode ->
  (Handler r) ()
sendActivationCode Public.SendActivationCode {..} = do
  either customerExtensionCheckBlockedDomains (const $ pure ()) saUserKey
  checkAllowlist saUserKey
  API.sendActivationCode saUserKey saLocale saCall !>> sendActCodeError

-- | If the user presents an email address from a blocked domain, throw an error.
--
-- The tautological constraint in the type signature is added so that once we remove the
-- feature, ghc will guide us here.
customerExtensionCheckBlockedDomains :: Public.Email -> (Handler r) ()
customerExtensionCheckBlockedDomains email = do
  mBlockedDomains <- asks (fmap domainsBlockedForRegistration . setCustomerExtensions . view settings)
  for_ mBlockedDomains $ \(DomainsBlockedForRegistration blockedDomains) -> do
    case mkDomain (Public.emailDomain email) of
      Left _ ->
        pure () -- if it doesn't fit the syntax of blocked domains, it is not blocked
      Right domain ->
        when (domain `elem` blockedDomains) $
          throwM $
            customerExtensionBlockedDomain domain

createConnectionUnqualified ::
  ( Member GalleyProvider r,
    Member NotificationSubsystem r,
    Member TinyLog r,
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
    Member GalleyProvider r,
    Member NotificationSubsystem r,
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
  ( Member GalleyProvider r,
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
    Member GalleyProvider r
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
  ( Member GalleyProvider r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  Public.DeleteUser ->
  (Handler r) (Maybe Code.Timeout)
deleteSelfUser u body = do
  API.deleteSelfUser u (Public.deleteUserPassword body) !>> deleteUserError

verifyDeleteUser ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  Public.VerifyDeleteUser ->
  Handler r ()
verifyDeleteUser body = API.verifyDeleteUser body !>> deleteUserError

updateUserEmail ::
  forall r.
  ( Member BlacklistStore r,
    Member GalleyProvider r
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
  void $ API.changeSelfEmail emailOwnerId email API.AllowSCIMUpdates
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
          teamMember <- MaybeT $ lift $ liftSem $ GalleyProvider.getTeamMember zuserId teamId
          pure $ teamMember `hasPermission` ChangeTeamMemberProfiles

-- activation

activate ::
  ( Member GalleyProvider r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  Public.ActivationKey ->
  Public.ActivationCode ->
  (Handler r) ActivationRespWithStatus
activate k c = do
  let activationRequest = Public.Activate (Public.ActivateKey k) c False
  activateKey activationRequest

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKey ::
  ( Member GalleyProvider r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
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
  Member GalleyProvider r =>
  Public.SendVerificationCode ->
  (Handler r) ()
sendVerificationCode req = do
  let email = Public.svcEmail req
  let action = Public.svcAction req
  mbAccount <- getAccount email
  featureEnabled <- getFeatureStatus mbAccount
  case (mbAccount, featureEnabled) of
    (Just account, True) -> do
      gen <- Code.mk6DigitGen $ Code.ForEmail email
      timeout <- setVerificationTimeout <$> view settings
      code <-
        Code.generate
          gen
          (Code.scopeFromAction action)
          (Code.Retries 3)
          timeout
          (Just $ toUUID $ Public.userId $ accountUser account)
      tryInsertVerificationCode code $ verificationCodeThrottledError . VerificationCodeThrottled
      sendMail email (Code.codeValue code) (Just $ Public.userLocale $ accountUser account) action
    _ -> pure ()
  where
    getAccount :: Public.Email -> (Handler r) (Maybe UserAccount)
    getAccount email = lift $ do
      mbUserId <- wrapClient . UserKey.lookupKey $ UserKey.userEmailKey email
      join <$> wrapClient (Data.lookupAccount `traverse` mbUserId)

    sendMail :: Public.Email -> Code.Value -> Maybe Public.Locale -> Public.VerificationAction -> (Handler r) ()
    sendMail email value mbLocale =
      lift . \case
        Public.CreateScimToken -> sendCreateScimTokenVerificationMail email value mbLocale
        Public.Login -> sendLoginVerificationMail email value mbLocale
        Public.DeleteTeam -> sendTeamDeletionVerificationMail email value mbLocale

    getFeatureStatus :: Maybe UserAccount -> (Handler r) Bool
    getFeatureStatus mbAccount = do
      mbStatusEnabled <- lift $ liftSem $ GalleyProvider.getVerificationCodeEnabled `traverse` (Public.userTeam <$> accountUser =<< mbAccount)
      pure $ fromMaybe False mbStatusEnabled

getSystemSettings :: (Handler r) SystemSettingsPublic
getSystemSettings = do
  optSettings <- view settings
  pure $
    SystemSettingsPublic $
      fromMaybe False (setRestrictUserCreation optSettings)

getSystemSettingsInternal :: UserId -> (Handler r) SystemSettings
getSystemSettingsInternal _ = do
  optSettings <- view settings
  let pSettings = SystemSettingsPublic $ fromMaybe False (setRestrictUserCreation optSettings)
  let iSettings = SystemSettingsInternal $ fromMaybe False (setEnableMLS optSettings)
  pure $ SystemSettings pSettings iSettings

-- Deprecated

deprecatedOnboarding :: UserId -> JsonValue -> (Handler r) DeprecatedMatchingResult
deprecatedOnboarding _ _ = pure DeprecatedMatchingResult

deprecatedCompletePasswordReset ::
  ( Member CodeStore r,
    Member PasswordResetStore r,
    Member TinyLog r
  ) =>
  Public.PasswordResetKey ->
  Public.PasswordReset ->
  (Handler r) ()
deprecatedCompletePasswordReset k pwr = do
  API.completePasswordReset
    (Public.PasswordResetIdentityKey k)
    (Public.pwrCode pwr)
    (Public.pwrPassword pwr)
    !>> pwResetError

-- Utilities

ifNothing :: Utilities.Error -> Maybe a -> (Handler r) a
ifNothing e = maybe (throwStd e) pure
