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
  ( sitemap,
    apiDocs,
    servantSitemap,
    docsAPI,
    DocsAPI,
  )
where

import Brig.API.Auth
import qualified Brig.API.Client as API
import qualified Brig.API.Connection as API
import Brig.API.Error
import Brig.API.Handler
import Brig.API.MLS.KeyPackages
import qualified Brig.API.Properties as API
import Brig.API.Public.Swagger
import Brig.API.Types
import qualified Brig.API.User as API
import Brig.API.Util
import qualified Brig.API.Util as API
import Brig.App
import qualified Brig.Calling.API as Calling
import qualified Brig.Code as Code
import qualified Brig.Data.Connection as Data
import Brig.Data.Nonce as Nonce
import qualified Brig.Data.User as Data
import qualified Brig.Data.UserKey as UserKey
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.CodeStore (CodeStore)
import Brig.Effects.GalleyProvider (GalleyProvider)
import qualified Brig.Effects.GalleyProvider as GalleyProvider
import Brig.Effects.JwtTools (JwtTools)
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.PublicKeyBundle (PublicKeyBundle)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Provider.API as Provider
import qualified Brig.Team.API as Team
import qualified Brig.Team.Email as Team
import Brig.Types.Activation (ActivationPair)
import Brig.Types.Intra (AccountStatus (Ephemeral), UserAccount (UserAccount, accountUser))
import Brig.Types.User (HavePendingInvitations (..))
import qualified Brig.User.API.Handle as Handle
import Brig.User.API.Search (teamUserSearch)
import qualified Brig.User.API.Search as Search
import qualified Brig.User.Auth.Cookie as Auth
import Brig.User.Email
import Brig.User.Phone
import qualified Cassandra as C
import qualified Cassandra as Data
import Control.Error hiding (bool)
import Control.Lens (view, (.~), (?~), (^.))
import Control.Monad.Catch (throwM)
import Data.Aeson hiding (json)
import Data.Bifunctor
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.CommaSeparatedList
import Data.Domain
import Data.FileEmbed
import Data.Handle (Handle, parseHandle)
import Data.Id as Id
import qualified Data.Map.Strict as Map
import Data.Misc (IpAddr (..))
import Data.Nonce (Nonce, randomNonce)
import Data.Qualified
import Data.Range
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Lazy (pack)
import qualified Data.ZAuth.Token as ZAuth
import FileEmbedLzma
import Galley.Types.Teams (HiddenPerm (..), hasPermission)
import Imports hiding (head)
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities as Utilities
import Network.Wai.Utilities.Swagger (mkSwaggerApi)
import Polysemy
import Servant hiding (Handler, JSON, addHeader, respond)
import qualified Servant
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import qualified System.Logger.Class as Log
import Util.Logging (logFunction, logHandle, logTeam, logUser)
import qualified Wire.API.Connection as Public
import Wire.API.Error
import qualified Wire.API.Error.Brig as E
import Wire.API.Federation.API
import qualified Wire.API.Properties as Public
import qualified Wire.API.Routes.MultiTablePaging as Public
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Brig
import qualified Wire.API.Routes.Public.Cannon as CannonAPI
import qualified Wire.API.Routes.Public.Cargohold as CargoholdAPI
import qualified Wire.API.Routes.Public.Galley as GalleyAPI
import qualified Wire.API.Routes.Public.Gundeck as GundeckAPI
import qualified Wire.API.Routes.Public.Proxy as ProxyAPI
import qualified Wire.API.Routes.Public.Spar as SparAPI
import qualified Wire.API.Routes.Public.Util as Public
import Wire.API.Routes.Version
import qualified Wire.API.Swagger as Public.Swagger (models)
import Wire.API.SwaggerHelper (cleanupSwagger)
import Wire.API.SystemSettings
import qualified Wire.API.Team as Public
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.User (RegisterError (RegisterErrorWhitelistError))
import qualified Wire.API.User as Public
import qualified Wire.API.User.Activation as Public
import qualified Wire.API.User.Auth as Public
import qualified Wire.API.User.Client as Public
import Wire.API.User.Client.DPoPAccessToken
import qualified Wire.API.User.Client.Prekey as Public
import qualified Wire.API.User.Handle as Public
import qualified Wire.API.User.Password as Public
import qualified Wire.API.User.RichInfo as Public
import qualified Wire.API.UserMap as Public
import qualified Wire.API.Wrapped as Public
import Wire.Sem.Concurrency
import Wire.Sem.Now (Now)

-- User API -----------------------------------------------------------

docsAPI :: Servant.Server DocsAPI
docsAPI = versionedSwaggerDocsAPI :<|> pure eventNotificationSchemas

versionedSwaggerDocsAPI :: Servant.Server VersionedSwaggerDocsAPI
versionedSwaggerDocsAPI (Just V3) =
  swaggerSchemaUIServer $
    ( brigSwagger
        <> versionSwagger
        <> GalleyAPI.swaggerDoc
        <> SparAPI.swaggerDoc
        <> CargoholdAPI.swaggerDoc
        <> CannonAPI.swaggerDoc
        <> GundeckAPI.swaggerDoc
        <> ProxyAPI.swaggerDoc
    )
      & S.info . S.title .~ "Wire-Server API"
      & S.info . S.description ?~ $(embedText =<< makeRelativeToProject "docs/swagger.md")
      & cleanupSwagger
versionedSwaggerDocsAPI (Just V0) = swaggerPregenUIServer $(pregenSwagger V0)
versionedSwaggerDocsAPI (Just V1) = swaggerPregenUIServer $(pregenSwagger V1)
versionedSwaggerDocsAPI (Just V2) = swaggerPregenUIServer $(pregenSwagger V2)
versionedSwaggerDocsAPI Nothing = versionedSwaggerDocsAPI (Just maxBound)

servantSitemap ::
  forall r p.
  ( Members
      '[ BlacklistPhonePrefixStore,
         BlacklistStore,
         CodeStore,
         Concurrency 'Unsafe,
         Concurrency 'Unsafe,
         GalleyProvider,
         JwtTools,
         Now,
         PasswordResetStore,
         PublicKeyBundle,
         UserPendingActivationStore p
       ]
      r
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
  where
    userAPI :: ServerT UserAPI (Handler r)
    userAPI =
      Named @"get-user-unqualified" (callsFed getUserUnqualifiedH)
        :<|> Named @"get-user-qualified" (callsFed getUser)
        :<|> Named @"update-user-email" updateUserEmail
        :<|> Named @"get-handle-info-unqualified" (callsFed getHandleInfoUnqualifiedH)
        :<|> Named @"get-user-by-handle-qualified" (callsFed Handle.getHandleInfo)
        :<|> Named @"list-users-by-unqualified-ids-or-handles" (callsFed listUsersByUnqualifiedIdsOrHandles)
        :<|> Named @"list-users-by-ids-or-handles" (callsFed listUsersByIdsOrHandles)
        :<|> Named @"send-verification-code" sendVerificationCode
        :<|> Named @"get-rich-info" getRichInfo

    selfAPI :: ServerT SelfAPI (Handler r)
    selfAPI =
      Named @"get-self" getSelf
        :<|> Named @"delete-self" (callsFed deleteSelfUser)
        :<|> Named @"put-self" (callsFed updateUser)
        :<|> Named @"change-phone" changePhone
        :<|> Named @"remove-phone" (callsFed removePhone)
        :<|> Named @"remove-email" (callsFed removeEmail)
        :<|> Named @"check-password-exists" checkPasswordExists
        :<|> Named @"change-password" changePassword
        :<|> Named @"change-locale" (callsFed changeLocale)
        :<|> Named @"change-handle" (callsFed changeHandle)

    accountAPI :: ServerT AccountAPI (Handler r)
    accountAPI =
      Named @"register" (callsFed createUser)
        :<|> Named @"verify-delete" (callsFed verifyDeleteUser)
        :<|> Named @"get-activate" (callsFed activate)
        :<|> Named @"post-activate" (callsFed activateKey)
        :<|> Named @"post-activate-send" sendActivationCode
        :<|> Named @"post-password-reset" beginPasswordReset
        :<|> Named @"post-password-reset-complete" completePasswordReset
        :<|> Named @"post-password-reset-key-deprecated" deprecatedCompletePasswordReset
        :<|> Named @"onboarding" deprecatedOnboarding

    clientAPI :: ServerT ClientAPI (Handler r)
    clientAPI =
      Named @"get-user-clients-unqualified" (callsFed getUserClientsUnqualified)
        :<|> Named @"get-user-clients-qualified" (callsFed getUserClientsQualified)
        :<|> Named @"get-user-client-unqualified" (callsFed getUserClientUnqualified)
        :<|> Named @"get-user-client-qualified" (callsFed getUserClientQualified)
        :<|> Named @"list-clients-bulk" (callsFed listClientsBulk)
        :<|> Named @"list-clients-bulk-v2" (callsFed listClientsBulkV2)
        :<|> Named @"list-clients-bulk@v2" (callsFed listClientsBulkV2)

    prekeyAPI :: ServerT PrekeyAPI (Handler r)
    prekeyAPI =
      Named @"get-users-prekeys-client-unqualified" (callsFed getPrekeyUnqualifiedH)
        :<|> Named @"get-users-prekeys-client-qualified" (callsFed getPrekeyH)
        :<|> Named @"get-users-prekey-bundle-unqualified" (callsFed getPrekeyBundleUnqualifiedH)
        :<|> Named @"get-users-prekey-bundle-qualified" (callsFed getPrekeyBundleH)
        :<|> Named @"get-multi-user-prekey-bundle-unqualified" getMultiUserPrekeyBundleUnqualifiedH
        :<|> Named @"get-multi-user-prekey-bundle-qualified" (callsFed getMultiUserPrekeyBundleH)

    userClientAPI :: ServerT UserClientAPI (Handler r)
    userClientAPI =
      Named @"add-client" (callsFed addClient)
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
      Named @"create-connection-unqualified" (callsFed createConnectionUnqualified)
        :<|> Named @"create-connection" (callsFed createConnection)
        :<|> Named @"list-local-connections" listLocalConnections
        :<|> Named @"list-connections" listConnections
        :<|> Named @"get-connection-unqualified" getLocalConnection
        :<|> Named @"get-connection" getConnection
        :<|> Named @"update-connection-unqualified" (callsFed updateLocalConnection)
        :<|> Named @"update-connection" (callsFed updateConnection)
        :<|> Named @"search-contacts" (callsFed Search.search)

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
        :<|> Named @"mls-key-packages-claim" (callsFed claimKeyPackages)
        :<|> Named @"mls-key-packages-count" countKeyPackages

    userHandleAPI :: ServerT UserHandleAPI (Handler r)
    userHandleAPI =
      Named @"check-user-handles" checkHandles
        :<|> Named @"check-user-handle" checkHandle

    searchAPI :: ServerT SearchAPI (Handler r)
    searchAPI =
      Named @"browse-team" teamUserSearch

    authAPI :: ServerT AuthAPI (Handler r)
    authAPI =
      Named @"access" (callsFed accessH)
        :<|> Named @"send-login-code" sendLoginCode
        :<|> Named @"login" (callsFed login)
        :<|> Named @"logout" logoutH
        :<|> Named @"change-self-email" changeSelfEmailH
        :<|> Named @"list-cookies" listCookies
        :<|> Named @"remove-cookies" removeCookies

    callingAPI :: ServerT CallingAPI (Handler r)
    callingAPI =
      Named @"get-calls-config" Calling.getCallsConfig
        :<|> Named @"get-calls-config-v2" Calling.getCallsConfigV2

    systemSettingsAPI :: ServerT SystemSettingsAPI (Handler r)
    systemSettingsAPI = Named @"get-system-settings" getSystemSettings

-- Note [ephemeral user sideeffect]
-- If the user is ephemeral and expired, it will be removed upon calling
-- CheckUserExists[Un]Qualified, see 'Brig.API.User.userGC'.
-- This leads to the following events being sent:
-- - UserDeleted event to contacts of the user
-- - MemberLeave event to members for all conversations the user was in (via galley)

sitemap ::
  Members
    '[ BlacklistPhonePrefixStore,
       BlacklistStore,
       CodeStore,
       Concurrency 'Unsafe,
       GalleyProvider,
       PasswordResetStore
     ]
    r =>
  Routes Doc.ApiBuilder (Handler r) ()
sitemap = do
  Provider.routesPublic

apiDocs ::
  forall r.
  Members
    '[ BlacklistPhonePrefixStore,
       BlacklistStore,
       CodeStore,
       Concurrency 'Unsafe,
       GalleyProvider,
       PasswordResetStore
     ]
    r =>
  Routes Doc.ApiBuilder (Handler r) ()
apiDocs =
  get
    "/users/api-docs"
    ( \(_ ::: url) k ->
        let doc = mkSwaggerApi (decodeLatin1 url) Public.Swagger.models (sitemap @r)
         in k $ json doc
    )
    $ accept "application" "json"
      .&. query "base_url"

---------------------------------------------------------------------------
-- Handlers

setProperty :: UserId -> ConnId -> Public.PropertyKey -> Public.RawPropertyValue -> Handler r ()
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

deleteProperty :: UserId -> ConnId -> Public.PropertyKey -> Handler r ()
deleteProperty u c k = lift (API.deleteProperty u c k)

clearProperties :: UserId -> ConnId -> Handler r ()
clearProperties u c = lift (API.clearProperties u c)

getProperty :: UserId -> Public.PropertyKey -> Handler r (Maybe Public.RawPropertyValue)
getProperty u k = lift . wrapClient $ API.lookupProperty u k

listPropertyKeys :: UserId -> Handler r [Public.PropertyKey]
listPropertyKeys u = lift $ wrapClient (API.lookupPropertyKeys u)

listPropertyKeysAndValues :: UserId -> Handler r Public.PropertyKeysAndValues
listPropertyKeysAndValues u = do
  keysAndVals <- fmap Map.fromList . lift $ wrapClient (API.lookupPropertyKeysAndValues u)
  Public.PropertyKeysAndValues <$> traverse parseStoredPropertyValue keysAndVals

getPrekeyUnqualifiedH :: (CallsFed 'Brig "claim-prekey") => UserId -> UserId -> ClientId -> (Handler r) Public.ClientPrekey
getPrekeyUnqualifiedH zusr user client = do
  domain <- viewFederationDomain
  getPrekeyH zusr (Qualified user domain) client

getPrekeyH :: (CallsFed 'Brig "claim-prekey") => UserId -> Qualified UserId -> ClientId -> (Handler r) Public.ClientPrekey
getPrekeyH zusr (Qualified user domain) client = do
  mPrekey <- API.claimPrekey (ProtectedUser zusr) user domain client !>> clientError
  ifNothing (notFound "prekey not found") mPrekey

getPrekeyBundleUnqualifiedH :: (CallsFed 'Brig "claim-prekey-bundle") => UserId -> UserId -> (Handler r) Public.PrekeyBundle
getPrekeyBundleUnqualifiedH zusr uid = do
  domain <- viewFederationDomain
  API.claimPrekeyBundle (ProtectedUser zusr) domain uid !>> clientError

getPrekeyBundleH :: (CallsFed 'Brig "claim-prekey-bundle") => UserId -> Qualified UserId -> (Handler r) Public.PrekeyBundle
getPrekeyBundleH zusr (Qualified uid domain) =
  API.claimPrekeyBundle (ProtectedUser zusr) domain uid !>> clientError

getMultiUserPrekeyBundleUnqualifiedH ::
  Members '[Concurrency 'Unsafe] r =>
  UserId ->
  Public.UserClients ->
  Handler r Public.UserClientPrekeyMap
getMultiUserPrekeyBundleUnqualifiedH zusr userClients = do
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  when (Map.size (Public.userClients userClients) > maxSize) $
    throwStd (errorToWai @'E.TooManyClients)
  API.claimLocalMultiPrekeyBundles (ProtectedUser zusr) userClients !>> clientError

getMultiUserPrekeyBundleH ::
  (Members '[Concurrency 'Unsafe] r, CallsFed 'Brig "claim-multi-prekey-bundle") =>
  UserId ->
  Public.QualifiedUserClients ->
  (Handler r) Public.QualifiedUserClientPrekeyMap
getMultiUserPrekeyBundleH zusr qualUserClients = do
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  let Sum (size :: Int) =
        Map.foldMapWithKey
          (\_ v -> Sum . Map.size $ v)
          (Public.qualifiedUserClients qualUserClients)
  when (size > maxSize) $
    throwStd (errorToWai @'E.TooManyClients)
  API.claimMultiPrekeyBundles (ProtectedUser zusr) qualUserClients !>> clientError

addClient ::
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "on-user-deleted-connections"
  ) =>
  UserId ->
  ConnId ->
  Maybe IpAddr ->
  Public.NewClient ->
  (Handler r) NewClientResponse
addClient usr con ip new = do
  -- Users can't add legal hold clients
  when (Public.newClientType new == Public.LegalHoldClientType) $
    throwE (clientError ClientLegalHoldCannotBeAdded)
  clientResponse
    <$> API.addClient usr (Just con) (ipAddr <$> ip) new
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

getUserClientsUnqualified :: (CallsFed 'Brig "get-user-clients") => UserId -> (Handler r) [Public.PubClient]
getUserClientsUnqualified uid = do
  localdomain <- viewFederationDomain
  API.lookupPubClients (Qualified uid localdomain) !>> clientError

getUserClientsQualified :: (CallsFed 'Brig "get-user-clients") => Qualified UserId -> (Handler r) [Public.PubClient]
getUserClientsQualified quid = API.lookupPubClients quid !>> clientError

getUserClientUnqualified :: (CallsFed 'Brig "get-user-clients") => UserId -> ClientId -> (Handler r) Public.PubClient
getUserClientUnqualified uid cid = do
  localdomain <- viewFederationDomain
  x <- API.lookupPubClient (Qualified uid localdomain) cid !>> clientError
  ifNothing (notFound "client not found") x

listClientsBulk :: (CallsFed 'Brig "get-user-clients") => UserId -> Range 1 MaxUsersForListClientsBulk [Qualified UserId] -> (Handler r) (Public.QualifiedUserMap (Set Public.PubClient))
listClientsBulk _zusr limitedUids =
  API.lookupPubClientsBulk (fromRange limitedUids) !>> clientError

listClientsBulkV2 :: (CallsFed 'Brig "get-user-clients") => UserId -> Public.LimitedQualifiedUserIdList MaxUsersForListClientsBulk -> (Handler r) (Public.WrappedQualifiedUserMap (Set Public.PubClient))
listClientsBulkV2 zusr userIds = Public.Wrapped <$> listClientsBulk zusr (Public.qualifiedUsers userIds)

getUserClientQualified :: (CallsFed 'Brig "get-user-clients") => Qualified UserId -> ClientId -> (Handler r) Public.PubClient
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

getClientPrekeys :: UserId -> ClientId -> (Handler r) [Public.PrekeyId]
getClientPrekeys usr clt = lift (wrapClient $ API.lookupPrekeyIds usr clt)

newNonce :: UserId -> ClientId -> (Handler r) (Nonce, CacheControl)
newNonce uid cid = do
  ttl <- setNonceTtlSecs <$> view settings
  nonce <- randomNonce
  lift $ wrapClient $ Nonce.insertNonce ttl uid (client cid) nonce
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
  UserId ->
  ClientId ->
  Proof ->
  (Handler r) (DPoPAccessTokenResponse, CacheControl)
createAccessToken method uid cid proof = do
  let link = safeLink (Proxy @api) (Proxy @endpoint) cid
  API.createAccessToken uid cid method link proof !>> certEnrollmentError

-- | docs/reference/user/registration.md {#RefRegistration}
createUser ::
  ( Members
      '[ BlacklistStore,
         GalleyProvider,
         UserPendingActivationStore p
       ]
      r,
    CallsFed 'Brig "on-user-deleted-connections"
  ) =>
  Public.NewUserPublic ->
  (Handler r) (Either Public.RegisterError Public.RegisterSuccess)
createUser (Public.NewUserPublic new) = lift . runExceptT $ do
  API.checkRestrictedUserCreation new
  for_ (Public.newUserEmail new) $ mapExceptT wrapHttp . checkWhitelistWithError RegisterErrorWhitelistError . Left
  for_ (Public.newUserPhone new) $ mapExceptT wrapHttp . checkWhitelistWithError RegisterErrorWhitelistError . Right
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
      UserAccount _ Ephemeral ->
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

getSelf :: UserId -> (Handler r) Public.SelfProfile
getSelf self =
  lift (API.lookupSelfProfile self)
    >>= ifNothing (errorToWai @'E.UserNotFound)

getUserUnqualifiedH ::
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "get-users-by-ids"
  ) =>
  UserId ->
  UserId ->
  (Handler r) (Maybe Public.UserProfile)
getUserUnqualifiedH self uid = do
  domain <- viewFederationDomain
  getUser self (Qualified uid domain)

getUser ::
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "get-users-by-ids"
  ) =>
  UserId ->
  Qualified UserId ->
  (Handler r) (Maybe Public.UserProfile)
getUser self qualifiedUserId = do
  lself <- qualifyLocal self
  API.lookupProfile lself qualifiedUserId !>> fedError

-- FUTUREWORK: Make servant understand that at least one of these is required
listUsersByUnqualifiedIdsOrHandles ::
  ( Members
      '[ GalleyProvider,
         Concurrency 'Unsafe
       ]
      r,
    CallsFed 'Brig "get-users-by-ids"
  ) =>
  UserId ->
  Maybe (CommaSeparatedList UserId) ->
  Maybe (Range 1 4 (CommaSeparatedList Handle)) ->
  (Handler r) [Public.UserProfile]
listUsersByUnqualifiedIdsOrHandles self mUids mHandles = do
  domain <- viewFederationDomain
  case (mUids, mHandles) of
    (Just uids, _) -> listUsersByIdsOrHandles self (Public.ListUsersByIds ((`Qualified` domain) <$> fromCommaSeparatedList uids))
    (_, Just handles) ->
      let normalRangedList = fromCommaSeparatedList $ fromRange handles
          qualifiedList = (`Qualified` domain) <$> normalRangedList
          -- Use of unsafeRange here is ok only because we know that 'handles'
          -- is valid for 'Range 1 4'. However, we must not forget to keep this
          -- annotation here otherwise a change in 'Public.ListUsersByHandles'
          -- could cause this code to break.
          qualifiedRangedList :: Range 1 4 [Qualified Handle] = unsafeRange qualifiedList
       in listUsersByIdsOrHandles self (Public.ListUsersByHandles qualifiedRangedList)
    (Nothing, Nothing) -> throwStd $ badRequest "at least one ids or handles must be provided"

listUsersByIdsOrHandles ::
  forall r.
  ( Members
      '[ GalleyProvider,
         Concurrency 'Unsafe
       ]
      r,
    CallsFed 'Brig "get-users-by-ids"
  ) =>
  UserId ->
  Public.ListUsersQuery ->
  (Handler r) [Public.UserProfile]
listUsersByIdsOrHandles self q = do
  lself <- qualifyLocal self
  foundUsers <- case q of
    Public.ListUsersByIds us ->
      byIds lself us
    Public.ListUsersByHandles hs -> do
      let (localHandles, _) = partitionQualified lself (fromRange hs)
      us <- getIds localHandles
      Handle.filterHandleResults lself =<< byIds lself us
  case foundUsers of
    [] -> throwStd $ notFound "None of the specified ids or handles match any users"
    _ -> pure foundUsers
  where
    getIds :: [Handle] -> (Handler r) [Qualified UserId]
    getIds localHandles = do
      localUsers <- catMaybes <$> traverse (lift . wrapClient . API.lookupHandle) localHandles
      domain <- viewFederationDomain
      pure $ map (`Qualified` domain) localUsers
    byIds :: Local UserId -> [Qualified UserId] -> (Handler r) [Public.UserProfile]
    byIds lself uids = API.lookupProfiles lself uids !>> fedError

newtype GetActivationCodeResp
  = GetActivationCodeResp (Public.ActivationKey, Public.ActivationCode)

instance ToJSON GetActivationCodeResp where
  toJSON (GetActivationCodeResp (k, c)) = object ["key" .= k, "code" .= c]

updateUser :: (CallsFed 'Brig "on-user-deleted-connections") => UserId -> ConnId -> Public.UserUpdate -> (Handler r) (Maybe Public.UpdateProfileError)
updateUser uid conn uu = do
  eithErr <- lift $ runExceptT $ API.updateUser uid (Just conn) uu API.ForbidSCIMUpdates
  pure $ either Just (const Nothing) eithErr

changePhone ::
  Members
    '[ BlacklistStore,
       BlacklistPhonePrefixStore
     ]
    r =>
  UserId ->
  ConnId ->
  Public.PhoneUpdate ->
  (Handler r) (Maybe Public.ChangePhoneError)
changePhone u _ (Public.puPhone -> phone) = lift . exceptTToMaybe $ do
  (adata, pn) <- API.changePhone u phone
  loc <- lift $ wrapClient $ API.lookupLocale u
  let apair = (activationKey adata, activationCode adata)
  lift . wrapClient $ sendActivationSms pn apair loc

removePhone :: (CallsFed 'Brig "on-user-deleted-connections") => UserId -> ConnId -> (Handler r) (Maybe Public.RemoveIdentityError)
removePhone self conn =
  lift . exceptTToMaybe $ API.removePhone self conn

removeEmail :: (CallsFed 'Brig "on-user-deleted-connections") => UserId -> ConnId -> (Handler r) (Maybe Public.RemoveIdentityError)
removeEmail self conn =
  lift . exceptTToMaybe $ API.removeEmail self conn

checkPasswordExists :: UserId -> (Handler r) Bool
checkPasswordExists = fmap isJust . lift . wrapClient . API.lookupPassword

changePassword :: UserId -> Public.PasswordChange -> (Handler r) (Maybe Public.ChangePasswordError)
changePassword u cp = lift . exceptTToMaybe $ API.changePassword u cp

changeLocale :: (CallsFed 'Brig "on-user-deleted-connections") => UserId -> ConnId -> Public.LocaleUpdate -> (Handler r) ()
changeLocale u conn l = lift $ API.changeLocale u conn l

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
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "get-user-by-handle",
    CallsFed 'Brig "get-users-by-ids"
  ) =>
  UserId ->
  Handle ->
  (Handler r) (Maybe Public.UserHandleInfo)
getHandleInfoUnqualifiedH self handle = do
  domain <- viewFederationDomain
  Public.UserHandleInfo . Public.profileQualifiedId
    <$$> Handle.getHandleInfo self (Qualified handle domain)

changeHandle :: (CallsFed 'Brig "on-user-deleted-connections") => UserId -> ConnId -> Public.HandleUpdate -> (Handler r) (Maybe Public.ChangeHandleError)
changeHandle u conn (Public.HandleUpdate h) = lift . exceptTToMaybe $ do
  handle <- maybe (throwError Public.ChangeHandleInvalid) pure $ parseHandle h
  API.changeHandle u (Just conn) handle API.ForbidSCIMUpdates

beginPasswordReset ::
  Members '[PasswordResetStore] r =>
  Public.NewPasswordReset ->
  (Handler r) ()
beginPasswordReset (Public.NewPasswordReset target) = do
  checkWhitelist target
  (u, pair) <- API.beginPasswordReset target !>> pwResetError
  loc <- lift $ wrapClient $ API.lookupLocale u
  lift $ case target of
    Left email -> sendPasswordResetMail email pair loc
    Right phone -> wrapClient $ sendPasswordResetSms phone pair loc

completePasswordReset ::
  Members '[CodeStore, PasswordResetStore] r =>
  Public.CompletePasswordReset ->
  (Handler r) ()
completePasswordReset req = do
  API.completePasswordReset (Public.cpwrIdent req) (Public.cpwrCode req) (Public.cpwrPassword req) !>> pwResetError

-- docs/reference/user/activation.md {#RefActivationRequest}
-- docs/reference/user/registration.md {#RefRegistration}
sendActivationCode ::
  Members
    '[ BlacklistStore,
       BlacklistPhonePrefixStore,
       GalleyProvider
     ]
    r =>
  Public.SendActivationCode ->
  (Handler r) ()
sendActivationCode Public.SendActivationCode {..} = do
  either customerExtensionCheckBlockedDomains (const $ pure ()) saUserKey
  checkWhitelist saUserKey
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
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "send-connection-action"
  ) =>
  UserId ->
  ConnId ->
  Public.ConnectionRequest ->
  (Handler r) (Public.ResponseForExistedCreated Public.UserConnection)
createConnectionUnqualified self conn cr = do
  lself <- qualifyLocal self
  target <- qualifyLocal (Public.crUser cr)
  API.createConnection lself conn (tUntagged target) !>> connError

createConnection ::
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "send-connection-action"
  ) =>
  UserId ->
  ConnId ->
  Qualified UserId ->
  (Handler r) (Public.ResponseForExistedCreated Public.UserConnection)
createConnection self conn target = do
  lself <- qualifyLocal self
  API.createConnection lself conn target !>> connError

updateLocalConnection :: (CallsFed 'Brig "send-connection-action") => UserId -> ConnId -> UserId -> Public.ConnectionUpdate -> (Handler r) (Public.UpdateResult Public.UserConnection)
updateLocalConnection self conn other update = do
  lother <- qualifyLocal other
  updateConnection self conn (tUntagged lother) update

updateConnection :: (CallsFed 'Brig "send-connection-action") => UserId -> ConnId -> Qualified UserId -> Public.ConnectionUpdate -> (Handler r) (Public.UpdateResult Public.UserConnection)
updateConnection self conn other update = do
  let newStatus = Public.cuStatus update
  lself <- qualifyLocal self
  mc <- API.updateConnection lself other newStatus (Just conn) !>> connError
  pure $ maybe Public.Unchanged Public.Updated mc

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
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "on-user-deleted-connections"
  ) =>
  UserId ->
  Public.DeleteUser ->
  (Handler r) (Maybe Code.Timeout)
deleteSelfUser u body =
  API.deleteSelfUser u (Public.deleteUserPassword body) !>> deleteUserError

verifyDeleteUser :: (CallsFed 'Brig "on-user-deleted-connections") => Public.VerifyDeleteUser -> Handler r ()
verifyDeleteUser body = API.verifyDeleteUser body !>> deleteUserError

updateUserEmail ::
  forall r.
  Members
    '[ BlacklistStore,
       GalleyProvider
     ]
    r =>
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
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "on-user-deleted-connections"
  ) =>
  Public.ActivationKey ->
  Public.ActivationCode ->
  (Handler r) ActivationRespWithStatus
activate k c = do
  let activationRequest = Public.Activate (Public.ActivateKey k) c False
  activateKey activationRequest

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKey ::
  ( Members
      '[ GalleyProvider
       ]
      r,
    CallsFed 'Brig "on-user-deleted-connections"
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
  Members
    '[ GalleyProvider
     ]
    r =>
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

getSystemSettings :: ExceptT Brig.API.Error.Error (AppT r) SystemSettings
getSystemSettings = do
  optSettings <- view settings
  pure $
    SystemSettings
      { systemSettingsSetRestrictUserCreation = fromMaybe False (setRestrictUserCreation optSettings)
      }

-- Deprecated

deprecatedOnboarding :: UserId -> JsonValue -> (Handler r) DeprecatedMatchingResult
deprecatedOnboarding _ _ = pure DeprecatedMatchingResult

deprecatedCompletePasswordReset ::
  Members '[CodeStore, PasswordResetStore] r =>
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
