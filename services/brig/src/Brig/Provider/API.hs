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

module Brig.Provider.API
  ( -- * Main stuff
    botAPI,
    servicesAPI,
    providerAPI,
    internalProviderAPI,

    -- * Event handlers
    finishDeleteService,
  )
where

import Bilge.IO (MonadHttp)
import Bilge.RPC (HasRequestId)
import Brig.API.Client qualified as Client
import Brig.API.Error
import Brig.API.Handler
import Brig.API.Types (PasswordResetError (..))
import Brig.App
import Brig.Data.Client qualified as User
import Brig.Data.User qualified as User
import Brig.Options (Settings (..))
import Brig.Options qualified as Opt
import Brig.Provider.DB (ServiceConn (..))
import Brig.Provider.DB qualified as DB
import Brig.Provider.Email
import Brig.Provider.RPC qualified as RPC
import Cassandra (MonadClient)
import Control.Error (throwE)
import Control.Exception.Enclosed (handleAny)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except
import Data.ByteString.Conversion
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Code qualified as Code
import Data.CommaSeparatedList (CommaSeparatedList (fromCommaSeparatedList))
import Data.Conduit (runConduit, (.|))
import Data.Conduit.List qualified as C
import Data.Hashable (hash)
import Data.HavePendingInvitations
import Data.Id
import Data.LegalHold
import Data.List qualified as List
import Data.List1 (maybeList1)
import Data.Map.Strict qualified as Map
import Data.Misc
  ( Fingerprint (Fingerprint),
    FutureWork (FutureWork),
    IpAddr,
    Rsa,
  )
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text
import Data.ZAuth.CryptoSign (CryptoSign)
import GHC.TypeNats
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities.Error ((!>>))
import Network.Wai.Utilities.Error qualified as Wai
import OpenSSL.EVP.Digest qualified as SSL
import OpenSSL.EVP.PKey qualified as SSL
import OpenSSL.PEM qualified as SSL
import OpenSSL.RSA qualified as SSL
import OpenSSL.Random (randBytes)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Servant (ServerT, (:<|>) (..))
import Ssl.Util qualified as SSL
import System.Logger.Class (MonadLogger)
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Bot qualified as Public
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Event.Conversation qualified as Public (Event)
import Wire.API.Password
import Wire.API.Provider
import Wire.API.Provider qualified as Public
import Wire.API.Provider.Bot qualified as Ext
import Wire.API.Provider.Bot qualified as Public (BotUserView)
import Wire.API.Provider.External
import Wire.API.Provider.External qualified as Ext
import Wire.API.Provider.Service
import Wire.API.Provider.Service qualified as Public
import Wire.API.Provider.Service.Tag qualified as Public
import Wire.API.Routes.Internal.Brig qualified as BrigIRoutes
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Brig.Bot (BotAPI)
import Wire.API.Routes.Public.Brig.Provider (ProviderAPI)
import Wire.API.Routes.Public.Brig.Services (ServicesAPI)
import Wire.API.Team.Feature qualified as Feature
import Wire.API.Team.LegalHold (LegalholdProtectee (UnprotectedBot))
import Wire.API.Team.Permission
import Wire.API.User
import Wire.API.User qualified as Public (UserProfile, mkUserProfile)
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client qualified as Public (Client, ClientCapability (ClientSupportsLegalholdImplicitConsent), PubClient (..), UserClientPrekeyMap, UserClients, userClients)
import Wire.API.User.Client.Prekey qualified as Public (PrekeyId)
import Wire.AuthenticationSubsystem as Authentication
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.ZAuth qualified as ZAuth
import Wire.DeleteQueue
import Wire.EmailSending (EmailSending)
import Wire.Error
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.HashPassword (HashPassword)
import Wire.HashPassword qualified as HashPassword
import Wire.RateLimit
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Unsafe))
import Wire.Sem.Now (Now)
import Wire.SessionStore (SessionStore)
import Wire.UserKeyStore (mkEmailKey)
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.VerificationCode as VerificationCode
import Wire.VerificationCodeGen
import Wire.VerificationCodeSubsystem

botAPI ::
  ( Member GalleyAPIAccess r,
    Member (Concurrency 'Unsafe) r,
    Member DeleteQueue r,
    Member AuthenticationSubsystem r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member SessionStore r,
    Member Now r,
    Member CryptoSign r
  ) =>
  ServerT BotAPI (Handler r)
botAPI =
  Named @"add-bot@v6" addBot
    :<|> Named @"add-bot" addBot
    :<|> Named @"remove-bot@v6" removeBot
    :<|> Named @"remove-bot" removeBot
    :<|> Named @"bot-get-self" botGetSelf
    :<|> Named @"bot-delete-self" botDeleteSelf
    :<|> Named @"bot-list-prekeys" botListPrekeys
    :<|> Named @"bot-update-prekeys" botUpdatePrekeys
    :<|> Named @"bot-get-client@v6" botGetClient
    :<|> Named @"bot-get-client@v7" botGetClient
    :<|> Named @"bot-get-client" botGetClient
    :<|> Named @"bot-claim-users-prekeys" botClaimUsersPrekeys
    :<|> Named @"bot-list-users" botListUserProfiles
    :<|> Named @"bot-get-user-clients" botGetUserClients

servicesAPI ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r,
    Member DeleteQueue r,
    Member (Error UserSubsystemError) r
  ) =>
  ServerT ServicesAPI (Handler r)
servicesAPI =
  Named @"post-provider-services" addService
    :<|> Named @"get-provider-services" listServices
    :<|> Named @"get-provider-services-by-service-id" getService
    :<|> Named @"put-provider-services-by-service-id" updateService
    :<|> Named @"put-provider-services-connection-by-service-id" updateServiceConn
    :<|> Named @"delete-provider-services-by-service-id" deleteService
    :<|> Named @"get-provider-services-by-provider-id" listServiceProfiles
    :<|> Named @"get-services" searchServiceProfiles
    :<|> Named @"get-services-tags" getServiceTagList
    :<|> Named @"get-provider-services-by-provider-id-and-service-id" getServiceProfile
    :<|> Named @"get-whitelisted-services-by-team-id" searchTeamServiceProfiles
    :<|> Named @"post-team-whitelist-by-team-id" updateServiceWhitelist

providerAPI ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r,
    Member EmailSending r,
    Member HashPassword r,
    Member VerificationCodeSubsystem r,
    Member RateLimit r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member CryptoSign r,
    Member Now r
  ) =>
  ServerT ProviderAPI (Handler r)
providerAPI =
  Named @"provider-register" newAccount
    :<|> Named @"provider-activate" activateAccountKey
    :<|> Named @"provider-login" login
    :<|> Named @"provider-password-reset" beginPasswordReset
    :<|> Named @"provider-password-reset-complete" completePasswordReset
    :<|> Named @"provider-delete" deleteAccount
    :<|> Named @"provider-update" updateAccountProfile
    :<|> Named @"provider-update-email" updateAccountEmail
    :<|> Named @"provider-update-password" updateAccountPassword
    :<|> Named @"provider-get-account" getAccount
    :<|> Named @"provider-get-profile" getProviderProfile

internalProviderAPI ::
  ( Member GalleyAPIAccess r,
    Member VerificationCodeSubsystem r
  ) =>
  ServerT BrigIRoutes.ProviderAPI (Handler r)
internalProviderAPI =
  Named @"get-provider-activation-code" getActivationCode
    :<|> Named @"get-provider-password-reset-code" getPasswordResetCode

--------------------------------------------------------------------------------
-- Public API (Unauthenticated)

newAccount ::
  ( Member GalleyAPIAccess r,
    Member EmailSending r,
    Member HashPassword r,
    Member VerificationCodeSubsystem r,
    Member RateLimit r
  ) =>
  IpAddr ->
  Public.NewProvider ->
  (Handler r) Public.NewProviderResponse
newAccount ip new = do
  guardSecondFactorDisabled Nothing
  let email = new.newProviderEmail
  let name = new.newProviderName
  let pass = new.newProviderPassword
  let descr = fromRange new.newProviderDescr
  let url = new.newProviderUrl
  let emailKey = mkEmailKey email
  wrapClientE (DB.lookupKey emailKey) >>= mapM_ (const $ throwStd emailExists)
  (safePass, newPass) <- case pass of
    Just newPass -> do
      hashed <- lift . liftSem $ HashPassword.hashPassword6 (RateLimitIp ip) newPass
      pure (hashed, Nothing)
    Nothing -> do
      newPass <- genPassword
      safePass <- lift . liftSem $ HashPassword.hashPassword8 (RateLimitIp ip) newPass
      pure (safePass, Just newPass)
  pid <- wrapClientE $ DB.insertAccount name safePass url descr
  let gen = mkVerificationCodeGen email
  code <-
    lift . liftSem $
      createCodeOverwritePrevious
        gen
        IdentityVerification
        (Retries 3)
        (Timeout (3600 * 24)) -- 24h
        (Just (toUUID pid))
  let key = codeKey code
  let value = codeValue code
  lift $ sendActivationMail name email key value False
  pure $ Public.NewProviderResponse pid newPass

activateAccountKey ::
  ( Member GalleyAPIAccess r,
    Member EmailSending r,
    Member VerificationCodeSubsystem r
  ) =>
  Code.Key ->
  Code.Value ->
  (Handler r) (Maybe Public.ProviderActivationResponse)
activateAccountKey key value = do
  guardSecondFactorDisabled Nothing
  c <- (lift . liftSem $ verifyCode key IdentityVerification value) >>= maybeInvalidCode
  (pid, email) <- case (codeAccount c, Just (codeFor c)) of
    (Just p, Just e) -> pure (Id p, e)
    _ -> throwStd (errorToWai @'E.InvalidCode)
  (name, memail, _url, _descr) <- wrapClientE (DB.lookupAccountData pid) >>= maybeInvalidCode
  case memail of
    Just email' | email == email' -> pure Nothing
    Just email' -> do
      -- Ensure we remove any pending password reset
      let gen = mkVerificationCodeGen email'
      lift $ liftSem $ deleteCode gen.genKey VerificationCode.PasswordReset
      -- Activate the new and remove the old key
      activate pid (Just email') email
      pure . Just $ Public.ProviderActivationResponse email
    -- Immediate approval for everybody (for now).
    Nothing -> do
      activate pid Nothing email
      lift $ sendApprovalConfirmMail name email
      pure . Just $ Public.ProviderActivationResponse email

getActivationCode ::
  ( Member GalleyAPIAccess r,
    Member VerificationCodeSubsystem r
  ) =>
  EmailAddress ->
  (Handler r) Code.KeyValuePair
getActivationCode email = do
  guardSecondFactorDisabled Nothing
  let gen = mkVerificationCodeGen email
  code <- lift . liftSem $ internalLookupCode gen.genKey IdentityVerification
  maybe (throwStd activationKeyNotFound) (pure . codeToKeyValuePair) code

getPasswordResetCode ::
  (Member VerificationCodeSubsystem r) =>
  EmailAddress ->
  Handler r Code.KeyValuePair
getPasswordResetCode email = do
  let gen = mkVerificationCodeGen email
  code <- lift . liftSem $ internalLookupCode gen.genKey VerificationCode.PasswordReset
  maybe (throwStd $ notFound "Password reset key not found.") (pure . codeToKeyValuePair) code

login ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member CryptoSign r,
    Member Now r
  ) =>
  ProviderLogin ->
  Handler r ProviderTokenCookie
login l = do
  guardSecondFactorDisabled Nothing
  pid <-
    wrapClientE (DB.lookupKey (mkEmailKey (providerLoginEmail l)))
      >>= maybeBadCredentials
  unlessM (fst <$> (lift . liftSem $ Authentication.verifyProviderPassword pid l.providerLoginPassword)) do
    throwStd (errorToWai @E.BadCredentials)
  token <- lift . liftSem $ ZAuth.newProviderToken pid
  s <- asks (.settings)
  pure $ ProviderTokenCookie (ProviderToken token) (not s.cookieInsecure)

beginPasswordReset :: (Member GalleyAPIAccess r, Member EmailSending r, Member VerificationCodeSubsystem r) => Public.PasswordReset -> (Handler r) ()
beginPasswordReset (Public.PasswordReset target) = do
  guardSecondFactorDisabled Nothing
  pid <- wrapClientE (DB.lookupKey (mkEmailKey target)) >>= maybeBadCredentials
  let gen = mkVerificationCodeGen target
  (lift . liftSem $ createCode gen VerificationCode.PasswordReset (Retries 3) (Timeout 3600) (Just (toUUID pid))) >>= \case
    Left (CodeAlreadyExists code) ->
      -- FUTUREWORK: use subsystem error
      throwE $ pwResetError (PasswordResetInProgress $ Just code.codeTTL)
    Right code ->
      lift $ sendPasswordResetMail target (code.codeKey) (code.codeValue)

completePasswordReset ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r,
    Member VerificationCodeSubsystem r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  Public.CompletePasswordReset ->
  (Handler r) ()
completePasswordReset (Public.CompletePasswordReset key value newpwd) = do
  guardSecondFactorDisabled Nothing
  code <- (lift . liftSem $ verifyCode key VerificationCode.PasswordReset value) >>= maybeInvalidCode
  case Id <$> code.codeAccount of
    Nothing -> throwStd (errorToWai @E.InvalidPasswordResetCode)
    Just pid -> do
      whenM (fst <$> (lift . liftSem $ Authentication.verifyProviderPassword pid newpwd)) do
        throwStd (errorToWai @E.ResetPasswordMustDiffer)
      hashedPwd <- lift . liftSem $ HashPassword.hashPassword6 (RateLimitProvider pid) newpwd
      wrapClientE $ DB.updateAccountPassword pid hashedPwd
      lift . liftSem $ deleteCode key VerificationCode.PasswordReset

--------------------------------------------------------------------------------
-- Provider API

getAccount :: (Member GalleyAPIAccess r) => ProviderId -> (Handler r) (Maybe Public.Provider)
getAccount pid = do
  guardSecondFactorDisabled Nothing
  wrapClientE $ DB.lookupAccount pid

updateAccountProfile :: (Member GalleyAPIAccess r) => ProviderId -> Public.UpdateProvider -> (Handler r) ()
updateAccountProfile pid upd = do
  guardSecondFactorDisabled Nothing
  _ <- wrapClientE (DB.lookupAccount pid) >>= maybeInvalidProvider
  wrapClientE $
    DB.updateAccountProfile
      pid
      (updateProviderName upd)
      (updateProviderUrl upd)
      (updateProviderDescr upd)

updateAccountEmail ::
  ( Member GalleyAPIAccess r,
    Member EmailSending r,
    Member VerificationCodeSubsystem r
  ) =>
  ProviderId ->
  Public.EmailUpdate ->
  (Handler r) ()
updateAccountEmail pid (Public.EmailUpdate email) = do
  guardSecondFactorDisabled Nothing
  let emailKey = mkEmailKey email
  wrapClientE (DB.lookupKey emailKey) >>= mapM_ (const $ throwStd emailExists)
  let gen = mkVerificationCodeGen email
  code <-
    lift . liftSem $
      createCodeOverwritePrevious
        gen
        IdentityVerification
        (Retries 3)
        (Timeout (3600 * 24)) -- 24h
        (Just (toUUID pid))
  lift $ sendActivationMail (Name "name") email code.codeKey code.codeValue True

updateAccountPassword ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  ProviderId ->
  Public.PasswordChange ->
  (Handler r) ()
updateAccountPassword pid upd = do
  guardSecondFactorDisabled Nothing
  unlessM (fst <$> (lift . liftSem $ Authentication.verifyProviderPassword pid upd.oldPassword)) do
    throwStd (errorToWai @E.BadCredentials)
  whenM (fst <$> (lift . liftSem $ Authentication.verifyProviderPassword pid upd.newPassword)) do
    throwStd (errorToWai @E.ResetPasswordMustDiffer)
  hashedPwd <- lift . liftSem $ HashPassword.hashPassword6 (RateLimitProvider pid) upd.newPassword
  wrapClientE $ DB.updateAccountPassword pid hashedPwd

addService ::
  (Member GalleyAPIAccess r) =>
  ProviderId ->
  Public.NewService ->
  (Handler r) Public.NewServiceResponse
addService pid new = do
  guardSecondFactorDisabled Nothing
  _ <- wrapClientE (DB.lookupAccount pid) >>= maybeInvalidProvider
  let name = newServiceName new
  let summary = fromRange (newServiceSummary new)
  let descr = fromRange (newServiceDescr new)
  let baseUrl = newServiceUrl new
  let pubkey = newServiceKey new
  let assets = newServiceAssets new
  let tags = fromRange (newServiceTags new)
  (pk, fp) <- validateServiceKey pubkey >>= maybeInvalidServiceKey
  token <- maybe randServiceToken pure (newServiceToken new)
  sid <- wrapClientE $ DB.insertService pid name summary descr baseUrl token pk fp assets tags
  let rstoken = maybe (Just token) (const Nothing) (newServiceToken new)
  pure $ Public.NewServiceResponse sid rstoken

listServices :: (Member GalleyAPIAccess r) => ProviderId -> (Handler r) [Public.Service]
listServices pid = do
  guardSecondFactorDisabled Nothing
  wrapClientE $ DB.listServices pid

getService ::
  (Member GalleyAPIAccess r) =>
  ProviderId ->
  ServiceId ->
  (Handler r) Public.Service
getService pid sid = do
  guardSecondFactorDisabled Nothing
  wrapClientE (DB.lookupService pid sid) >>= maybeServiceNotFound

updateService ::
  (Member GalleyAPIAccess r) =>
  ProviderId ->
  ServiceId ->
  Public.UpdateService ->
  Handler r ()
updateService pid sid upd = do
  guardSecondFactorDisabled Nothing
  _ <- wrapClientE (DB.lookupAccount pid) >>= maybeInvalidProvider
  -- Update service profile
  svc <- wrapClientE (DB.lookupService pid sid) >>= maybeServiceNotFound
  let name = serviceName svc
  let newName = updateServiceName upd
  let nameChange = fmap (name,) newName
  let tags = unsafeRange (serviceTags svc)
  let newTags = updateServiceTags upd
  let tagsChange = fmap (tags,) (rcast <$> newTags)
  let newSummary = fromRange <$> updateServiceSummary upd
  let newDescr = fromRange <$> updateServiceDescr upd
  let newAssets = updateServiceAssets upd
  -- Update service, tags/prefix index if the service is enabled
  wrapClientE $
    DB.updateService
      pid
      sid
      name
      tags
      nameChange
      newSummary
      newDescr
      newAssets
      tagsChange
      (serviceEnabled svc)

updateServiceConn ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r
  ) =>
  ProviderId ->
  ServiceId ->
  Public.UpdateServiceConn ->
  Handler r ()
updateServiceConn pid sid upd = do
  guardSecondFactorDisabled Nothing
  unlessM (fst <$> (lift . liftSem $ Authentication.verifyProviderPassword pid upd.updateServiceConnPassword)) $
    throwStd (errorToWai @E.BadCredentials)
  scon <- wrapClientE (DB.lookupServiceConn pid sid) >>= maybeServiceNotFound
  svc <- wrapClientE (DB.lookupServiceProfile pid sid) >>= maybeServiceNotFound
  let newBaseUrl = updateServiceConnUrl upd
  let newTokens = maybeList1 . fromRange =<< updateServiceConnTokens upd
  let newEnabled = updateServiceConnEnabled upd
  let newKeyPems = fromRange <$> updateServiceConnKeys upd
  keys <- forM newKeyPems (mapM (validateServiceKey >=> maybeInvalidServiceKey))
  let newKeys = keys >>= maybeList1
  let newFps = fmap snd <$> newKeys
  wrapClientE $ DB.updateServiceConn pid sid newBaseUrl newTokens newKeys newEnabled
  let scon' =
        scon
          { sconBaseUrl = fromMaybe (sconBaseUrl scon) newBaseUrl,
            sconAuthTokens = fromMaybe (sconAuthTokens scon) newTokens,
            sconFingerprints = fromMaybe (sconFingerprints scon) newFps,
            sconEnabled = fromMaybe (sconEnabled scon) newEnabled
          }
  when (sconEnabled scon || sconEnabled scon') $ do
    lift $ RPC.setServiceConn scon'
    -- If the service got enabled or disabled, update the tag index.
    unless (sconEnabled scon && sconEnabled scon') $ do
      let name = serviceProfileName svc
      let tags = unsafeRange (serviceProfileTags svc)
      -- Update index, make it visible over search
      wrapClientE $
        if sconEnabled scon
          then DB.deleteServiceIndexes pid sid name tags
          else DB.insertServiceIndexes pid sid name tags

-- TODO: Send informational email to provider.

-- | The endpoint that is called to delete a service.
--
-- Since deleting a service can be costly, it just marks the service as
-- disabled and then creates an event that will, when processed, actually
-- delete the service. See 'finishDeleteService'.
deleteService ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r,
    Member DeleteQueue r
  ) =>
  ProviderId ->
  ServiceId ->
  Public.DeleteService ->
  (Handler r) ()
deleteService pid sid del = do
  guardSecondFactorDisabled Nothing
  unlessM (fst <$> (lift . liftSem $ Authentication.verifyProviderPassword pid del.deleteServicePassword)) do
    throwStd (errorToWai @E.BadCredentials)
  _ <- wrapClientE (DB.lookupService pid sid) >>= maybeServiceNotFound
  -- Disable the service
  wrapClientE $ DB.updateServiceConn pid sid Nothing Nothing Nothing (Just False)
  -- Create an event
  lift . liftSem $ enqueueServiceDeletion pid sid

finishDeleteService ::
  ( MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m,
    MonadClient m,
    MonadUnliftIO m
  ) =>
  ProviderId ->
  ServiceId ->
  m ()
finishDeleteService pid sid = do
  mbSvc <- DB.lookupService pid sid
  for_ mbSvc $ \svc -> do
    let tags = unsafeRange (serviceTags svc)
        name = serviceName svc
    runConduit $
      User.lookupServiceUsers pid sid
        .| C.mapM_ (pooledMapConcurrentlyN_ 16 kick)
    RPC.removeServiceConn pid sid
    DB.deleteService pid sid name tags
  where
    kick (bid, cid, _) = deleteBot (botUserId bid) Nothing bid cid

deleteAccount ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r
  ) =>
  ProviderId ->
  Public.DeleteProvider ->
  (Handler r) ()
deleteAccount pid del = do
  guardSecondFactorDisabled Nothing
  prov <- wrapClientE (DB.lookupAccount pid) >>= maybeInvalidProvider
  -- We don't care about pwd update status (scrypt, argon2id etc) when deleting things
  unlessM (fst <$> (lift . liftSem $ Authentication.verifyProviderPassword pid del.deleteProviderPassword)) do
    throwStd (errorToWai @E.BadCredentials)
  svcs <- wrapClientE $ DB.listServices pid
  forM_ svcs $ \svc -> do
    let sid = serviceId svc
    let tags = unsafeRange (serviceTags svc)
        name = serviceName svc
    lift $ wrapHttpClient $ RPC.removeServiceConn pid sid
    wrapClientE $ DB.deleteService pid sid name tags
  wrapClientE $ DB.deleteKey (mkEmailKey (providerEmail prov))
  wrapClientE $ DB.deleteAccount pid

--------------------------------------------------------------------------------
-- User API

getProviderProfile :: (Member GalleyAPIAccess r) => UserId -> ProviderId -> (Handler r) (Maybe Public.ProviderProfile)
getProviderProfile _ pid = do
  guardSecondFactorDisabled Nothing
  wrapClientE (DB.lookupAccountProfile pid)

listServiceProfiles :: (Member GalleyAPIAccess r) => UserId -> ProviderId -> (Handler r) [Public.ServiceProfile]
listServiceProfiles _ pid = do
  guardSecondFactorDisabled Nothing
  wrapClientE $ DB.listServiceProfiles pid

getServiceProfile :: (Member GalleyAPIAccess r) => UserId -> ProviderId -> ServiceId -> (Handler r) Public.ServiceProfile
getServiceProfile _ pid sid = do
  guardSecondFactorDisabled Nothing
  wrapClientE (DB.lookupServiceProfile pid sid) >>= maybeServiceNotFound

-- TODO: in order to actually make it possible for clients to implement
-- pagination here, we need both 'start' and 'prefix'.
--
-- Also see Note [buggy pagination].
searchServiceProfiles :: (Member GalleyAPIAccess r) => UserId -> Maybe (Public.QueryAnyTags 1 3) -> Maybe Text -> Maybe (Range 10 100 Int32) -> (Handler r) Public.ServiceProfilePage
searchServiceProfiles _ Nothing (Just start) mSize = do
  guardSecondFactorDisabled Nothing
  prefix :: Range 1 128 Text <- rangeChecked start
  let size = fromMaybe (unsafeRange 20) mSize
  wrapClientE . DB.paginateServiceNames (Just prefix) (fromRange size) =<< asks (.settings.providerSearchFilter)
searchServiceProfiles _ (Just tags) start mSize = do
  guardSecondFactorDisabled Nothing
  let size = fromMaybe (unsafeRange 20) mSize
  (wrapClientE . DB.paginateServiceTags tags start (fromRange size))
    =<< asks (.settings.providerSearchFilter)
searchServiceProfiles _ Nothing Nothing _ = do
  guardSecondFactorDisabled Nothing
  throwStd $ badRequest "At least `tags` or `start` must be provided."

-- NB: unlike 'searchServiceProfiles', we don't filter by service provider here
searchTeamServiceProfiles ::
  UserId ->
  TeamId ->
  Maybe (Range 1 128 Text) ->
  Maybe Bool ->
  Maybe (Range 10 100 Int32) ->
  (Handler r) Public.ServiceProfilePage
searchTeamServiceProfiles uid tid prefix mFilterDisabled mSize = do
  -- Check that the user actually belong to the team they claim they
  -- belong to. (Note: the 'tid' team might not even exist but we'll throw
  -- 'insufficientTeamPermissions' anyway)
  let filterDisabled = fromMaybe True mFilterDisabled
  let size = fromMaybe (unsafeRange 20) mSize
  teamId <- lift $ wrapClient $ User.lookupUserTeam uid
  unless (Just tid == teamId) $
    throwStd insufficientTeamPermissions
  -- Get search results
  wrapClientE $ DB.paginateServiceWhitelist tid prefix filterDisabled (fromRange size)

getServiceTagList :: (Member GalleyAPIAccess r) => UserId -> (Handler r) Public.ServiceTagList
getServiceTagList _ = do
  guardSecondFactorDisabled Nothing
  pure (Public.ServiceTagList allTags)
  where
    allTags = [(minBound :: Public.ServiceTag) ..]

updateServiceWhitelist ::
  ( Member GalleyAPIAccess r,
    Member (Error UserSubsystemError) r
  ) =>
  UserId ->
  ConnId ->
  TeamId ->
  Public.UpdateServiceWhitelist ->
  (Handler r) UpdateServiceWhitelistResp
updateServiceWhitelist uid con tid upd = do
  -- Preconditions
  guardSecondFactorDisabled (Just uid)
  guardMLSNotDefault
  let pid = updateServiceWhitelistProvider upd
      sid = updateServiceWhitelistService upd
      newWhitelisted = updateServiceWhitelistStatus upd
  lift . liftSem $ ensurePermissions uid tid (Set.toList serviceWhitelistPermissions)
  _ <- wrapClientE (DB.lookupService pid sid) >>= maybeServiceNotFound
  -- Add to various tables
  whitelisted <- wrapClientE $ DB.getServiceWhitelistStatus tid pid sid
  case (whitelisted, newWhitelisted) of
    (False, False) -> pure UpdateServiceWhitelistRespUnchanged
    (True, True) -> pure UpdateServiceWhitelistRespUnchanged
    (False, True) -> do
      wrapClientE $ DB.insertServiceWhitelist tid pid sid
      pure UpdateServiceWhitelistRespChanged
    (True, False) -> do
      -- When the service is de-whitelisted, remove its bots from team
      -- conversations
      lift
        $ fmap
          wrapHttpClient
          runConduit
        $ User.lookupServiceUsersForTeam pid sid tid
          .| C.mapM_
            ( pooledMapConcurrentlyN_
                16
                ( uncurry (deleteBot uid (Just con))
                )
            )
      wrapClientE $ DB.deleteServiceWhitelist (Just tid) pid sid
      pure UpdateServiceWhitelistRespChanged
  where
    guardMLSNotDefault = lift . liftSem $ do
      feat <- GalleyAPIAccess.getFeatureConfigForTeam @_ @Feature.MLSConfig tid
      let defProtocol = feat.config.mlsDefaultProtocol
      case defProtocol of
        ProtocolProteusTag -> pure ()
        ProtocolMLSTag -> throw UserSubsystemMLSServicesNotAllowed
        ProtocolMixedTag -> throw UserSubsystemMLSServicesNotAllowed

--------------------------------------------------------------------------------
-- Bot API

addBot ::
  ( Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member Now r,
    Member CryptoSign r
  ) =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.AddBot ->
  (Handler r) Public.AddBotResponse
addBot zuid zcon cid add = do
  guardSecondFactorDisabled (Just zuid)
  zusr <- lift (wrapClient $ User.lookupUser NoPendingInvitations zuid) >>= maybeInvalidUser
  let pid = addBotProvider add
  let sid = addBotService add
  -- Get the conversation and check preconditions
  lcid <- qualifyLocal cid
  cnv <- lift (liftSem $ GalleyAPIAccess.getConv zuid lcid) >>= maybeConvNotFound
  -- Check that the user is a conversation admin and therefore is allowed to add a bot to this conversation.
  -- Note that this precondition is also checked in the internal galley API,
  -- but by having this check here we prevent any (useless) data to be written to the database
  -- as well as the unnecessary creation of the bot via the external service API call.
  -- However, in case we refine the roles model in the future, this check might not be granular enough.
  -- In that case we should rather do an internal call to galley to check for the correct permissions.
  -- Also see `removeBot` for a similar check.
  guardConvAdmin cnv
  let mems = cnvMembers cnv
  unless (cnvType cnv == RegularConv) $
    throwStd (errorToWai @'E.InvalidConversation)
  maxSize <- fromIntegral <$> asks (.settings.maxConvSize)
  unless (length (cmOthers mems) < maxSize - 1) $
    throwStd (errorToWai @'E.TooManyConversationMembers)
  -- For team conversations: bots are not allowed in
  -- team-only conversations
  unless (Set.member ServiceAccessRole (cnvAccessRoles cnv)) $
    throwStd (errorToWai @'E.InvalidConversation)
  -- Lookup the relevant service data
  scon <- wrapClientE (DB.lookupServiceConn pid sid) >>= maybeServiceNotFound
  unless (sconEnabled scon) $
    throwStd (errorToWai @'E.ServiceDisabled)
  svp <- wrapClientE (DB.lookupServiceProfile pid sid) >>= maybeServiceNotFound
  for_ (cnvTeam cnv) $ \tid -> do
    whitelisted <- wrapClientE $ DB.getServiceWhitelistStatus tid pid sid
    unless whitelisted $
      throwStd serviceNotWhitelisted
  -- Prepare a user ID, client ID and token for the bot.
  bid <- BotId <$> randomId
  domain <- viewFederationDomain
  btk <- lift . liftSem $ Text.decodeLatin1 . toByteString' <$> ZAuth.newBotToken pid bid cid
  let bcl = ClientId (fromIntegral (hash bid))
  -- Ask the external service to create a bot
  let zQualifiedUid = Qualified zuid domain
  let origmem = OtherMember zQualifiedUid Nothing roleNameWireAdmin
  let members = origmem : cmOthers mems
  let bcnv = Ext.botConvView (qUnqualified . cnvQualifiedId $ cnv) (cnvName cnv) members
  let busr = mkBotUserView zusr
  let bloc = fromMaybe (userLocale zusr) (addBotLocale add)
  let botReq = NewBotRequest bid bcl busr bcnv btk bloc
  rs <- RPC.createBot scon botReq !>> StdError . serviceError
  -- Insert the bot user and client
  locale <- Opt.defaultUserLocale <$> asks (.settings)
  let name = fromMaybe (serviceProfileName svp) (Ext.rsNewBotName rs)
  let assets = fromMaybe (serviceProfileAssets svp) (Ext.rsNewBotAssets rs)
  let colour = fromMaybe defaultAccentId (Ext.rsNewBotColour rs)
  let pict = Pict [] -- Legacy
  let sref = newServiceRef sid pid
  let usr = User (Qualified (botUserId bid) domain) Nothing Nothing name Nothing pict assets colour Active locale (Just sref) Nothing Nothing Nothing ManagedByWire defSupportedProtocols
  let newClt =
        (newClient PermanentClientType (Ext.rsNewBotLastPrekey rs))
          { newClientPrekeys = Ext.rsNewBotPrekeys rs
          }
  lift $ wrapClient $ User.insertAccount usr (Just (cid, cnvTeam cnv)) Nothing True
  maxPermClients <- fromMaybe Opt.defUserMaxPermClients <$> asks (.settings.userMaxPermClients)
  (clt, _, _) <- do
    _ <- do
      -- if we want to protect bots against lh, 'addClient' cannot just send lh capability
      -- implicitly in the next line.
      pure $ FutureWork @'UnprotectedBot undefined
    lbid <- qualifyLocal (botUserId bid)
    ( User.addClient
        lbid
        bcl
        newClt
        maxPermClients
        ( Just $ ClientCapabilityList $ Set.singleton Public.ClientSupportsLegalholdImplicitConsent
        )
      )
      !>> const (StdError $ badGatewayWith "MalformedPrekeys")

  -- Add the bot to the conversation
  ev <- lift $ RPC.addBotMember zuid zcon cid bid (clientId clt) pid sid
  pure $
    Public.AddBotResponse
      { Public.rsAddBotId = bid,
        Public.rsAddBotClient = bcl,
        Public.rsAddBotName = name,
        Public.rsAddBotColour = colour,
        Public.rsAddBotAssets = assets,
        Public.rsAddBotEvent = ev
      }

removeBot :: (Member GalleyAPIAccess r) => UserId -> ConnId -> ConvId -> BotId -> (Handler r) (Maybe Public.RemoveBotResponse)
removeBot zusr zcon cid bid = do
  guardSecondFactorDisabled (Just zusr)
  -- Get the conversation and check preconditions
  lcid <- qualifyLocal cid
  cnv <- lift (liftSem $ GalleyAPIAccess.getConv zusr lcid) >>= maybeConvNotFound
  -- Check that the user is a conversation admin and therefore is allowed to remove a bot from the conversation.
  -- Note that this precondition is also checked in the internal galley API.
  -- However, in case we refine the roles model in the future, this check might not be granular enough.
  -- In that case we should rather do an internal call to galley to check for the correct permissions.
  -- Also see `addBot` for a similar check.
  guardConvAdmin cnv
  let mems = cnvMembers cnv
  unless (cnvType cnv == RegularConv) $
    (throwStd (errorToWai @'E.InvalidConversation))
  -- Find the bot in the member list and delete it
  let busr = botUserId bid
  let bot = List.find ((== busr) . qUnqualified . omQualifiedId) (cmOthers mems)
  case bot >>= omService of
    Nothing -> pure Nothing
    Just _ -> do
      lift $ Public.RemoveBotResponse <$$> wrapHttpClient (deleteBot zusr (Just zcon) bid cid)

guardConvAdmin :: Conversation -> ExceptT HttpError (AppT r) ()
guardConvAdmin conv = do
  let selfMember = cmSelf . cnvMembers $ conv
  unless (memConvRoleName selfMember == roleNameWireAdmin) $ (throwStd (errorToWai @'E.AccessDenied))

botGetSelf :: BotId -> (Handler r) Public.UserProfile
botGetSelf bot = do
  p <- lift $ wrapClient $ User.lookupUser NoPendingInvitations (botUserId bot)
  maybe (throwStd (errorToWai @'E.UserNotFound)) (\u -> pure $ Public.mkUserProfile EmailVisibleToSelf u UserLegalHoldNoConsent) p

botGetClient :: (Member GalleyAPIAccess r) => BotId -> (Handler r) (Maybe Public.Client)
botGetClient bot = do
  guardSecondFactorDisabled (Just (botUserId bot))
  lift $ listToMaybe <$> wrapClient (User.lookupClients (botUserId bot))

botListPrekeys :: (Member GalleyAPIAccess r) => BotId -> (Handler r) [Public.PrekeyId]
botListPrekeys bot = do
  guardSecondFactorDisabled (Just (botUserId bot))
  clt <- lift $ listToMaybe <$> wrapClient (User.lookupClients (botUserId bot))
  case clientId <$> clt of
    Nothing -> pure []
    Just ci -> lift (wrapClient $ User.lookupPrekeyIds (botUserId bot) ci)

botUpdatePrekeys :: (Member GalleyAPIAccess r) => BotId -> Public.UpdateBotPrekeys -> (Handler r) ()
botUpdatePrekeys bot upd = do
  guardSecondFactorDisabled (Just (botUserId bot))
  clt <- lift $ listToMaybe <$> wrapClient (User.lookupClients (botUserId bot))
  case clt of
    Nothing -> throwStd (errorToWai @'E.ClientNotFound)
    Just c -> do
      let pks = updateBotPrekeyList upd
      wrapClientE (User.updatePrekeys (botUserId bot) (clientId c) pks) !>> clientDataError

botClaimUsersPrekeys ::
  ( Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member DeleteQueue r,
    Member SessionStore r
  ) =>
  BotId ->
  Public.UserClients ->
  Handler r Public.UserClientPrekeyMap
botClaimUsersPrekeys _ body = do
  guardSecondFactorDisabled Nothing
  maxSize <- fromIntegral <$> asks (.settings.maxConvSize)
  when (Map.size (Public.userClients body) > maxSize) $
    throwStd (errorToWai @'E.TooManyClients)
  Client.claimLocalMultiPrekeyBundles UnprotectedBot body !>> clientError

botListUserProfiles :: (Member GalleyAPIAccess r) => BotId -> (CommaSeparatedList UserId) -> (Handler r) [Public.BotUserView]
botListUserProfiles _ uids = do
  guardSecondFactorDisabled Nothing -- should we check all user ids?
  us <- lift . wrapClient $ User.lookupUsers NoPendingInvitations (fromCommaSeparatedList uids)
  pure (map mkBotUserView us)

botGetUserClients :: (Member GalleyAPIAccess r) => BotId -> UserId -> (Handler r) [Public.PubClient]
botGetUserClients _ uid = do
  guardSecondFactorDisabled (Just uid)
  lift $ pubClient <$$> wrapClient (User.lookupClients uid)
  where
    pubClient c = Public.PubClient (clientId c) (clientClass c)

botDeleteSelf :: (Member GalleyAPIAccess r) => BotId -> ConvId -> (Handler r) ()
botDeleteSelf bid cid = do
  guardSecondFactorDisabled (Just (botUserId bid))
  bot <- lift . wrapClient $ User.lookupUser NoPendingInvitations (botUserId bid)
  _ <- maybe (throwStd (errorToWai @'E.InvalidBot)) pure $ (userService =<< bot)
  _ <- lift $ wrapHttpClient $ deleteBot (botUserId bid) Nothing bid cid
  pure ()

--------------------------------------------------------------------------------
-- Utilities

-- | If second factor auth is enabled, make sure that end-points that don't support it, but should, are blocked completely.
-- (This is a workaround until we have 2FA for those end-points as well.)
guardSecondFactorDisabled ::
  (Member GalleyAPIAccess r) =>
  Maybe UserId ->
  ExceptT HttpError (AppT r) ()
guardSecondFactorDisabled mbUserId = do
  feat <- lift $ liftSem $ GalleyAPIAccess.getAllTeamFeaturesForUser mbUserId
  let enabled =
        (Feature.npProject @Feature.SndFactorPasswordChallengeConfig feat).status
          == Feature.FeatureStatusEnabled
  when enabled do
    throwStd $ errorToWai @'E.AccessDenied

minRsaKeySize :: Int
minRsaKeySize = 256 -- Bytes (= 2048 bits)

activate :: ProviderId -> Maybe EmailAddress -> EmailAddress -> (Handler r) ()
activate pid old new = do
  let emailKey = mkEmailKey new
  taken <- maybe False (/= pid) <$> wrapClientE (DB.lookupKey emailKey)
  when taken $
    throwStd emailExists
  wrapClientE $ DB.insertKey pid (mkEmailKey <$> old) emailKey

deleteBot ::
  ( MonadHttp m,
    MonadReader Env m,
    MonadMask m,
    MonadUnliftIO m,
    HasRequestId m,
    MonadLogger m,
    MonadClient m
  ) =>
  UserId ->
  Maybe ConnId ->
  BotId ->
  ConvId ->
  m (Maybe Public.Event)
deleteBot zusr zcon bid cid = do
  -- Remove the bot from the conversation
  ev <- RPC.removeBotMember zusr zcon cid bid
  -- Delete the bot user and client
  let buid = botUserId bid
  mbUser <- User.lookupUser NoPendingInvitations buid
  User.lookupClients buid >>= mapM_ (User.rmClient buid . clientId)
  for_ (userService =<< mbUser) $ \sref -> do
    let pid = sref ^. serviceRefProvider
        sid = sref ^. serviceRefId
    User.deleteServiceUser pid sid bid
  -- TODO: Consider if we can actually delete the bot user entirely,
  -- i.e. not just marking the account as deleted.
  void $ runExceptT $ User.updateStatus buid Deleted
  pure ev

validateServiceKey :: (MonadIO m) => Public.ServiceKeyPEM -> m (Maybe (Public.ServiceKey, Fingerprint Rsa))
validateServiceKey pem =
  liftIO $
    readPublicKey >>= \pk ->
      case SSL.toPublicKey =<< pk of
        Nothing -> pure Nothing
        Just pk' -> do
          Just sha <- SSL.getDigestByName "SHA256"
          let size = SSL.rsaSize (pk' :: SSL.RSAPubKey)
          if size < minRsaKeySize
            then pure Nothing
            else do
              fpr <- Fingerprint <$> SSL.rsaFingerprint sha pk'
              let bits = fromIntegral size * 8
              let key = Public.ServiceKey Public.RsaServiceKey bits pem
              pure $ Just (key, fpr)
  where
    readPublicKey =
      handleAny
        (const $ pure Nothing)
        (SSL.readPublicKey (LC8.unpack (toByteString pem)) <&> Just)

mkBotUserView :: User -> Public.BotUserView
mkBotUserView u =
  Ext.BotUserView
    { Ext.botUserViewId = userId u,
      Ext.botUserViewName = userDisplayName u,
      Ext.botUserViewColour = userAccentId u,
      Ext.botUserViewHandle = userHandle u,
      Ext.botUserViewTeam = userTeam u
    }

maybeInvalidProvider :: (Monad m) => Maybe a -> (ExceptT HttpError m) a
maybeInvalidProvider = maybe (throwStd (errorToWai @'E.ProviderNotFound)) pure

maybeInvalidCode :: (Monad m) => Maybe a -> (ExceptT HttpError m) a
maybeInvalidCode = maybe (throwStd (errorToWai @'E.InvalidCode)) pure

maybeServiceNotFound :: (Monad m) => Maybe a -> (ExceptT HttpError m) a
maybeServiceNotFound = maybe (throwStd (errorToWai @'E.ServiceNotFound)) pure

maybeConvNotFound :: (Monad m) => Maybe a -> (ExceptT HttpError m) a
maybeConvNotFound = maybe (throwStd (notFound "Conversation not found")) pure

maybeBadCredentials :: (Monad m) => Maybe a -> (ExceptT HttpError m) a
maybeBadCredentials = maybe (throwStd (errorToWai @'E.BadCredentials)) pure

maybeInvalidServiceKey :: (Monad m) => Maybe a -> (ExceptT HttpError m) a
maybeInvalidServiceKey = maybe (throwStd (errorToWai @'E.InvalidServiceKey)) pure

maybeInvalidUser :: (Monad m) => Maybe a -> (ExceptT HttpError m) a
maybeInvalidUser = maybe (throwStd (errorToWai @'E.InvalidUser)) pure

rangeChecked :: (KnownNat n, KnownNat m, Within a n m, Monad monad) => a -> (ExceptT HttpError monad) (Range n m a)
rangeChecked = either (throwStd . invalidRange . fromString) pure . checkedEither

badGatewayWith :: String -> Wai.Error
badGatewayWith str = Wai.mkError status502 "bad-gateway" ("The upstream service returned an invalid response: " <> Text.pack str)

tooManyBots :: Wai.Error
tooManyBots = Wai.mkError status409 "too-many-bots" "Maximum number of bots for the service reached."

serviceNotWhitelisted :: Wai.Error
serviceNotWhitelisted = Wai.mkError status403 "service-not-whitelisted" "The desired service is not on the whitelist of allowed services for this team."

serviceError :: RPC.ServiceError -> Wai.Error
serviceError (RPC.ServiceUnavailableWith str) = badGatewayWith str
serviceError RPC.ServiceBotConflict = tooManyBots

randServiceToken :: (MonadIO m) => m Public.ServiceToken
randServiceToken = ServiceToken . Ascii.encodeBase64Url <$> liftIO (randBytes 18)
