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
    routesInternal,
    botAPI,
    servicesAPI,
    providerAPI,

    -- * Event handlers
    finishDeleteService,
  )
where

import Bilge.IO (MonadHttp)
import Bilge.RPC (HasRequestId)
import Brig.API.Client qualified as Client
import Brig.API.Error
import Brig.API.Handler
import Brig.API.Types (PasswordResetError (..), VerificationCodeThrottledError (VerificationCodeThrottled))
import Brig.API.Util
import Brig.App
import Brig.Code qualified as Code
import Brig.Data.Client qualified as User
import Brig.Data.User qualified as User
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.GalleyProvider qualified as GalleyProvider
import Brig.Email (mkEmailKey)
import Brig.InternalEvent.Types qualified as Internal
import Brig.Options (Settings (..))
import Brig.Options qualified as Opt
import Brig.Provider.DB (ServiceConn (..))
import Brig.Provider.DB qualified as DB
import Brig.Provider.Email
import Brig.Provider.RPC qualified as RPC
import Brig.Queue qualified as Queue
import Brig.Team.Util
import Brig.Types.User
import Brig.ZAuth qualified as ZAuth
import Cassandra (MonadClient)
import Control.Error (throwE)
import Control.Exception.Enclosed (handleAny)
import Control.Lens (view, (^.))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.CommaSeparatedList (CommaSeparatedList (fromCommaSeparatedList))
import Data.Conduit (runConduit, (.|))
import Data.Conduit.List qualified as C
import Data.Hashable (hash)
import Data.Id
import Data.LegalHold
import Data.List qualified as List
import Data.List1 (maybeList1)
import Data.Map.Strict qualified as Map
import Data.Misc (Fingerprint (..), FutureWork (FutureWork), Rsa)
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text
import GHC.TypeNats
import Imports
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate (accept)
import Network.Wai.Routing
import Network.Wai.Utilities.Error ((!>>))
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.ZAuth
import OpenSSL.EVP.Digest qualified as SSL
import OpenSSL.EVP.PKey qualified as SSL
import OpenSSL.PEM qualified as SSL
import OpenSSL.RSA qualified as SSL
import OpenSSL.Random (randBytes)
import Polysemy
import Servant (ServerT, (:<|>) (..))
import Ssl.Util qualified as SSL
import System.Logger.Class (MonadLogger)
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Bot qualified as Public
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
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Brig.Bot (BotAPI)
import Wire.API.Routes.Public.Brig.Provider (ProviderAPI)
import Wire.API.Routes.Public.Brig.Services (ServicesAPI)
import Wire.API.Team.Feature qualified as Feature
import Wire.API.Team.LegalHold (LegalholdProtectee (UnprotectedBot))
import Wire.API.Team.Permission
import Wire.API.User hiding (cpNewPassword, cpOldPassword)
import Wire.API.User qualified as Public (UserProfile, publicProfile)
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client qualified as Public (Client, ClientCapability (ClientSupportsLegalholdImplicitConsent), PubClient (..), UserClientPrekeyMap, UserClients, userClients)
import Wire.API.User.Client.Prekey qualified as Public (PrekeyId)
import Wire.API.User.Identity qualified as Public (Email)
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Unsafe))

botAPI ::
  ( Member GalleyProvider r,
    Member (Concurrency 'Unsafe) r
  ) =>
  ServerT BotAPI (Handler r)
botAPI =
  Named @"add-bot" addBot
    :<|> Named @"remove-bot" removeBot
    :<|> Named @"bot-get-self" botGetSelf
    :<|> Named @"bot-delete-self" botDeleteSelf
    :<|> Named @"bot-list-prekeys" botListPrekeys
    :<|> Named @"bot-update-prekeys" botUpdatePrekeys
    :<|> Named @"bot-get-client" botGetClient
    :<|> Named @"bot-claim-users-prekeys" botClaimUsersPrekeys
    :<|> Named @"bot-list-users" botListUserProfiles
    :<|> Named @"bot-get-user-clients" botGetUserClients

servicesAPI :: (Member GalleyProvider r) => ServerT ServicesAPI (Handler r)
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

providerAPI :: Member GalleyProvider r => ServerT ProviderAPI (Handler r)
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

routesInternal :: Member GalleyProvider r => Routes a (Handler r) ()
routesInternal = do
  get "/i/provider/activation-code" (continue getActivationCodeH) $
    accept "application" "json"
      .&> param "email"

--------------------------------------------------------------------------------
-- Public API (Unauthenticated)

newAccount :: Member GalleyProvider r => Public.NewProvider -> (Handler r) Public.NewProviderResponse
newAccount new = do
  guardSecondFactorDisabled Nothing
  email <- case validateEmail (Public.newProviderEmail new) of
    Right em -> pure em
    Left _ -> throwStd (errorToWai @'E.InvalidEmail)
  let name = Public.newProviderName new
  let pass = Public.newProviderPassword new
  let descr = fromRange (Public.newProviderDescr new)
  let url = Public.newProviderUrl new
  let emailKey = mkEmailKey email
  wrapClientE (DB.lookupKey emailKey) >>= mapM_ (const $ throwStd emailExists)
  (safePass, newPass) <- case pass of
    Just newPass -> (,Nothing) <$> mkSafePassword newPass
    Nothing -> do
      newPass <- genPassword
      safePass <- mkSafePassword newPass
      pure (safePass, Just newPass)
  pid <- wrapClientE $ DB.insertAccount name safePass url descr
  gen <- Code.mkGen (Code.ForEmail email)
  code <-
    Code.generate
      gen
      Code.IdentityVerification
      (Code.Retries 3)
      (Code.Timeout (3600 * 24)) -- 24h
      (Just (toUUID pid))
  tryInsertVerificationCode code $ verificationCodeThrottledError . VerificationCodeThrottled
  let key = Code.codeKey code
  let val = Code.codeValue code
  lift $ sendActivationMail name email key val False
  pure $ Public.NewProviderResponse pid newPass

activateAccountKey :: Member GalleyProvider r => Code.Key -> Code.Value -> (Handler r) (Maybe Public.ProviderActivationResponse)
activateAccountKey key val = do
  guardSecondFactorDisabled Nothing
  c <- wrapClientE (Code.verify key Code.IdentityVerification val) >>= maybeInvalidCode
  (pid, email) <- case (Code.codeAccount c, Code.codeForEmail c) of
    (Just p, Just e) -> pure (Id p, e)
    _ -> throwStd (errorToWai @'E.InvalidCode)
  (name, memail, _url, _descr) <- wrapClientE (DB.lookupAccountData pid) >>= maybeInvalidCode
  case memail of
    Just email' | email == email' -> pure Nothing
    Just email' -> do
      -- Ensure we remove any pending password reset
      gen <- Code.mkGen (Code.ForEmail email')
      lift $ wrapClient $ Code.delete (Code.genKey gen) Code.PasswordReset
      -- Activate the new and remove the old key
      activate pid (Just email') email
      pure . Just $ Public.ProviderActivationResponse email
    -- Immediate approval for everybody (for now).
    Nothing -> do
      activate pid Nothing email
      lift $ sendApprovalConfirmMail name email
      pure . Just $ Public.ProviderActivationResponse email

getActivationCodeH :: Member GalleyProvider r => Public.Email -> (Handler r) Response
getActivationCodeH e = do
  guardSecondFactorDisabled Nothing
  json <$> getActivationCode e

getActivationCode :: Public.Email -> (Handler r) FoundActivationCode
getActivationCode e = do
  email <- case validateEmail e of
    Right em -> pure em
    Left _ -> throwStd (errorToWai @'E.InvalidEmail)
  gen <- Code.mkGen (Code.ForEmail email)
  code <- wrapClientE $ Code.lookup (Code.genKey gen) Code.IdentityVerification
  maybe (throwStd activationKeyNotFound) (pure . FoundActivationCode) code

newtype FoundActivationCode = FoundActivationCode Code.Code

instance ToJSON FoundActivationCode where
  toJSON (FoundActivationCode vcode) =
    toJSON $
      Code.KeyValuePair (Code.codeKey vcode) (Code.codeValue vcode)

login :: Member GalleyProvider r => ProviderLogin -> Handler r ProviderTokenCookie
login l = do
  guardSecondFactorDisabled Nothing
  pid <- wrapClientE (DB.lookupKey (mkEmailKey (providerLoginEmail l))) >>= maybeBadCredentials
  pass <- wrapClientE (DB.lookupPassword pid) >>= maybeBadCredentials
  unless (verifyPassword (providerLoginPassword l) pass) $
    throwStd (errorToWai @'E.BadCredentials)
  token <- ZAuth.newProviderToken pid
  s <- view settings
  pure $ ProviderTokenCookie (ProviderToken token) (not (setCookieInsecure s))

beginPasswordReset :: Member GalleyProvider r => Public.PasswordReset -> (Handler r) ()
beginPasswordReset (Public.PasswordReset target) = do
  guardSecondFactorDisabled Nothing
  pid <- wrapClientE (DB.lookupKey (mkEmailKey target)) >>= maybeBadCredentials
  gen <- Code.mkGen (Code.ForEmail target)
  pending <- lift . wrapClient $ Code.lookup (Code.genKey gen) Code.PasswordReset
  code <- case pending of
    Just p -> throwE $ pwResetError (PasswordResetInProgress . Just $ Code.codeTTL p)
    Nothing ->
      Code.generate
        gen
        Code.PasswordReset
        (Code.Retries 3)
        (Code.Timeout 3600) -- 1h
        (Just (toUUID pid))
  tryInsertVerificationCode code $ verificationCodeThrottledError . VerificationCodeThrottled
  lift $ sendPasswordResetMail target (Code.codeKey code) (Code.codeValue code)

completePasswordReset :: Member GalleyProvider r => Public.CompletePasswordReset -> (Handler r) ()
completePasswordReset (Public.CompletePasswordReset key val newpwd) = do
  guardSecondFactorDisabled Nothing
  code <- wrapClientE (Code.verify key Code.PasswordReset val) >>= maybeInvalidCode
  case Id <$> Code.codeAccount code of
    Nothing -> throwStd (errorToWai @'E.InvalidPasswordResetCode)
    Just pid -> do
      oldpass <- wrapClientE (DB.lookupPassword pid) >>= maybeBadCredentials
      when (verifyPassword newpwd oldpass) $ do
        throwStd (errorToWai @'E.ResetPasswordMustDiffer)
      wrapClientE $ do
        DB.updateAccountPassword pid newpwd
        Code.delete key Code.PasswordReset

--------------------------------------------------------------------------------
-- Provider API

getAccount :: Member GalleyProvider r => ProviderId -> (Handler r) (Maybe Public.Provider)
getAccount pid = do
  guardSecondFactorDisabled Nothing
  wrapClientE $ DB.lookupAccount pid

updateAccountProfile :: Member GalleyProvider r => ProviderId -> Public.UpdateProvider -> (Handler r) ()
updateAccountProfile pid upd = do
  guardSecondFactorDisabled Nothing
  _ <- wrapClientE (DB.lookupAccount pid) >>= maybeInvalidProvider
  wrapClientE $
    DB.updateAccountProfile
      pid
      (updateProviderName upd)
      (updateProviderUrl upd)
      (updateProviderDescr upd)

updateAccountEmail :: Member GalleyProvider r => ProviderId -> Public.EmailUpdate -> (Handler r) ()
updateAccountEmail pid (Public.EmailUpdate new) = do
  guardSecondFactorDisabled Nothing
  email <- case validateEmail new of
    Right em -> pure em
    Left _ -> throwStd (errorToWai @'E.InvalidEmail)
  let emailKey = mkEmailKey email
  wrapClientE (DB.lookupKey emailKey) >>= mapM_ (const $ throwStd emailExists)
  gen <- Code.mkGen (Code.ForEmail email)
  code <-
    Code.generate
      gen
      Code.IdentityVerification
      (Code.Retries 3)
      (Code.Timeout (3600 * 24)) -- 24h
      (Just (toUUID pid))
  tryInsertVerificationCode code $ verificationCodeThrottledError . VerificationCodeThrottled
  lift $ sendActivationMail (Name "name") email (Code.codeKey code) (Code.codeValue code) True

updateAccountPassword :: Member GalleyProvider r => ProviderId -> Public.PasswordChange -> (Handler r) ()
updateAccountPassword pid upd = do
  guardSecondFactorDisabled Nothing
  pass <- wrapClientE (DB.lookupPassword pid) >>= maybeBadCredentials
  unless (verifyPassword (oldPassword upd) pass) $
    throwStd (errorToWai @'E.BadCredentials)
  when (verifyPassword (newPassword upd) pass) $
    throwStd (errorToWai @'E.ResetPasswordMustDiffer)
  wrapClientE $ DB.updateAccountPassword pid (newPassword upd)

addService ::
  Member GalleyProvider r =>
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

listServices :: Member GalleyProvider r => ProviderId -> (Handler r) [Public.Service]
listServices pid = do
  guardSecondFactorDisabled Nothing
  wrapClientE $ DB.listServices pid

getService ::
  Member GalleyProvider r =>
  ProviderId ->
  ServiceId ->
  (Handler r) Public.Service
getService pid sid = do
  guardSecondFactorDisabled Nothing
  wrapClientE (DB.lookupService pid sid) >>= maybeServiceNotFound

updateService ::
  Member GalleyProvider r =>
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
  Member GalleyProvider r =>
  ProviderId ->
  ServiceId ->
  Public.UpdateServiceConn ->
  Handler r ()
updateServiceConn pid sid upd = do
  guardSecondFactorDisabled Nothing
  pass <- wrapClientE (DB.lookupPassword pid) >>= maybeBadCredentials
  unless (verifyPassword (updateServiceConnPassword upd) pass) $
    throwStd (errorToWai @'E.BadCredentials)
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
  Member GalleyProvider r =>
  ProviderId ->
  ServiceId ->
  Public.DeleteService ->
  (Handler r) ()
deleteService pid sid del = do
  guardSecondFactorDisabled Nothing
  pass <- wrapClientE (DB.lookupPassword pid) >>= maybeBadCredentials
  -- We don't care about pwd status when deleting things
  unless (verifyPassword (deleteServicePassword del) pass) $
    throwStd (errorToWai @'E.BadCredentials)
  _ <- wrapClientE (DB.lookupService pid sid) >>= maybeServiceNotFound
  -- Disable the service
  wrapClientE $ DB.updateServiceConn pid sid Nothing Nothing Nothing (Just False)
  -- Create an event
  queue <- view internalEvents
  lift $ Queue.enqueue queue (Internal.DeleteService pid sid)

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
  ( Member GalleyProvider r
  ) =>
  ProviderId ->
  Public.DeleteProvider ->
  (Handler r) ()
deleteAccount pid del = do
  guardSecondFactorDisabled Nothing
  prov <- wrapClientE (DB.lookupAccount pid) >>= maybeInvalidProvider
  pass <- wrapClientE (DB.lookupPassword pid) >>= maybeBadCredentials
  -- We don't care about pwd status when deleting things
  unless (verifyPassword (deleteProviderPassword del) pass) $
    throwStd (errorToWai @'E.BadCredentials)
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

getProviderProfile :: Member GalleyProvider r => UserId -> ProviderId -> (Handler r) (Maybe Public.ProviderProfile)
getProviderProfile _ pid = do
  guardSecondFactorDisabled Nothing
  wrapClientE (DB.lookupAccountProfile pid)

listServiceProfiles :: Member GalleyProvider r => UserId -> ProviderId -> (Handler r) [Public.ServiceProfile]
listServiceProfiles _ pid = do
  guardSecondFactorDisabled Nothing
  wrapClientE $ DB.listServiceProfiles pid

getServiceProfile :: Member GalleyProvider r => UserId -> ProviderId -> ServiceId -> (Handler r) Public.ServiceProfile
getServiceProfile _ pid sid = do
  guardSecondFactorDisabled Nothing
  wrapClientE (DB.lookupServiceProfile pid sid) >>= maybeServiceNotFound

-- TODO: in order to actually make it possible for clients to implement
-- pagination here, we need both 'start' and 'prefix'.
--
-- Also see Note [buggy pagination].
searchServiceProfiles :: Member GalleyProvider r => UserId -> Maybe (Public.QueryAnyTags 1 3) -> Maybe Text -> Maybe (Range 10 100 Int32) -> (Handler r) Public.ServiceProfilePage
searchServiceProfiles _ Nothing (Just start) mSize = do
  guardSecondFactorDisabled Nothing
  prefix :: Range 1 128 Text <- rangeChecked start
  let size = fromMaybe (unsafeRange 20) mSize
  wrapClientE . DB.paginateServiceNames (Just prefix) (fromRange size) . setProviderSearchFilter =<< view settings
searchServiceProfiles _ (Just tags) start mSize = do
  guardSecondFactorDisabled Nothing
  let size = fromMaybe (unsafeRange 20) mSize
  (wrapClientE . DB.paginateServiceTags tags start (fromRange size)) . setProviderSearchFilter =<< view settings
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

getServiceTagList :: Member GalleyProvider r => UserId -> (Handler r) Public.ServiceTagList
getServiceTagList _ = do
  guardSecondFactorDisabled Nothing
  pure (Public.ServiceTagList allTags)
  where
    allTags = [(minBound :: Public.ServiceTag) ..]

updateServiceWhitelist :: Member GalleyProvider r => UserId -> ConnId -> TeamId -> Public.UpdateServiceWhitelist -> (Handler r) UpdateServiceWhitelistResp
updateServiceWhitelist uid con tid upd = do
  guardSecondFactorDisabled (Just uid)
  let pid = updateServiceWhitelistProvider upd
      sid = updateServiceWhitelistService upd
      newWhitelisted = updateServiceWhitelistStatus upd
  -- Preconditions
  ensurePermissions uid tid (Set.toList serviceWhitelistPermissions)
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

--------------------------------------------------------------------------------
-- Bot API

addBot :: Member GalleyProvider r => UserId -> ConnId -> ConvId -> Public.AddBot -> (Handler r) Public.AddBotResponse
addBot zuid zcon cid add = do
  guardSecondFactorDisabled (Just zuid)
  zusr <- lift (wrapClient $ User.lookupUser NoPendingInvitations zuid) >>= maybeInvalidUser
  let pid = addBotProvider add
  let sid = addBotService add
  -- Get the conversation and check preconditions
  lcid <- qualifyLocal cid
  cnv <- lift (liftSem $ GalleyProvider.getConv zuid lcid) >>= maybeConvNotFound
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
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
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
  btk <- Text.decodeLatin1 . toByteString' <$> ZAuth.newBotToken pid bid cid
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
  locale <- Opt.setDefaultUserLocale <$> view settings
  let name = fromMaybe (serviceProfileName svp) (Ext.rsNewBotName rs)
  let assets = fromMaybe (serviceProfileAssets svp) (Ext.rsNewBotAssets rs)
  let colour = fromMaybe defaultAccentId (Ext.rsNewBotColour rs)
  let pict = Pict [] -- Legacy
  let sref = newServiceRef sid pid
  let usr = User (botUserId bid) (Qualified (botUserId bid) domain) Nothing name pict assets colour False locale (Just sref) Nothing Nothing Nothing ManagedByWire defSupportedProtocols
  let newClt =
        (newClient PermanentClientType (Ext.rsNewBotLastPrekey rs))
          { newClientPrekeys = Ext.rsNewBotPrekeys rs
          }
  lift $ wrapClient $ User.insertAccount (UserAccount usr Active) (Just (cid, cnvTeam cnv)) Nothing True
  maxPermClients <- fromMaybe Opt.defUserMaxPermClients . Opt.setUserMaxPermClients <$> view settings
  (clt, _, _) <- do
    _ <- do
      -- if we want to protect bots against lh, 'addClient' cannot just send lh capability
      -- implicitly in the next line.
      pure $ FutureWork @'UnprotectedBot undefined
    wrapClientE (User.addClient (botUserId bid) bcl newClt maxPermClients (Just $ Set.singleton Public.ClientSupportsLegalholdImplicitConsent))
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

removeBot :: Member GalleyProvider r => UserId -> ConnId -> ConvId -> BotId -> (Handler r) (Maybe Public.RemoveBotResponse)
removeBot zusr zcon cid bid = do
  guardSecondFactorDisabled (Just zusr)
  -- Get the conversation and check preconditions
  lcid <- qualifyLocal cid
  cnv <- lift (liftSem $ GalleyProvider.getConv zusr lcid) >>= maybeConvNotFound
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

guardConvAdmin :: Conversation -> ExceptT Error (AppT r) ()
guardConvAdmin conv = do
  let selfMember = cmSelf . cnvMembers $ conv
  unless (memConvRoleName selfMember == roleNameWireAdmin) $ (throwStd (errorToWai @'E.AccessDenied))

botGetSelf :: BotId -> (Handler r) Public.UserProfile
botGetSelf bot = do
  p <- lift $ wrapClient $ User.lookupUser NoPendingInvitations (botUserId bot)
  maybe (throwStd (errorToWai @'E.UserNotFound)) (pure . (`Public.publicProfile` UserLegalHoldNoConsent)) p

botGetClient :: Member GalleyProvider r => BotId -> (Handler r) (Maybe Public.Client)
botGetClient bot = do
  guardSecondFactorDisabled (Just (botUserId bot))
  lift $ listToMaybe <$> wrapClient (User.lookupClients (botUserId bot))

botListPrekeys :: Member GalleyProvider r => BotId -> (Handler r) [Public.PrekeyId]
botListPrekeys bot = do
  guardSecondFactorDisabled (Just (botUserId bot))
  clt <- lift $ listToMaybe <$> wrapClient (User.lookupClients (botUserId bot))
  case clientId <$> clt of
    Nothing -> pure []
    Just ci -> lift (wrapClient $ User.lookupPrekeyIds (botUserId bot) ci)

botUpdatePrekeys :: Member GalleyProvider r => BotId -> Public.UpdateBotPrekeys -> (Handler r) ()
botUpdatePrekeys bot upd = do
  guardSecondFactorDisabled (Just (botUserId bot))
  clt <- lift $ listToMaybe <$> wrapClient (User.lookupClients (botUserId bot))
  case clt of
    Nothing -> throwStd (errorToWai @'E.ClientNotFound)
    Just c -> do
      let pks = updateBotPrekeyList upd
      wrapClientE (User.updatePrekeys (botUserId bot) (clientId c) pks) !>> clientDataError

botClaimUsersPrekeys ::
  (Member (Concurrency 'Unsafe) r, Member GalleyProvider r) =>
  BotId ->
  Public.UserClients ->
  Handler r Public.UserClientPrekeyMap
botClaimUsersPrekeys _ body = do
  guardSecondFactorDisabled Nothing
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  when (Map.size (Public.userClients body) > maxSize) $
    throwStd (errorToWai @'E.TooManyClients)
  Client.claimLocalMultiPrekeyBundles UnprotectedBot body !>> clientError

botListUserProfiles :: Member GalleyProvider r => BotId -> (CommaSeparatedList UserId) -> (Handler r) [Public.BotUserView]
botListUserProfiles _ uids = do
  guardSecondFactorDisabled Nothing -- should we check all user ids?
  us <- lift . wrapClient $ User.lookupUsers NoPendingInvitations (fromCommaSeparatedList uids)
  pure (map mkBotUserView us)

botGetUserClients :: Member GalleyProvider r => BotId -> UserId -> (Handler r) [Public.PubClient]
botGetUserClients _ uid = do
  guardSecondFactorDisabled (Just uid)
  lift $ pubClient <$$> wrapClient (User.lookupClients uid)
  where
    pubClient c = Public.PubClient (clientId c) (clientClass c)

botDeleteSelf :: Member GalleyProvider r => BotId -> ConvId -> (Handler r) ()
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
  Member GalleyProvider r =>
  Maybe UserId ->
  ExceptT Error (AppT r) ()
guardSecondFactorDisabled mbUserId = do
  enabled <- lift $ liftSem $ (==) Feature.FeatureStatusEnabled . Feature.wsStatus . Feature.afcSndFactorPasswordChallenge <$> GalleyProvider.getAllFeatureConfigsForUser mbUserId
  when enabled $ (throwStd (errorToWai @'E.AccessDenied))

minRsaKeySize :: Int
minRsaKeySize = 256 -- Bytes (= 2048 bits)

activate :: ProviderId -> Maybe Public.Email -> Public.Email -> (Handler r) ()
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

validateServiceKey :: MonadIO m => Public.ServiceKeyPEM -> m (Maybe (Public.ServiceKey, Fingerprint Rsa))
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

maybeInvalidProvider :: Monad m => Maybe a -> (ExceptT Error m) a
maybeInvalidProvider = maybe (throwStd (errorToWai @'E.ProviderNotFound)) pure

maybeInvalidCode :: Monad m => Maybe a -> (ExceptT Error m) a
maybeInvalidCode = maybe (throwStd (errorToWai @'E.InvalidCode)) pure

maybeServiceNotFound :: Monad m => Maybe a -> (ExceptT Error m) a
maybeServiceNotFound = maybe (throwStd (errorToWai @'E.ServiceNotFound)) pure

maybeConvNotFound :: Monad m => Maybe a -> (ExceptT Error m) a
maybeConvNotFound = maybe (throwStd (notFound "Conversation not found")) pure

maybeBadCredentials :: Monad m => Maybe a -> (ExceptT Error m) a
maybeBadCredentials = maybe (throwStd (errorToWai @'E.BadCredentials)) pure

maybeInvalidServiceKey :: Monad m => Maybe a -> (ExceptT Error m) a
maybeInvalidServiceKey = maybe (throwStd (errorToWai @'E.InvalidServiceKey)) pure

maybeInvalidUser :: Monad m => Maybe a -> (ExceptT Error m) a
maybeInvalidUser = maybe (throwStd (errorToWai @'E.InvalidUser)) pure

rangeChecked :: (KnownNat n, KnownNat m, Within a n m, Monad monad) => a -> (ExceptT Error monad) (Range n m a)
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

randServiceToken :: MonadIO m => m Public.ServiceToken
randServiceToken = ServiceToken . Ascii.encodeBase64Url <$> liftIO (randBytes 18)
