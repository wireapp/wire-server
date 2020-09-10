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

module Brig.Provider.API
  ( -- * Main stuff
    routesPublic,
    routesInternal,

    -- * Event handlers
    finishDeleteService,
  )
where

import qualified Brig.API.Client as Client
import Brig.API.Error
import Brig.API.Handler
import Brig.API.Types (PasswordResetError (..))
import Brig.App (AppIO, internalEvents, settings)
import qualified Brig.Code as Code
import qualified Brig.Data.Client as User
import qualified Brig.Data.User as User
import Brig.Email (mkEmailKey, validateEmail)
import qualified Brig.IO.Intra as RPC
import qualified Brig.InternalEvent.Types as Internal
import Brig.Options (Settings (..))
import qualified Brig.Options as Opt
import Brig.Password
import Brig.Provider.DB (ServiceConn (..))
import qualified Brig.Provider.DB as DB
import Brig.Provider.Email
import qualified Brig.Provider.RPC as RPC
import qualified Brig.Queue as Queue
import Brig.Team.Util
import Brig.Types.Client (Client (..), ClientType (..), newClient, newClientPrekeys)
import Brig.Types.Intra (AccountStatus (..), UserAccount (..))
import Brig.Types.Provider (AddBot (..), DeleteProvider (..), DeleteService (..), NewService (..), PasswordChange (..), Provider (..), ProviderLogin (..), Service (..), ServiceProfile (..), ServiceToken (..), UpdateBotPrekeys (..), UpdateProvider (..), UpdateService (..), UpdateServiceConn (..), UpdateServiceWhitelist (..))
import qualified Brig.Types.Provider.External as Ext
import Brig.Types.User (ManagedBy (..), Name (..), Pict (..), User (..), defaultAccentId)
import qualified Brig.ZAuth as ZAuth
import Control.Error (throwE)
import Control.Exception.Enclosed (handleAny)
import Control.Lens (view, (^.))
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as C
import Data.Hashable (hash)
import Data.Id
import qualified Data.List as List
import Data.List1 (maybeList1)
import qualified Data.Map.Strict as Map
import Data.Misc (Fingerprint (..), Rsa, (<$$>))
import Data.Predicate
import Data.Range
import qualified Data.Set as Set
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text.Encoding as Text
import Galley.Types (AccessRole (..), ConvMembers (..), ConvType (..), Conversation (..), OtherMember (..))
import Galley.Types.Bot (newServiceRef, serviceRefId, serviceRefProvider)
import Galley.Types.Conversations.Roles (roleNameWireAdmin)
import qualified Galley.Types.Teams as Teams
import Imports
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate (accept, contentType, def, opt, query)
import Network.Wai.Routing
import Network.Wai.Utilities.Error ((!>>))
import qualified Network.Wai.Utilities.Error as Wai
import Network.Wai.Utilities.Request (JsonRequest, jsonRequest)
import Network.Wai.Utilities.Response (addHeader, empty, json, setStatus)
import Network.Wai.Utilities.ZAuth
import qualified OpenSSL.EVP.Digest as SSL
import qualified OpenSSL.EVP.PKey as SSL
import qualified OpenSSL.PEM as SSL
import qualified OpenSSL.RSA as SSL
import OpenSSL.Random (randBytes)
import qualified Ssl.Util as SSL
import UnliftIO.Async (pooledMapConcurrentlyN_)
import qualified Web.Cookie as Cookie
import qualified Wire.API.Conversation.Bot as Public
import qualified Wire.API.Event.Conversation as Public (Event)
import qualified Wire.API.Provider as Public
import qualified Wire.API.Provider.Bot as Public (BotUserView)
import qualified Wire.API.Provider.Service as Public
import qualified Wire.API.Provider.Service.Tag as Public
import qualified Wire.API.User as Public (UserProfile, publicProfile)
import qualified Wire.API.User.Client as Public (Client, PubClient (..), UserClientMap, UserClients, userClients)
import qualified Wire.API.User.Client.Prekey as Public (Prekey, PrekeyId)
import qualified Wire.API.User.Identity as Public (Email)

routesPublic :: Routes Doc.ApiBuilder Handler ()
routesPublic = do
  -- Public API (Unauthenticated) --------------------------------------------

  post "/provider/register" (continue newAccountH) $
    accept "application" "json"
      .&> jsonRequest @Public.NewProvider

  get "/provider/activate" (continue activateAccountKeyH) $
    accept "application" "json"
      .&> query "key"
      .&. query "code"

  get "/provider/approve" (continue approveAccountKeyH) $
    accept "application" "json"
      .&> query "key"
      .&. query "code"

  post "/provider/login" (continue loginH) $
    jsonRequest @Public.ProviderLogin

  post "/provider/password-reset" (continue beginPasswordResetH) $
    accept "application" "json"
      .&> jsonRequest @Public.PasswordReset

  post "/provider/password-reset/complete" (continue completePasswordResetH) $
    accept "application" "json"
      .&> jsonRequest @Public.CompletePasswordReset

  -- Provider API ------------------------------------------------------------

  delete "/provider" (continue deleteAccountH) $
    zauth ZAuthProvider
      .&> zauthProviderId
      .&. jsonRequest @Public.DeleteProvider

  put "/provider" (continue updateAccountProfileH) $
    accept "application" "json"
      .&> zauth ZAuthProvider
      .&> zauthProviderId
      .&. jsonRequest @Public.UpdateProvider

  put "/provider/email" (continue updateAccountEmailH) $
    zauth ZAuthProvider
      .&> zauthProviderId
      .&. jsonRequest @Public.EmailUpdate

  put "/provider/password" (continue updateAccountPasswordH) $
    zauth ZAuthProvider
      .&> zauthProviderId
      .&. jsonRequest @Public.PasswordChange

  get "/provider" (continue getAccountH) $
    accept "application" "json"
      .&> zauth ZAuthProvider
      .&> zauthProviderId

  post "/provider/services" (continue addServiceH) $
    accept "application" "json"
      .&> zauth ZAuthProvider
      .&> zauthProviderId
      .&. jsonRequest @Public.NewService

  get "/provider/services" (continue listServicesH) $
    accept "application" "json"
      .&> zauth ZAuthProvider
      .&> zauthProviderId

  get "/provider/services/:sid" (continue getServiceH) $
    accept "application" "json"
      .&> zauth ZAuthProvider
      .&> zauthProviderId
      .&. capture "sid"

  put "/provider/services/:sid" (continue updateServiceH) $
    zauth ZAuthProvider
      .&> zauthProviderId
      .&. capture "sid"
      .&. jsonRequest @Public.UpdateService

  put "/provider/services/:sid/connection" (continue updateServiceConnH) $
    zauth ZAuthProvider
      .&> zauthProviderId
      .&. capture "sid"
      .&. jsonRequest @Public.UpdateServiceConn

  -- TODO
  --     post "/provider/services/:sid/token" (continue genServiceTokenH) $
  --         accept "application" "json"
  --         .&. zauthProvider

  delete "/provider/services/:sid" (continue deleteServiceH) $
    zauth ZAuthProvider
      .&> zauthProviderId
      .&. capture "sid"
      .&. jsonRequest @Public.DeleteService

  -- User API ----------------------------------------------------------------

  get "/providers/:pid" (continue getProviderProfileH) $
    accept "application" "json"
      .&> zauth ZAuthAccess
      .&> capture "pid"

  get "/providers/:pid/services" (continue listServiceProfilesH) $
    accept "application" "json"
      .&> zauth ZAuthAccess
      .&> capture "pid"

  get "/providers/:pid/services/:sid" (continue getServiceProfileH) $
    accept "application" "json"
      .&> zauth ZAuthAccess
      .&> capture "pid"
      .&. capture "sid"

  get "/services" (continue searchServiceProfilesH) $
    accept "application" "json"
      .&> zauth ZAuthAccess
      .&> opt (query "tags")
      .&. opt (query "start")
      .&. def (unsafeRange 20) (query "size")

  get "/services/tags" (continue getServiceTagListH) $
    accept "application" "json"
      .&> zauth ZAuthAccess

  get "/teams/:tid/services/whitelisted" (continue searchTeamServiceProfilesH) $
    accept "application" "json"
      .&> zauthUserId
      .&. capture "tid"
      .&. opt (query "prefix")
      .&. def True (query "filter_disabled")
      .&. def (unsafeRange 20) (query "size")

  post "/teams/:tid/services/whitelist" (continue updateServiceWhitelistH) $
    accept "application" "json"
      .&> zauth ZAuthAccess
      .&> zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. jsonRequest @Public.UpdateServiceWhitelist

  post "/conversations/:cnv/bots" (continue addBotH) $
    accept "application" "json"
      .&> zauth ZAuthAccess
      .&> zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.AddBot

  delete "/conversations/:cnv/bots/:bot" (continue removeBotH) $
    zauth ZAuthAccess
      .&> zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. capture "bot"

  -- Bot API -----------------------------------------------------------------

  get "/bot/self" (continue botGetSelfH) $
    accept "application" "json"
      .&> zauth ZAuthBot
      .&> zauthBotId

  delete "/bot/self" (continue botDeleteSelfH) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. zauthConvId

  get "/bot/client/prekeys" (continue botListPrekeysH) $
    accept "application" "json"
      .&> zauth ZAuthBot
      .&> zauthBotId

  post "/bot/client/prekeys" (continue botUpdatePrekeysH) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. jsonRequest @Public.UpdateBotPrekeys

  get "/bot/client" (continue botGetClientH) $
    contentType "application" "json"
      .&> zauth ZAuthBot
      .&> zauthBotId

  post "/bot/users/prekeys" (continue botClaimUsersPrekeysH) $
    accept "application" "json"
      .&> zauth ZAuthBot
      .&> jsonRequest @Public.UserClients

  get "/bot/users" (continue botListUserProfilesH) $
    accept "application" "json"
      .&> zauth ZAuthBot
      .&> query "ids"

  get "/bot/users/:uid/clients" (continue botGetUserClientsH) $
    accept "application" "json"
      .&> zauth ZAuthBot
      .&> capture "uid"

routesInternal :: Routes a Handler ()
routesInternal = do
  get "/i/provider/activation-code" (continue getActivationCodeH) $
    accept "application" "json"
      .&> param "email"

--------------------------------------------------------------------------------
-- Public API (Unauthenticated)

newAccountH :: JsonRequest Public.NewProvider -> Handler Response
newAccountH req = do
  setStatus status201 . json <$> (newAccount =<< parseJsonBody req)

newAccount :: Public.NewProvider -> Handler Public.NewProviderResponse
newAccount new = do
  email <- case validateEmail (Public.newProviderEmail new) of
    Right em -> return em
    Left _ -> throwStd invalidEmail
  let name = Public.newProviderName new
  let pass = Public.newProviderPassword new
  let descr = fromRange (Public.newProviderDescr new)
  let url = Public.newProviderUrl new
  let emailKey = mkEmailKey email
  DB.lookupKey emailKey >>= mapM_ (const $ throwStd emailExists)
  (safePass, newPass) <- case pass of
    Just newPass -> (,Nothing) <$> mkSafePassword newPass
    Nothing -> do
      newPass <- genPassword
      safePass <- mkSafePassword newPass
      return (safePass, Just newPass)
  pid <- DB.insertAccount name safePass url descr
  gen <- Code.mkGen (Code.ForEmail email)
  code <-
    Code.generate
      gen
      Code.IdentityVerification
      (Code.Retries 3)
      (Code.Timeout (3600 * 24)) -- 24h
      (Just (toUUID pid))
  Code.insert code
  let key = Code.codeKey code
  let val = Code.codeValue code
  lift $ sendActivationMail name email key val False
  return $ Public.NewProviderResponse pid newPass

activateAccountKeyH :: Code.Key ::: Code.Value -> Handler Response
activateAccountKeyH (key ::: val) = do
  maybe (setStatus status204 empty) json <$> activateAccountKey key val

activateAccountKey :: Code.Key -> Code.Value -> Handler (Maybe Public.ProviderActivationResponse)
activateAccountKey key val = do
  c <- Code.verify key Code.IdentityVerification val >>= maybeInvalidCode
  (pid, email) <- case (Code.codeAccount c, Code.codeForEmail c) of
    (Just p, Just e) -> return (Id p, e)
    _ -> throwStd invalidCode
  (name, memail, _url, _descr) <- DB.lookupAccountData pid >>= maybeInvalidCode
  case memail of
    Just email' | email == email' -> return Nothing
    Just email' -> do
      -- Ensure we remove any pending password reset
      gen <- Code.mkGen (Code.ForEmail email')
      lift $ Code.delete (Code.genKey gen) Code.PasswordReset
      -- Activate the new and remove the old key
      activate pid (Just email') email
      return . Just $ Public.ProviderActivationResponse email
    -- Immediate approval for everybody (for now).
    Nothing -> do
      activate pid Nothing email
      lift $ sendApprovalConfirmMail name email
      return . Just $ Public.ProviderActivationResponse email

getActivationCodeH :: Public.Email -> Handler Response
getActivationCodeH e = do
  json <$> getActivationCode e

getActivationCode :: Public.Email -> Handler FoundActivationCode
getActivationCode e = do
  email <- case validateEmail e of
    Right em -> return em
    Left _ -> throwStd invalidEmail
  gen <- Code.mkGen (Code.ForEmail email)
  code <- Code.lookup (Code.genKey gen) Code.IdentityVerification
  maybe (throwStd activationKeyNotFound) (return . FoundActivationCode) code

data FoundActivationCode = FoundActivationCode Code.Code

instance ToJSON FoundActivationCode where
  toJSON (FoundActivationCode vcode) =
    toJSON $
      Code.KeyValuePair (Code.codeKey vcode) (Code.codeValue vcode)

approveAccountKeyH :: Code.Key ::: Code.Value -> Handler Response
approveAccountKeyH (key ::: val) = do
  empty <$ approveAccountKey key val

approveAccountKey :: Code.Key -> Code.Value -> Handler ()
approveAccountKey key val = do
  c <- Code.verify key Code.AccountApproval val >>= maybeInvalidCode
  case (Code.codeAccount c, Code.codeForEmail c) of
    (Just pid, Just email) -> do
      (name, _, _, _) <- DB.lookupAccountData (Id pid) >>= maybeInvalidCode
      activate (Id pid) Nothing email
      lift $ sendApprovalConfirmMail name email
    _ -> throwStd invalidCode

loginH :: JsonRequest Public.ProviderLogin -> Handler Response
loginH req = do
  tok <- login =<< parseJsonBody req
  setProviderCookie tok empty

login :: Public.ProviderLogin -> Handler ZAuth.ProviderToken
login l = do
  pid <- DB.lookupKey (mkEmailKey (providerLoginEmail l)) >>= maybeBadCredentials
  pass <- DB.lookupPassword pid >>= maybeBadCredentials
  unless (verifyPassword (providerLoginPassword l) pass) $
    throwStd badCredentials
  ZAuth.newProviderToken pid

beginPasswordResetH :: JsonRequest Public.PasswordReset -> Handler Response
beginPasswordResetH req = do
  setStatus status201 empty <$ (beginPasswordReset =<< parseJsonBody req)

beginPasswordReset :: Public.PasswordReset -> Handler ()
beginPasswordReset (Public.PasswordReset target) = do
  pid <- DB.lookupKey (mkEmailKey target) >>= maybeBadCredentials
  gen <- Code.mkGen (Code.ForEmail target)
  pending <- lift $ Code.lookup (Code.genKey gen) Code.PasswordReset
  code <- case pending of
    Just p -> throwE $ pwResetError (PasswordResetInProgress . Just $ Code.codeTTL p)
    Nothing ->
      Code.generate
        gen
        Code.PasswordReset
        (Code.Retries 3)
        (Code.Timeout 3600) -- 1h
        (Just (toUUID pid))
  Code.insert code
  lift $ sendPasswordResetMail target (Code.codeKey code) (Code.codeValue code)

completePasswordResetH :: JsonRequest Public.CompletePasswordReset -> Handler Response
completePasswordResetH req = do
  empty <$ (completePasswordReset =<< parseJsonBody req)

completePasswordReset :: Public.CompletePasswordReset -> Handler ()
completePasswordReset (Public.CompletePasswordReset key val newpwd) = do
  code <- Code.verify key Code.PasswordReset val >>= maybeInvalidCode
  case Id <$> Code.codeAccount code of
    Nothing -> throwE $ pwResetError InvalidPasswordResetCode
    Just pid -> do
      oldpass <- DB.lookupPassword pid >>= maybeBadCredentials
      when (verifyPassword newpwd oldpass) $ do
        throwStd newPasswordMustDiffer
      DB.updateAccountPassword pid newpwd
      Code.delete key Code.PasswordReset

--------------------------------------------------------------------------------
-- Provider API

getAccountH :: ProviderId -> Handler Response
getAccountH pid = do
  getAccount pid <&> \case
    Just p -> json p
    Nothing -> setStatus status404 empty

getAccount :: ProviderId -> Handler (Maybe Public.Provider)
getAccount pid = do
  DB.lookupAccount pid

updateAccountProfileH :: ProviderId ::: JsonRequest Public.UpdateProvider -> Handler Response
updateAccountProfileH (pid ::: req) = do
  empty <$ (updateAccountProfile pid =<< parseJsonBody req)

updateAccountProfile :: ProviderId -> Public.UpdateProvider -> Handler ()
updateAccountProfile pid upd = do
  _ <- DB.lookupAccount pid >>= maybeInvalidProvider
  DB.updateAccountProfile
    pid
    (updateProviderName upd)
    (updateProviderUrl upd)
    (updateProviderDescr upd)

updateAccountEmailH :: ProviderId ::: JsonRequest Public.EmailUpdate -> Handler Response
updateAccountEmailH (pid ::: req) = do
  setStatus status202 empty <$ (updateAccountEmail pid =<< parseJsonBody req)

updateAccountEmail :: ProviderId -> Public.EmailUpdate -> Handler ()
updateAccountEmail pid (Public.EmailUpdate new) = do
  email <- case validateEmail new of
    Right em -> return em
    Left _ -> throwStd invalidEmail
  let emailKey = mkEmailKey email
  DB.lookupKey emailKey >>= mapM_ (const $ throwStd emailExists)
  gen <- Code.mkGen (Code.ForEmail email)
  code <-
    Code.generate
      gen
      Code.IdentityVerification
      (Code.Retries 3)
      (Code.Timeout (3600 * 24)) -- 24h
      (Just (toUUID pid))
  Code.insert code
  lift $ sendActivationMail (Name "name") email (Code.codeKey code) (Code.codeValue code) True

updateAccountPasswordH :: ProviderId ::: JsonRequest Public.PasswordChange -> Handler Response
updateAccountPasswordH (pid ::: req) = do
  empty <$ (updateAccountPassword pid =<< parseJsonBody req)

updateAccountPassword :: ProviderId -> Public.PasswordChange -> Handler ()
updateAccountPassword pid upd = do
  pass <- DB.lookupPassword pid >>= maybeBadCredentials
  unless (verifyPassword (cpOldPassword upd) pass) $
    throwStd badCredentials
  when (verifyPassword (cpNewPassword upd) pass) $
    throwStd newPasswordMustDiffer
  DB.updateAccountPassword pid (cpNewPassword upd)

addServiceH :: ProviderId ::: JsonRequest Public.NewService -> Handler Response
addServiceH (pid ::: req) = do
  setStatus status201 . json <$> (addService pid =<< parseJsonBody req)

addService :: ProviderId -> Public.NewService -> Handler Public.NewServiceResponse
addService pid new = do
  _ <- DB.lookupAccount pid >>= maybeInvalidProvider
  let name = newServiceName new
  let summary = fromRange (newServiceSummary new)
  let descr = fromRange (newServiceDescr new)
  let baseUrl = newServiceUrl new
  let pubkey = newServiceKey new
  let assets = newServiceAssets new
  let tags = fromRange (newServiceTags new)
  (pk, fp) <- validateServiceKey pubkey >>= maybeInvalidServiceKey
  token <- maybe randServiceToken return (newServiceToken new)
  sid <- DB.insertService pid name summary descr baseUrl token pk fp assets tags
  let rstoken = maybe (Just token) (const Nothing) (newServiceToken new)
  return $ Public.NewServiceResponse sid rstoken

listServicesH :: ProviderId -> Handler Response
listServicesH pid = json <$> listServices pid

listServices :: ProviderId -> Handler [Public.Service]
listServices = DB.listServices

getServiceH :: ProviderId ::: ServiceId -> Handler Response
getServiceH (pid ::: sid) = do
  json <$> getService pid sid

getService :: ProviderId -> ServiceId -> Handler Public.Service
getService pid sid = do
  DB.lookupService pid sid >>= maybeServiceNotFound

updateServiceH :: ProviderId ::: ServiceId ::: JsonRequest Public.UpdateService -> Handler Response
updateServiceH (pid ::: sid ::: req) = do
  empty <$ (updateService pid sid =<< parseJsonBody req)

updateService :: ProviderId -> ServiceId -> Public.UpdateService -> Handler ()
updateService pid sid upd = do
  _ <- DB.lookupAccount pid >>= maybeInvalidProvider
  -- Update service profile
  svc <- DB.lookupService pid sid >>= maybeServiceNotFound
  let name = serviceName svc
  let newName = updateServiceName upd
  let nameChange = liftM2 (,) (pure name) newName
  let tags = unsafeRange (serviceTags svc)
  let newTags = updateServiceTags upd
  let tagsChange = liftM2 (,) (pure tags) (rcast <$> newTags)
  let newSummary = fromRange <$> updateServiceSummary upd
  let newDescr = fromRange <$> updateServiceDescr upd
  let newAssets = updateServiceAssets upd
  -- Update service, tags/prefix index if the service is enabled
  DB.updateService pid sid name tags nameChange newSummary newDescr newAssets tagsChange (serviceEnabled svc)

updateServiceConnH :: ProviderId ::: ServiceId ::: JsonRequest Public.UpdateServiceConn -> Handler Response
updateServiceConnH (pid ::: sid ::: req) = do
  empty <$ (updateServiceConn pid sid =<< parseJsonBody req)

updateServiceConn :: ProviderId -> ServiceId -> Public.UpdateServiceConn -> Handler ()
updateServiceConn pid sid upd = do
  pass <- DB.lookupPassword pid >>= maybeBadCredentials
  unless (verifyPassword (updateServiceConnPassword upd) pass) $
    throwStd badCredentials
  scon <- DB.lookupServiceConn pid sid >>= maybeServiceNotFound
  svc <- DB.lookupServiceProfile pid sid >>= maybeServiceNotFound
  let newBaseUrl = updateServiceConnUrl upd
  let newTokens = maybeList1 . fromRange =<< updateServiceConnTokens upd
  let newEnabled = updateServiceConnEnabled upd
  let newKeyPems = fromRange <$> updateServiceConnKeys upd
  keys <- forM newKeyPems (mapM (validateServiceKey >=> maybeInvalidServiceKey))
  let newKeys = keys >>= maybeList1
  let newFps = fmap snd <$> newKeys
  DB.updateServiceConn pid sid newBaseUrl newTokens newKeys newEnabled
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
      if sconEnabled scon
        then DB.deleteServiceIndexes pid sid name tags
        else DB.insertServiceIndexes pid sid name tags

-- TODO: Send informational email to provider.

-- | The endpoint that is called to delete a service.
--
-- Since deleting a service can be costly, it just marks the service as
-- disabled and then creates an event that will, when processed, actually
-- delete the service. See 'finishDeleteService'.
deleteServiceH :: ProviderId ::: ServiceId ::: JsonRequest Public.DeleteService -> Handler Response
deleteServiceH (pid ::: sid ::: req) = do
  setStatus status202 empty <$ (deleteService pid sid =<< parseJsonBody req)

-- | The endpoint that is called to delete a service.
--
-- Since deleting a service can be costly, it just marks the service as
-- disabled and then creates an event that will, when processed, actually
-- delete the service. See 'finishDeleteService'.
deleteService :: ProviderId -> ServiceId -> Public.DeleteService -> Handler ()
deleteService pid sid del = do
  pass <- DB.lookupPassword pid >>= maybeBadCredentials
  unless (verifyPassword (deleteServicePassword del) pass) $
    throwStd badCredentials
  _ <- DB.lookupService pid sid >>= maybeServiceNotFound
  -- Disable the service
  DB.updateServiceConn pid sid Nothing Nothing Nothing (Just False)
  -- Create an event
  queue <- view internalEvents
  lift $ Queue.enqueue queue (Internal.DeleteService pid sid)

finishDeleteService :: ProviderId -> ServiceId -> AppIO ()
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

deleteAccountH :: ProviderId ::: JsonRequest Public.DeleteProvider -> Handler Response
deleteAccountH (pid ::: req) = do
  empty <$ (deleteAccount pid =<< parseJsonBody req)

deleteAccount :: ProviderId -> Public.DeleteProvider -> Handler ()
deleteAccount pid del = do
  prov <- DB.lookupAccount pid >>= maybeInvalidProvider
  pass <- DB.lookupPassword pid >>= maybeBadCredentials
  unless (verifyPassword (deleteProviderPassword del) pass) $
    throwStd badCredentials
  svcs <- DB.listServices pid
  forM_ svcs $ \svc -> do
    let sid = serviceId svc
    let tags = unsafeRange (serviceTags svc)
        name = serviceName svc
    lift $ RPC.removeServiceConn pid sid
    DB.deleteService pid sid name tags
  DB.deleteKey (mkEmailKey (providerEmail prov))
  DB.deleteAccount pid

--------------------------------------------------------------------------------
-- User API

getProviderProfileH :: ProviderId -> Handler Response
getProviderProfileH pid = do
  json <$> getProviderProfile pid

getProviderProfile :: ProviderId -> Handler Public.ProviderProfile
getProviderProfile pid = do
  DB.lookupAccountProfile pid >>= maybeProviderNotFound

listServiceProfilesH :: ProviderId -> Handler Response
listServiceProfilesH pid = do
  json <$> listServiceProfiles pid

listServiceProfiles :: ProviderId -> Handler [Public.ServiceProfile]
listServiceProfiles pid = do
  DB.listServiceProfiles pid

getServiceProfileH :: ProviderId ::: ServiceId -> Handler Response
getServiceProfileH (pid ::: sid) = do
  json <$> getServiceProfile pid sid

getServiceProfile :: ProviderId -> ServiceId -> Handler Public.ServiceProfile
getServiceProfile pid sid = do
  DB.lookupServiceProfile pid sid >>= maybeServiceNotFound

searchServiceProfilesH :: Maybe (Public.QueryAnyTags 1 3) ::: Maybe Text ::: Range 10 100 Int32 -> Handler Response
searchServiceProfilesH (qt ::: start ::: size) = do
  json <$> searchServiceProfiles qt start size

-- TODO: in order to actually make it possible for clients to implement
-- pagination here, we need both 'start' and 'prefix'.
--
-- Also see Note [buggy pagination].
searchServiceProfiles :: Maybe (Public.QueryAnyTags 1 3) -> Maybe Text -> Range 10 100 Int32 -> Handler Public.ServiceProfilePage
searchServiceProfiles Nothing (Just start) size = do
  prefix :: Range 1 128 Text <- rangeChecked start
  DB.paginateServiceNames (Just prefix) (fromRange size) =<< setProviderSearchFilter <$> view settings
searchServiceProfiles (Just tags) start size = do
  DB.paginateServiceTags tags start (fromRange size) =<< setProviderSearchFilter <$> view settings
searchServiceProfiles Nothing Nothing _ = do
  throwStd $ badRequest "At least `tags` or `start` must be provided."

searchTeamServiceProfilesH ::
  UserId ::: TeamId ::: Maybe (Range 1 128 Text) ::: Bool ::: Range 10 100 Int32 ->
  Handler Response
searchTeamServiceProfilesH (uid ::: tid ::: prefix ::: filterDisabled ::: size) = do
  json <$> searchTeamServiceProfiles uid tid prefix filterDisabled size

-- NB: unlike 'searchServiceProfiles', we don't filter by service provider here
searchTeamServiceProfiles ::
  UserId ->
  TeamId ->
  Maybe (Range 1 128 Text) ->
  Bool ->
  Range 10 100 Int32 ->
  Handler Public.ServiceProfilePage
searchTeamServiceProfiles uid tid prefix filterDisabled size = do
  -- Check that the user actually belong to the team they claim they
  -- belong to. (Note: the 'tid' team might not even exist but we'll throw
  -- 'insufficientTeamPermissions' anyway)
  teamId <- lift $ User.lookupUserTeam uid
  unless (Just tid == teamId) $
    throwStd insufficientTeamPermissions
  -- Get search results
  DB.paginateServiceWhitelist tid prefix filterDisabled (fromRange size)

getServiceTagListH :: () -> Handler Response
getServiceTagListH () = json <$> getServiceTagList ()

getServiceTagList :: () -> Monad m => m Public.ServiceTagList
getServiceTagList () = return (Public.ServiceTagList allTags)
  where
    allTags = [(minBound :: Public.ServiceTag) ..]

updateServiceWhitelistH :: UserId ::: ConnId ::: TeamId ::: JsonRequest Public.UpdateServiceWhitelist -> Handler Response
updateServiceWhitelistH (uid ::: con ::: tid ::: req) = do
  resp <- updateServiceWhitelist uid con tid =<< parseJsonBody req
  let status = case resp of
        UpdateServiceWhitelistRespChanged -> status200
        UpdateServiceWhitelistRespUnchanged -> status204
  pure $ setStatus status empty

data UpdateServiceWhitelistResp
  = UpdateServiceWhitelistRespChanged
  | UpdateServiceWhitelistRespUnchanged

updateServiceWhitelist :: UserId -> ConnId -> TeamId -> Public.UpdateServiceWhitelist -> Handler UpdateServiceWhitelistResp
updateServiceWhitelist uid con tid upd = do
  let pid = updateServiceWhitelistProvider upd
      sid = updateServiceWhitelistService upd
      newWhitelisted = updateServiceWhitelistStatus upd
  -- Preconditions
  ensurePermissions uid tid (Set.toList Teams.serviceWhitelistPermissions)
  _ <- DB.lookupService pid sid >>= maybeServiceNotFound
  -- Add to various tables
  whitelisted <- DB.getServiceWhitelistStatus tid pid sid
  case (whitelisted, newWhitelisted) of
    (False, False) -> return UpdateServiceWhitelistRespUnchanged
    (True, True) -> return UpdateServiceWhitelistRespUnchanged
    (False, True) -> do
      DB.insertServiceWhitelist tid pid sid
      return UpdateServiceWhitelistRespChanged
    (True, False) -> do
      -- When the service is de-whitelisted, remove its bots from team
      -- conversations
      lift $
        runConduit $
          User.lookupServiceUsersForTeam pid sid tid
            .| C.mapM_
              ( pooledMapConcurrentlyN_
                  16
                  ( \(bid, cid) ->
                      deleteBot uid (Just con) bid cid
                  )
              )
      DB.deleteServiceWhitelist (Just tid) pid sid
      return UpdateServiceWhitelistRespChanged

addBotH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.AddBot -> Handler Response
addBotH (zuid ::: zcon ::: cid ::: req) = do
  setStatus status201 . json <$> (addBot zuid zcon cid =<< parseJsonBody req)

addBot :: UserId -> ConnId -> ConvId -> Public.AddBot -> Handler Public.AddBotResponse
addBot zuid zcon cid add = do
  zusr <- lift (User.lookupUser zuid) >>= maybeInvalidUser
  let pid = addBotProvider add
  let sid = addBotService add
  -- Get the conversation and check preconditions
  cnv <- lift (RPC.getConv zuid cid) >>= maybeConvNotFound
  let mems = cnvMembers cnv
  unless (cnvType cnv == RegularConv) $
    throwStd invalidConv
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  unless (length (cmOthers mems) < maxSize - 1) $
    throwStd tooManyMembers
  -- For team conversations: bots are not allowed in managed and in
  -- team-only conversations
  when (cnvAccessRole cnv == TeamAccessRole) $
    throwStd invalidConv
  for_ (cnvTeam cnv) $ \tid -> do
    tc <- lift (RPC.getTeamConv zuid tid cid) >>= maybeConvNotFound
    when (view Teams.managedConversation tc) $
      throwStd invalidConv
  -- Lookup the relevant service data
  scon <- DB.lookupServiceConn pid sid >>= maybeServiceNotFound
  unless (sconEnabled scon) $
    throwStd serviceDisabled
  svp <- DB.lookupServiceProfile pid sid >>= maybeServiceNotFound
  for_ (cnvTeam cnv) $ \tid -> do
    whitelisted <- DB.getServiceWhitelistStatus tid pid sid
    unless whitelisted $
      throwStd serviceNotWhitelisted
  -- Prepare a user ID, client ID and token for the bot.
  bid <- BotId <$> randomId
  btk <- Text.decodeLatin1 . toByteString' <$> ZAuth.newBotToken pid bid cid
  let bcl = newClientId (fromIntegral (hash bid))
  -- Ask the external service to create a bot
  let origmem = OtherMember (makeIdOpaque zuid) Nothing roleNameWireAdmin
  let members = origmem : (cmOthers mems)
  let bcnv = Ext.botConvView (cnvId cnv) (cnvName cnv) members
  let busr = mkBotUserView zusr
  let bloc = fromMaybe (userLocale zusr) (addBotLocale add)
  let botReq = Ext.NewBotRequest bid bcl busr bcnv btk bloc
  rs <- RPC.createBot scon botReq !>> StdError . serviceError
  -- Insert the bot user and client
  locale <- setDefaultLocale <$> view settings
  let name = fromMaybe (serviceProfileName svp) (Ext.rsNewBotName rs)
  let assets = fromMaybe (serviceProfileAssets svp) (Ext.rsNewBotAssets rs)
  let colour = fromMaybe defaultAccentId (Ext.rsNewBotColour rs)
  let pict = Pict [] -- Legacy
  let sref = newServiceRef sid pid
  let usr = User (botUserId bid) Nothing name pict assets colour False locale (Just sref) Nothing Nothing Nothing ManagedByWire
  let newClt =
        (newClient PermanentClientType (Ext.rsNewBotLastPrekey rs))
          { newClientPrekeys = Ext.rsNewBotPrekeys rs
          }
  lift $ User.insertAccount (UserAccount usr Active) (Just (cid, cnvTeam cnv)) Nothing True
  maxPermClients <- fromMaybe Opt.defUserMaxPermClients <$> Opt.setUserMaxPermClients <$> view settings
  (clt, _, _) <-
    User.addClient (botUserId bid) bcl newClt maxPermClients Nothing
      !>> const (StdError badGateway) -- MalformedPrekeys

  -- Add the bot to the conversation
  ev <- lift $ RPC.addBotMember zuid zcon cid bid (clientId clt) pid sid
  return $
    Public.AddBotResponse
      { Public.rsAddBotId = bid,
        Public.rsAddBotClient = bcl,
        Public.rsAddBotName = name,
        Public.rsAddBotColour = colour,
        Public.rsAddBotAssets = assets,
        Public.rsAddBotEvent = ev
      }

removeBotH :: UserId ::: ConnId ::: ConvId ::: BotId -> Handler Response
removeBotH (zusr ::: zcon ::: cid ::: bid) = do
  maybe (setStatus status204 empty) json <$> removeBot zusr zcon cid bid

removeBot :: UserId -> ConnId -> ConvId -> BotId -> Handler (Maybe Public.RemoveBotResponse)
removeBot zusr zcon cid bid = do
  -- Get the conversation and check preconditions
  cnv <- lift (RPC.getConv zusr cid) >>= maybeConvNotFound
  let mems = cnvMembers cnv
  unless (cnvType cnv == RegularConv) $
    throwStd invalidConv
  -- Find the bot in the member list and delete it
  let busr = botUserId bid
  let bot = List.find ((== makeIdOpaque busr) . omId) (cmOthers mems)
  case bot >>= omService of
    Nothing -> return Nothing
    Just _ -> do
      lift $ Public.RemoveBotResponse <$$> deleteBot zusr (Just zcon) bid cid

--------------------------------------------------------------------------------
-- Bot API

botGetSelfH :: BotId -> Handler Response
botGetSelfH bot = json <$> botGetSelf bot

botGetSelf :: BotId -> Handler Public.UserProfile
botGetSelf bot = do
  p <- lift $ User.lookupUser (botUserId bot)
  maybe (throwStd userNotFound) (return . Public.publicProfile) p

botGetClientH :: BotId -> Handler Response
botGetClientH bot = do
  maybe (throwStd clientNotFound) (pure . json) =<< lift (botGetClient bot)

botGetClient :: BotId -> AppIO (Maybe Public.Client)
botGetClient bot = do
  listToMaybe <$> User.lookupClients (botUserId bot)

botListPrekeysH :: BotId -> Handler Response
botListPrekeysH bot = do
  json <$> botListPrekeys bot

botListPrekeys :: BotId -> Handler [Public.PrekeyId]
botListPrekeys bot = do
  clt <- lift $ listToMaybe <$> User.lookupClients (botUserId bot)
  case clientId <$> clt of
    Nothing -> return []
    Just ci -> lift (User.lookupPrekeyIds (botUserId bot) ci)

botUpdatePrekeysH :: BotId ::: JsonRequest Public.UpdateBotPrekeys -> Handler Response
botUpdatePrekeysH (bot ::: req) = do
  empty <$ (botUpdatePrekeys bot =<< parseJsonBody req)

botUpdatePrekeys :: BotId -> Public.UpdateBotPrekeys -> Handler ()
botUpdatePrekeys bot upd = do
  clt <- lift $ listToMaybe <$> User.lookupClients (botUserId bot)
  case clt of
    Nothing -> throwStd clientNotFound
    Just c -> do
      let pks = updateBotPrekeyList upd
      User.updatePrekeys (botUserId bot) (clientId c) pks !>> clientDataError

botClaimUsersPrekeysH :: JsonRequest Public.UserClients -> Handler Response
botClaimUsersPrekeysH req = do
  json <$> (botClaimUsersPrekeys =<< parseJsonBody req)

botClaimUsersPrekeys :: Public.UserClients -> Handler (Public.UserClientMap (Maybe Public.Prekey))
botClaimUsersPrekeys body = do
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  when (Map.size (Public.userClients body) > maxSize) $
    throwStd tooManyClients
  Client.claimMultiPrekeyBundles body

botListUserProfilesH :: List UserId -> Handler Response
botListUserProfilesH uids = do
  json <$> botListUserProfiles uids

botListUserProfiles :: List UserId -> Handler [Public.BotUserView]
botListUserProfiles uids = do
  us <- lift $ User.lookupUsers (fromList uids)
  return (map mkBotUserView us)

botGetUserClientsH :: UserId -> Handler Response
botGetUserClientsH uid = do
  json <$> lift (botGetUserClients uid)

botGetUserClients :: UserId -> AppIO [Public.PubClient]
botGetUserClients uid = do
  pubClient <$$> User.lookupClients uid
  where
    pubClient c = Public.PubClient (clientId c) (clientClass c)

botDeleteSelfH :: BotId ::: ConvId -> Handler Response
botDeleteSelfH (bid ::: cid) = do
  empty <$ botDeleteSelf bid cid

botDeleteSelf :: BotId -> ConvId -> Handler ()
botDeleteSelf bid cid = do
  bot <- lift $ User.lookupUser (botUserId bid)
  _ <- maybeInvalidBot (userService =<< bot)
  _ <- lift $ deleteBot (botUserId bid) Nothing bid cid
  return ()

--------------------------------------------------------------------------------
-- Utilities

minRsaKeySize :: Int
minRsaKeySize = 256 -- Bytes (= 2048 bits)

activate :: ProviderId -> Maybe Public.Email -> Public.Email -> Handler ()
activate pid old new = do
  let emailKey = mkEmailKey new
  taken <- maybe False (/= pid) <$> DB.lookupKey emailKey
  when taken $
    throwStd emailExists
  DB.insertKey pid (mkEmailKey <$> old) emailKey

deleteBot :: UserId -> Maybe ConnId -> BotId -> ConvId -> AppIO (Maybe Public.Event)
deleteBot zusr zcon bid cid = do
  -- Remove the bot from the conversation
  ev <- RPC.removeBotMember zusr zcon cid bid
  -- Delete the bot user and client
  let buid = botUserId bid
  mbUser <- User.lookupUser buid
  User.lookupClients buid >>= mapM_ (User.rmClient buid . clientId)
  for_ (userService =<< mbUser) $ \sref -> do
    let pid = sref ^. serviceRefProvider
        sid = sref ^. serviceRefId
    User.deleteServiceUser pid sid bid
  -- TODO: Consider if we can actually delete the bot user entirely,
  -- i.e. not just marking the account as deleted.
  User.updateStatus buid Deleted
  return ev

validateServiceKey :: MonadIO m => Public.ServiceKeyPEM -> m (Maybe (Public.ServiceKey, Fingerprint Rsa))
validateServiceKey pem =
  liftIO $
    readPublicKey >>= \pk ->
      case join (SSL.toPublicKey <$> pk) of
        Nothing -> return Nothing
        Just pk' -> do
          Just sha <- SSL.getDigestByName "SHA256"
          let size = SSL.rsaSize (pk' :: SSL.RSAPubKey)
          if size < minRsaKeySize
            then return Nothing
            else do
              fpr <- Fingerprint <$> SSL.rsaFingerprint sha pk'
              let bits = fromIntegral size * 8
              let key = Public.ServiceKey Public.RsaServiceKey bits pem
              return $ Just (key, fpr)
  where
    readPublicKey =
      handleAny
        (const $ return Nothing)
        (SSL.readPublicKey (LC8.unpack (toByteString pem)) >>= return . Just)

mkBotUserView :: User -> Public.BotUserView
mkBotUserView u =
  Ext.BotUserView
    { Ext.botUserViewId = userId u,
      Ext.botUserViewName = userDisplayName u,
      Ext.botUserViewColour = userAccentId u,
      Ext.botUserViewHandle = userHandle u,
      Ext.botUserViewTeam = userTeam u
    }

setProviderCookie :: ZAuth.ProviderToken -> Response -> Handler Response
setProviderCookie t r = do
  s <- view settings
  let hdr = toByteString' (Cookie.renderSetCookie (cookie s))
  return (addHeader "Set-Cookie" hdr r)
  where
    cookie s =
      Cookie.def
        { Cookie.setCookieName = "zprovider",
          Cookie.setCookieValue = toByteString' t,
          Cookie.setCookieDomain = Just $ Text.encodeUtf8 . setCookieDomain $ s,
          Cookie.setCookiePath = Just "/provider",
          Cookie.setCookieExpires = Just (ZAuth.tokenExpiresUTC t),
          Cookie.setCookieSecure = not (setCookieInsecure s),
          Cookie.setCookieHttpOnly = True
        }

maybeInvalidProvider :: Maybe a -> Handler a
maybeInvalidProvider = maybe (throwStd invalidProvider) return

maybeInvalidCode :: Maybe a -> Handler a
maybeInvalidCode = maybe (throwStd invalidCode) return

maybeServiceNotFound :: Maybe a -> Handler a
maybeServiceNotFound = maybe (throwStd (notFound "Service not found")) return

maybeProviderNotFound :: Maybe a -> Handler a
maybeProviderNotFound = maybe (throwStd (notFound "Provider not found")) return

maybeConvNotFound :: Maybe a -> Handler a
maybeConvNotFound = maybe (throwStd (notFound "Conversation not found")) return

maybeBadCredentials :: Maybe a -> Handler a
maybeBadCredentials = maybe (throwStd badCredentials) return

maybeInvalidServiceKey :: Maybe a -> Handler a
maybeInvalidServiceKey = maybe (throwStd invalidServiceKey) return

maybeInvalidBot :: Maybe a -> Handler a
maybeInvalidBot = maybe (throwStd invalidBot) return

maybeInvalidUser :: Maybe a -> Handler a
maybeInvalidUser = maybe (throwStd invalidUser) return

rangeChecked :: Within a n m => a -> Handler (Range n m a)
rangeChecked = either (throwStd . invalidRange . fromString) return . checkedEither

invalidServiceKey :: Wai.Error
invalidServiceKey = Wai.Error status400 "invalid-service-key" "Invalid service key."

invalidProvider :: Wai.Error
invalidProvider = Wai.Error status403 "invalid-provider" "The provider does not exist."

invalidBot :: Wai.Error
invalidBot = Wai.Error status403 "invalid-bot" "The targeted user is not a bot."

invalidConv :: Wai.Error
invalidConv = Wai.Error status403 "invalid-conversation" "The operation is not allowed in this conversation."

badGateway :: Wai.Error
badGateway = Wai.Error status502 "bad-gateway" "The upstream service returned an invalid response."

tooManyMembers :: Wai.Error
tooManyMembers = Wai.Error status403 "too-many-members" "Maximum number of members per conversation reached."

tooManyBots :: Wai.Error
tooManyBots = Wai.Error status409 "too-many-bots" "Maximum number of bots for the service reached."

serviceDisabled :: Wai.Error
serviceDisabled = Wai.Error status403 "service-disabled" "The desired service is currently disabled."

serviceNotWhitelisted :: Wai.Error
serviceNotWhitelisted = Wai.Error status403 "service-not-whitelisted" "The desired service is not on the whitelist of allowed services for this team."

serviceError :: RPC.ServiceError -> Wai.Error
serviceError RPC.ServiceUnavailable = badGateway
serviceError RPC.ServiceBotConflict = tooManyBots

randServiceToken :: MonadIO m => m Public.ServiceToken
randServiceToken = ServiceToken . Ascii.encodeBase64Url <$> liftIO (randBytes 18)
