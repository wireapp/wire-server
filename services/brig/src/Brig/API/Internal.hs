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

module Brig.API.Internal
  ( sitemap,
  )
where

import qualified Brig.API.Client as API
import qualified Brig.API.Connection as API
import Brig.API.Error
import Brig.API.Handler
import qualified Brig.API.IdMapping as IdMapping
import Brig.API.Types
import qualified Brig.API.User as API
import Brig.App
import qualified Brig.Data.User as Data
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Provider.API as Provider
import qualified Brig.Team.API as Team
import Brig.Types
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import qualified Brig.User.API.Auth as Auth
import qualified Brig.User.API.Search as Search
import Control.Error hiding (bool)
import Control.Lens (view)
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Handle (Handle)
import Data.Id as Id
import qualified Data.List1 as List1
import qualified Data.Map.Strict as Map
import Data.Misc ((<$$>))
import qualified Data.Set as Set
import Galley.Types (UserClients (..))
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities as Utilities
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.ZAuth (zauthConnId, zauthUserId)
import Wire.API.User.RichInfo

---------------------------------------------------------------------------
-- Sitemap

sitemap :: Routes a Handler ()
sitemap = do
  get "/i/status" (continue $ const $ return empty) true
  head "/i/status" (continue $ const $ return empty) true

  -- This endpoint can lead to the following events being sent:
  -- - ConnectionUpdated event to the user and all users connecting with
  -- - ConvCreate event to the user for each connect conversation that did not exist before
  --   (via galley)
  -- - ConvConnect event to the user for each connection that was not already accepted by the
  --   other
  -- - MemberJoin event to the user and other for each connection that was not already
  --   accepted by the other
  post "/i/users/:uid/auto-connect" (continue autoConnectH) $
    accept "application" "json"
      .&. capture "uid"
      .&. opt zauthConnId
      .&. jsonRequest @UserSet

  -- This endpoint can lead to the following events being sent:
  -- - UserActivated event to created user, if it is a team invitation or user has an SSO ID
  -- - UserIdentityUpdated event to created user, if email or phone get activated
  post "/i/users" (continue createUserNoVerifyH) $
    accept "application" "json"
      .&. jsonRequest @NewUser

  -- internal email activation (used in tests and in spar for validating emails obtains as
  -- SAML user identifiers).  if the validate query parameter is false or missing, only set
  -- the activation timeout, but do not send an email, and do not do anything about activating
  -- the email.
  put "/i/self/email" (continue changeSelfEmailMaybeSendH) $
    zauthUserId
      .&. def False (query "validate")
      .&. jsonRequest @EmailUpdate

  -- This endpoint will lead to the following events being sent:
  -- - UserDeleted event to all of its contacts
  -- - MemberLeave event to members for all conversations the user was in (via galley)
  delete "/i/users/:uid" (continue deleteUserNoVerifyH) $
    capture "uid"
  get "/i/users/connections-status" (continue deprecatedGetConnectionsStatusH) $
    query "users"
      .&. opt (query "filter")
  post "/i/users/connections-status" (continue getConnectionsStatusH) $
    accept "application" "json"
      .&. jsonRequest @ConnectionsStatusRequest
      .&. opt (query "filter")

  -- NOTE: this is only *activated* accounts, ie. accounts with @isJust . userIdentity@!!
  -- FUTUREWORK: this should be much more obvious in the UI.  or behavior should just be
  -- different.
  get "/i/users" (continue listActivatedAccountsH) $
    accept "application" "json"
      .&. (param "ids" ||| param "handles")

  get "/i/users" (continue listAccountsByIdentityH) $
    accept "application" "json"
      .&. (param "email" ||| param "phone")

  put "/i/users/:uid/status" (continue changeAccountStatusH) $
    capture "uid"
      .&. jsonRequest @AccountStatusUpdate

  get "/i/users/:uid/status" (continue getAccountStatusH) $
    accept "application" "json"
      .&. capture "uid"

  get "/i/users/:uid/contacts" (continue getContactListH) $
    accept "application" "json"
      .&. capture "uid"

  get "/i/users/activation-code" (continue getActivationCodeH) $
    accept "application" "json"
      .&. (param "email" ||| param "phone")

  get "/i/users/password-reset-code" (continue getPasswordResetCodeH) $
    accept "application" "json"
      .&. (param "email" ||| param "phone")

  -- This endpoint can lead to the following events being sent:
  -- - UserIdentityRemoved event to target user
  post "/i/users/revoke-identity" (continue revokeIdentityH) $
    param "email" ||| param "phone"

  head "/i/users/blacklist" (continue checkBlacklistH) $
    param "email" ||| param "phone"

  delete "/i/users/blacklist" (continue deleteFromBlacklistH) $
    param "email" ||| param "phone"

  post "/i/users/blacklist" (continue addBlacklistH) $
    param "email" ||| param "phone"

  -- given a phone number (or phone number prefix), see whether
  -- it is blocked via a prefix (and if so, via which specific prefix)
  get "/i/users/phone-prefixes/:prefix" (continue getPhonePrefixesH) $
    capture "prefix"

  delete "/i/users/phone-prefixes/:prefix" (continue deleteFromPhonePrefixH) $
    capture "prefix"

  post "/i/users/phone-prefixes" (continue addPhonePrefixH) $
    accept "application" "json"
      .&. jsonRequest @ExcludedPrefix

  put "/i/users/:uid/sso-id" (continue updateSSOIdH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @UserSSOId

  put "/i/users/:uid/managed-by" (continue updateManagedByH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @ManagedByUpdate

  put "/i/users/:uid/rich-info" (continue updateRichInfoH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @RichInfoUpdate

  post "/i/clients" (continue internalListClientsH) $
    accept "application" "json"
      .&. jsonRequest @UserSet

  -- This endpoint can lead to the following events being sent:
  -- - ClientAdded event to the user
  -- - ClientRemoved event to the user, if removing old clients due to max number of clients
  -- - UserLegalHoldEnabled event to contacts of the user, if client type is legalhold
  post "/i/clients/:uid" (continue addClientInternalH) $
    capture "uid"
      .&. jsonRequest @NewClient
      .&. opt zauthConnId
      .&. accept "application" "json"

  -- This endpoint can lead to the following events being sent:
  -- - LegalHoldClientRequested event to contacts of the user
  post "/i/clients/legalhold/:uid/request" (continue legalHoldClientRequestedH) $
    capture "uid"
      .&. jsonRequest @LegalHoldClientRequest
      .&. accept "application" "json"

  -- This endpoint can lead to the following events being sent:
  -- - ClientRemoved event to the user
  -- - UserLegalHoldDisabled event to contacts of the user
  delete "/i/clients/legalhold/:uid" (continue removeLegalHoldClientH) $
    capture "uid"
      .&. accept "application" "json"

  Provider.routesInternal
  Auth.routesInternal
  Search.routesInternal
  Team.routesInternal
  IdMapping.routesInternal

---------------------------------------------------------------------------
-- Handlers

-- | Add a client without authentication checks
addClientInternalH :: UserId ::: JsonRequest NewClient ::: Maybe ConnId ::: JSON -> Handler Response
addClientInternalH (usr ::: req ::: connId ::: _) = do
  new <- parseJsonBody req
  setStatus status201 . json <$> addClientInternal usr new connId

addClientInternal :: UserId -> NewClient -> Maybe ConnId -> Handler Client
addClientInternal usr new connId = do
  API.addClient usr connId Nothing new !>> clientError

legalHoldClientRequestedH :: UserId ::: JsonRequest LegalHoldClientRequest ::: JSON -> Handler Response
legalHoldClientRequestedH (targetUser ::: req ::: _) = do
  clientRequest <- parseJsonBody req
  lift $ API.legalHoldClientRequested targetUser clientRequest
  return $ setStatus status200 empty

removeLegalHoldClientH :: UserId ::: JSON -> Handler Response
removeLegalHoldClientH (uid ::: _) = do
  lift $ API.removeLegalHoldClient uid
  return $ setStatus status200 empty

internalListClientsH :: JSON ::: JsonRequest UserSet -> Handler Response
internalListClientsH (_ ::: req) = do
  json <$> (lift . internalListClients =<< parseJsonBody req)

internalListClients :: UserSet -> AppIO UserClients
internalListClients (UserSet usrs) = do
  UserClients . Map.mapKeys makeIdOpaque . Map.fromList
    <$> (API.lookupUsersClientIds $ Set.toList usrs)

autoConnectH :: JSON ::: UserId ::: Maybe ConnId ::: JsonRequest UserSet -> Handler Response
autoConnectH (_ ::: uid ::: conn ::: req) = do
  json <$> (autoConnect uid conn =<< parseJsonBody req)

autoConnect :: UserId -> Maybe ConnId -> UserSet -> Handler [UserConnection]
autoConnect uid conn (UserSet to) = do
  let num = Set.size to
  when (num < 1) $
    throwStd $
      badRequest "No users given for auto-connect."
  when (num > 25) $
    throwStd $
      badRequest "Too many users given for auto-connect (> 25)."
  API.autoConnect uid to conn !>> connError

createUserNoVerifyH :: JSON ::: JsonRequest NewUser -> Handler Response
createUserNoVerifyH (_ ::: req) = do
  CreateUserNoVerifyResponse uid prof <- createUserNoVerify =<< parseJsonBody req
  return . setStatus status201
    . addHeader "Location" (toByteString' uid)
    $ json prof

data CreateUserNoVerifyResponse = CreateUserNoVerifyResponse UserId SelfProfile

createUserNoVerify :: NewUser -> Handler CreateUserNoVerifyResponse
createUserNoVerify uData = do
  result <- API.createUser uData !>> newUserError
  let acc = createdAccount result
  let usr = accountUser acc
  let uid = userId usr
  let eac = createdEmailActivation result
  let pac = createdPhoneActivation result
  for_ (catMaybes [eac, pac]) $ \adata ->
    let key = ActivateKey $ activationKey adata
        code = activationCode adata
     in API.activate key code (Just uid) !>> actError
  return $ CreateUserNoVerifyResponse uid (SelfProfile usr)

deleteUserNoVerifyH :: UserId -> Handler Response
deleteUserNoVerifyH uid = do
  setStatus status202 empty <$ deleteUserNoVerify uid

deleteUserNoVerify :: UserId -> Handler ()
deleteUserNoVerify uid = do
  void $ lift (API.lookupAccount uid) >>= ifNothing userNotFound
  lift $ API.deleteUserNoVerify uid

changeSelfEmailMaybeSendH :: UserId ::: Bool ::: JsonRequest EmailUpdate -> Handler Response
changeSelfEmailMaybeSendH (u ::: validate ::: req) = do
  email <- euEmail <$> parseJsonBody req
  changeSelfEmailMaybeSend u (if validate then ActuallySendEmail else DoNotSendEmail) email >>= \case
    ChangeEmailResponseIdempotent -> pure (setStatus status204 empty)
    ChangeEmailResponseNeedsActivation -> pure (setStatus status202 empty)

data MaybeSendEmail = ActuallySendEmail | DoNotSendEmail

changeSelfEmailMaybeSend :: UserId -> MaybeSendEmail -> Email -> Handler ChangeEmailResponse
changeSelfEmailMaybeSend u ActuallySendEmail email = do
  API.changeSelfEmail u email
changeSelfEmailMaybeSend u DoNotSendEmail email = do
  API.changeEmail u email !>> changeEmailError >>= \case
    ChangeEmailIdempotent -> pure ChangeEmailResponseIdempotent
    ChangeEmailNeedsActivation _ -> pure ChangeEmailResponseNeedsActivation

listActivatedAccountsH :: JSON ::: Either (List UserId) (List Handle) -> Handler Response
listActivatedAccountsH (_ ::: qry) = do
  json <$> lift (listActivatedAccounts qry)

listActivatedAccounts :: Either (List UserId) (List Handle) -> AppIO [UserAccount]
listActivatedAccounts = \case
  Left us -> byIds (fromList us)
  Right hs -> do
    us <- mapM (API.lookupHandle) (fromList hs)
    byIds (catMaybes us)
  where
    byIds uids =
      filter (isJust . userIdentity . accountUser)
        <$> API.lookupAccounts uids

listAccountsByIdentityH :: JSON ::: Either Email Phone -> Handler Response
listAccountsByIdentityH (_ ::: emailOrPhone) =
  lift $
    json
      <$> API.lookupAccountsByIdentity emailOrPhone

getActivationCodeH :: JSON ::: Either Email Phone -> Handler Response
getActivationCodeH (_ ::: emailOrPhone) = do
  json <$> getActivationCode emailOrPhone

getActivationCode :: Either Email Phone -> Handler GetActivationCodeResp
getActivationCode emailOrPhone = do
  apair <- lift $ API.lookupActivationCode emailOrPhone
  maybe (throwStd activationKeyNotFound) (return . GetActivationCodeResp) apair

data GetActivationCodeResp = GetActivationCodeResp (ActivationKey, ActivationCode)

instance ToJSON GetActivationCodeResp where
  toJSON (GetActivationCodeResp (k, c)) = object ["key" .= k, "code" .= c]

getPasswordResetCodeH :: JSON ::: Either Email Phone -> Handler Response
getPasswordResetCodeH (_ ::: emailOrPhone) = do
  maybe (throwStd invalidPwResetKey) (pure . json) =<< lift (getPasswordResetCode emailOrPhone)

getPasswordResetCode :: Either Email Phone -> AppIO (Maybe GetPasswordResetCodeResp)
getPasswordResetCode emailOrPhone = do
  GetPasswordResetCodeResp <$$> API.lookupPasswordResetCode emailOrPhone

data GetPasswordResetCodeResp = GetPasswordResetCodeResp (PasswordResetKey, PasswordResetCode)

instance ToJSON GetPasswordResetCodeResp where
  toJSON (GetPasswordResetCodeResp (k, c)) = object ["key" .= k, "code" .= c]

changeAccountStatusH :: UserId ::: JsonRequest AccountStatusUpdate -> Handler Response
changeAccountStatusH (usr ::: req) = do
  status <- suStatus <$> parseJsonBody req
  API.changeAccountStatus (List1.singleton usr) status !>> accountStatusError
  return empty

getAccountStatusH :: JSON ::: UserId -> Handler Response
getAccountStatusH (_ ::: usr) = do
  status <- lift $ API.lookupStatus usr
  return $ case status of
    Just s -> json $ AccountStatusResp s
    Nothing -> setStatus status404 empty

getConnectionsStatusH ::
  JSON ::: JsonRequest ConnectionsStatusRequest ::: Maybe Relation ->
  Handler Response
getConnectionsStatusH (_ ::: req ::: flt) = do
  body <- parseJsonBody req
  json <$> lift (getConnectionsStatus body flt)

getConnectionsStatus :: ConnectionsStatusRequest -> Maybe Relation -> AppIO [ConnectionStatus]
getConnectionsStatus ConnectionsStatusRequest {csrFrom, csrTo} flt = do
  r <- API.lookupConnectionStatus csrFrom csrTo
  return $ maybe r (filterByRelation r) flt
  where
    filterByRelation l rel = filter ((== rel) . csStatus) l

revokeIdentityH :: Either Email Phone -> Handler Response
revokeIdentityH emailOrPhone = do
  lift $ API.revokeIdentity emailOrPhone
  return $ setStatus status200 empty

checkBlacklistH :: Either Email Phone -> Handler Response
checkBlacklistH emailOrPhone = do
  bl <- lift $ API.isBlacklisted emailOrPhone
  return $ setStatus (bool status404 status200 bl) empty

deleteFromBlacklistH :: Either Email Phone -> Handler Response
deleteFromBlacklistH emailOrPhone = do
  void . lift $ API.blacklistDelete emailOrPhone
  return empty

addBlacklistH :: Either Email Phone -> Handler Response
addBlacklistH emailOrPhone = do
  void . lift $ API.blacklistInsert emailOrPhone
  return empty

-- | Get any matching prefixes. Also try for shorter prefix matches,
-- i.e. checking for +123456 also checks for +12345, +1234, ...
getPhonePrefixesH :: PhonePrefix -> Handler Response
getPhonePrefixesH prefix = do
  results <- lift $ API.phonePrefixGet prefix
  return $ case results of
    [] -> setStatus status404 empty
    _ -> json results

-- | Delete a phone prefix entry (must be an exact match)
deleteFromPhonePrefixH :: PhonePrefix -> Handler Response
deleteFromPhonePrefixH prefix = do
  void . lift $ API.phonePrefixDelete prefix
  return empty

addPhonePrefixH :: JSON ::: JsonRequest ExcludedPrefix -> Handler Response
addPhonePrefixH (_ ::: req) = do
  prefix :: ExcludedPrefix <- parseJsonBody req
  void . lift $ API.phonePrefixInsert prefix
  return empty

updateSSOIdH :: UserId ::: JSON ::: JsonRequest UserSSOId -> Handler Response
updateSSOIdH (uid ::: _ ::: req) = do
  ssoid :: UserSSOId <- parseJsonBody req
  success <- lift $ Data.updateSSOId uid ssoid
  if success
    then return empty
    else return . setStatus status404 $ plain "User does not exist or has no team."

updateManagedByH :: UserId ::: JSON ::: JsonRequest ManagedByUpdate -> Handler Response
updateManagedByH (uid ::: _ ::: req) = do
  ManagedByUpdate managedBy <- parseJsonBody req
  lift $ Data.updateManagedBy uid managedBy
  return empty

updateRichInfoH :: UserId ::: JSON ::: JsonRequest RichInfoUpdate -> Handler Response
updateRichInfoH (uid ::: _ ::: req) = do
  empty <$ (updateRichInfo uid =<< parseJsonBody req)

updateRichInfo :: UserId -> RichInfoUpdate -> Handler ()
updateRichInfo uid rup = do
  let RichInfoAssocList richInfo = normalizeRichInfoAssocList . riuRichInfo $ rup
  maxSize <- setRichInfoLimit <$> view settings
  when (richInfoSize (RichInfo (RichInfoAssocList richInfo)) > maxSize) $ throwStd tooLargeRichInfo
  -- FUTUREWORK: send an event
  -- Intra.onUserEvent uid (Just conn) (richInfoUpdate uid ri)
  lift $ Data.updateRichInfo uid (RichInfoAssocList richInfo)

getContactListH :: JSON ::: UserId -> Handler Response
getContactListH (_ ::: uid) = do
  contacts <- lift $ API.lookupContactList uid
  return $ json $ (UserIds contacts)

-- Deprecated

-- Deprecated and to be removed after new versions of brig and galley are
-- deployed. Reason for deprecation: it returns N^2 things (which is not
-- needed), it doesn't scale, and it accepts everything in URL parameters,
-- which doesn't work when the list of users is long.
deprecatedGetConnectionsStatusH :: List UserId ::: Maybe Relation -> Handler Response
deprecatedGetConnectionsStatusH (users ::: flt) = do
  r <- lift $ API.lookupConnectionStatus (fromList users) (fromList users)
  return . json $ maybe r (filterByRelation r) flt
  where
    filterByRelation l rel = filter ((== rel) . csStatus) l

-- Utilities

ifNothing :: Utilities.Error -> Maybe a -> Handler a
ifNothing e = maybe (throwStd e) return
