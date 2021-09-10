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
    servantSitemap,
    swaggerDocsAPI,
    ServantAPI,
    SwaggerDocsAPI,
  )
where

import qualified Brig.API.Client as API
import qualified Brig.API.Connection as API
import Brig.API.Error
import Brig.API.Handler
import Brig.API.Types
import qualified Brig.API.User as API
import Brig.API.Util (validateHandle)
import Brig.App
import qualified Brig.Data.Client as Data
import qualified Brig.Data.User as Data
import qualified Brig.IO.Intra as Intra
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Provider.API as Provider
import qualified Brig.Team.API as Team
import Brig.Team.DB (lookupInvitationByEmail)
import Brig.Types
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import qualified Brig.Types.User.EJPD as EJPD
import Brig.Types.User.Event (UserEvent (UserUpdated), UserUpdatedData (eupSSOId, eupSSOIdRemoved), emptyUserUpdatedData)
import qualified Brig.User.API.Auth as Auth
import qualified Brig.User.API.Search as Search
import qualified Brig.User.EJPD
import Control.Error hiding (bool)
import Control.Lens (view, (.~))
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.ByteString.Conversion as List
import Data.Handle (Handle)
import Data.Id as Id
import qualified Data.List1 as List1
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Swagger (HasInfo (info), HasTitle (title), Swagger)
import Galley.Types (UserClients (..))
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities as Utilities
import Network.Wai.Utilities.ZAuth (zauthConnId, zauthUserId)
import Servant hiding (Handler, JSON, addHeader, respond)
import qualified Servant
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import qualified System.Logger.Class as Log
import Wire.API.ErrorDescription
import Wire.API.User
import Wire.API.User.Client (UserClientsFull (..))
import Wire.API.User.RichInfo

---------------------------------------------------------------------------
-- Sitemap (servant)

type EJPDRequest =
  Summary
    "Identify users for law enforcement.  Wire has legal requirements to cooperate \
    \with the authorities.  The wire backend operations team uses this to answer \
    \identification requests manually.  It is our best-effort representation of the \
    \minimum required information we need to hand over about targets and (in some \
    \cases) their communication peers.  For more information, consult ejpd.admin.ch."
    :> "ejpd-request"
    :> QueryParam'
         [ Optional,
           Strict,
           Description "Also provide information about all contacts of the identified users"
         ]
         "include_contacts"
         Bool
    :> Servant.ReqBody '[Servant.JSON] EJPD.EJPDRequestBody
    :> Post '[Servant.JSON] EJPD.EJPDResponseBody

type ServantAPI =
  "i"
    :> ( EJPDRequest
       )

servantSitemap :: ServerT ServantAPI Handler
servantSitemap = Brig.User.EJPD.ejpdRequest

type SwaggerDocsAPI = "api" :> "internal" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDocsAPI :: Servant.Server SwaggerDocsAPI
swaggerDocsAPI = swaggerSchemaUIServer swaggerDoc

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy @ServantAPI)
    & info . title .~ "Wire-Server API as Swagger 2.0 (internal end-points; incomplete) "

---------------------------------------------------------------------------
-- Sitemap (wai-route)

sitemap :: Routes a Handler ()
sitemap = do
  get "/i/status" (continue $ const $ return empty) true
  head "/i/status" (continue $ const $ return empty) true

  -- This endpoint can lead to the following events being sent:
  -- - UserActivated event to created user, if it is a team invitation or user has an SSO ID
  -- - UserIdentityUpdated event to created user, if email or phone get activated
  post "/i/users" (continue createUserNoVerifyH) $
    accept "application" "json"
      .&. jsonRequest @NewUser

  -- internal email activation (used in tests and in spar for validating emails obtained as
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

  put "/i/connections/connection-update" (continue updateConnectionInternalH) $
    accept "application" "json"
      .&. jsonRequest @UpdateConnectionsInternal

  -- NOTE: this is only *activated* accounts, ie. accounts with @isJust . userIdentity@!!
  -- FUTUREWORK: this should be much more obvious in the UI.  or behavior should just be
  -- different.
  get "/i/users" (continue listActivatedAccountsH) $
    accept "application" "json"
      .&. (param "ids" ||| param "handles")
      .&. def False (query "includePendingInvitations")

  get "/i/users" (continue listAccountsByIdentityH) $
    accept "application" "json"
      .&. (param "email" ||| param "phone")
      .&. def False (query "includePendingInvitations")

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

  delete "/i/users/:uid/sso-id" (continue deleteSSOIdH) $
    capture "uid"
      .&. accept "application" "json"

  put "/i/users/:uid/managed-by" (continue updateManagedByH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @ManagedByUpdate

  put "/i/users/:uid/rich-info" (continue updateRichInfoH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @RichInfoUpdate

  put "/i/users/:uid/handle" (continue updateHandleH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @HandleUpdate

  put "/i/users/:uid/name" (continue updateUserNameH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @NameUpdate

  get "/i/users/:uid/rich-info" (continue getRichInfoH) $
    capture "uid"

  get "/i/users/rich-info" (continue getRichInfoMultiH) $
    param "ids"

  head "/i/users/handles/:handle" (continue checkHandleInternalH) $
    capture "handle"

  post "/i/clients" (continue internalListClientsH) $
    accept "application" "json"
      .&. jsonRequest @UserSet

  post "/i/clients/full" (continue internalListFullClientsH) $
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
  UserClients . Map.fromList
    <$> API.lookupUsersClientIds (Set.toList usrs)

internalListFullClientsH :: JSON ::: JsonRequest UserSet -> Handler Response
internalListFullClientsH (_ ::: req) =
  json <$> (lift . internalListFullClients =<< parseJsonBody req)

internalListFullClients :: UserSet -> AppIO UserClientsFull
internalListFullClients (UserSet usrs) =
  UserClientsFull <$> Data.lookupClientsBulk (Set.toList usrs)

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
  void $
    lift (API.lookupAccount uid)
      >>= ifNothing (errorDescriptionToWai userNotFound)
  lift $ API.deleteUserNoVerify uid

changeSelfEmailMaybeSendH :: UserId ::: Bool ::: JsonRequest EmailUpdate -> Handler Response
changeSelfEmailMaybeSendH (u ::: validate ::: req) = do
  email <- euEmail <$> parseJsonBody req
  changeSelfEmailMaybeSend u (if validate then ActuallySendEmail else DoNotSendEmail) email API.AllowSCIMUpdates >>= \case
    ChangeEmailResponseIdempotent -> pure (setStatus status204 empty)
    ChangeEmailResponseNeedsActivation -> pure (setStatus status202 empty)

data MaybeSendEmail = ActuallySendEmail | DoNotSendEmail

changeSelfEmailMaybeSend :: UserId -> MaybeSendEmail -> Email -> API.AllowSCIMUpdates -> Handler ChangeEmailResponse
changeSelfEmailMaybeSend u ActuallySendEmail email allowScim = do
  API.changeSelfEmail u email allowScim
changeSelfEmailMaybeSend u DoNotSendEmail email allowScim = do
  API.changeEmail u email allowScim !>> changeEmailError >>= \case
    ChangeEmailIdempotent -> pure ChangeEmailResponseIdempotent
    ChangeEmailNeedsActivation _ -> pure ChangeEmailResponseNeedsActivation

listActivatedAccountsH :: JSON ::: Either (List UserId) (List Handle) ::: Bool -> Handler Response
listActivatedAccountsH (_ ::: qry ::: includePendingInvitations) = do
  json <$> lift (listActivatedAccounts qry includePendingInvitations)

listActivatedAccounts :: Either (List UserId) (List Handle) -> Bool -> AppIO [UserAccount]
listActivatedAccounts elh includePendingInvitations = do
  Log.debug (Log.msg $ "listActivatedAccounts: " <> show (elh, includePendingInvitations))
  case elh of
    Left us -> byIds (fromList us)
    Right hs -> do
      us <- mapM (API.lookupHandle) (fromList hs)
      byIds (catMaybes us)
  where
    byIds :: [UserId] -> AppIO [UserAccount]
    byIds uids = API.lookupAccounts uids >>= filterM accountValid

    accountValid :: UserAccount -> AppIO Bool
    accountValid account = case userIdentity . accountUser $ account of
      Nothing -> pure False
      Just ident ->
        case (accountStatus account, includePendingInvitations, emailIdentity ident) of
          (PendingInvitation, False, _) -> pure False
          (PendingInvitation, True, Just email) -> do
            hasInvitation <- isJust <$> lookupInvitationByEmail email
            unless hasInvitation $ do
              -- user invited via scim should expire together with its invitation
              API.deleteUserNoVerify (userId . accountUser $ account)
            pure hasInvitation
          (PendingInvitation, True, Nothing) ->
            pure True -- cannot happen, user invited via scim always has an email
          (Active, _, _) -> pure True
          (Suspended, _, _) -> pure True
          (Deleted, _, _) -> pure True
          (Ephemeral, _, _) -> pure True

listAccountsByIdentityH :: JSON ::: Either Email Phone ::: Bool -> Handler Response
listAccountsByIdentityH (_ ::: emailOrPhone ::: includePendingInvitations) =
  lift $
    json
      <$> API.lookupAccountsByIdentity emailOrPhone includePendingInvitations

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
  r <- maybe (API.lookupConnectionStatus' csrFrom) (API.lookupConnectionStatus csrFrom) csrTo
  return $ maybe r (filterByRelation r) flt
  where
    filterByRelation l rel = filter ((== rel) . csStatus) l

revokeIdentityH :: Either Email Phone -> Handler Response
revokeIdentityH emailOrPhone = do
  lift $ API.revokeIdentity emailOrPhone
  return $ setStatus status200 empty

updateConnectionInternalH :: JSON ::: JsonRequest UpdateConnectionsInternal -> Handler Response
updateConnectionInternalH (_ ::: req) = do
  updateConn <- parseJsonBody req
  API.updateConnectionInternal updateConn !>> connError
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
  success <- lift $ Data.updateSSOId uid (Just ssoid)
  if success
    then do
      lift $ Intra.onUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOId = Just ssoid}))
      return empty
    else return . setStatus status404 $ plain "User does not exist or has no team."

deleteSSOIdH :: UserId ::: JSON -> Handler Response
deleteSSOIdH (uid ::: _) = do
  success <- lift $ Data.updateSSOId uid Nothing
  if success
    then do
      lift $ Intra.onUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOIdRemoved = True}))
      return empty
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

getRichInfoH :: UserId -> Handler Response
getRichInfoH uid = json <$> getRichInfo uid

getRichInfo :: UserId -> Handler RichInfo
getRichInfo uid = RichInfo . fromMaybe emptyRichInfoAssocList <$> lift (API.lookupRichInfo uid)

getRichInfoMultiH :: List UserId -> Handler Response
getRichInfoMultiH uids = json <$> getRichInfoMulti (List.fromList uids)

getRichInfoMulti :: [UserId] -> Handler [(UserId, RichInfo)]
getRichInfoMulti uids =
  lift (API.lookupRichInfoMultiUsers uids)

updateHandleH :: UserId ::: JSON ::: JsonRequest HandleUpdate -> Handler Response
updateHandleH (uid ::: _ ::: body) = empty <$ (updateHandle uid =<< parseJsonBody body)

updateHandle :: UserId -> HandleUpdate -> Handler ()
updateHandle uid (HandleUpdate handleUpd) = do
  handle <- validateHandle handleUpd
  API.changeHandle uid Nothing handle API.AllowSCIMUpdates !>> changeHandleError

updateUserNameH :: UserId ::: JSON ::: JsonRequest NameUpdate -> Handler Response
updateUserNameH (uid ::: _ ::: body) = empty <$ (updateUserName uid =<< parseJsonBody body)

updateUserName :: UserId -> NameUpdate -> Handler ()
updateUserName uid (NameUpdate nameUpd) = do
  name <- either (const $ throwStd (errorDescriptionToWai invalidUser)) pure $ mkName nameUpd
  let uu =
        UserUpdate
          { uupName = Just name,
            uupPict = Nothing,
            uupAssets = Nothing,
            uupAccentId = Nothing
          }
  lift (Data.lookupUser WithPendingInvitations uid) >>= \case
    Just _ -> API.updateUser uid Nothing uu API.AllowSCIMUpdates !>> updateProfileError
    Nothing -> throwStd (errorDescriptionToWai invalidUser)

checkHandleInternalH :: Text -> Handler Response
checkHandleInternalH =
  API.checkHandle >=> \case
    API.CheckHandleInvalid -> throwE (StdError invalidHandle)
    API.CheckHandleFound -> pure $ setStatus status200 empty
    API.CheckHandleNotFound -> pure $ setStatus status404 empty

getContactListH :: JSON ::: UserId -> Handler Response
getContactListH (_ ::: uid) = do
  contacts <- lift $ API.lookupContactList uid
  return $ json $ UserIds contacts

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
