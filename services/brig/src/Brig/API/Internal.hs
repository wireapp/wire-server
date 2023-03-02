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
module Brig.API.Internal
  ( sitemap,
    servantSitemap,
    BrigIRoutes.API,
    getMLSClients,
  )
where

import Brig.API.Auth
import qualified Brig.API.Client as API
import qualified Brig.API.Connection as API
import Brig.API.Error
import Brig.API.Handler
import Brig.API.MLS.KeyPackages.Validation
import Brig.API.OAuth (internalOauthAPI)
import Brig.API.Types
import qualified Brig.API.User as API
import qualified Brig.API.User as Api
import Brig.API.Util
import Brig.App
import qualified Brig.Code as Code
import Brig.Data.Activation
import qualified Brig.Data.Client as Data
import qualified Brig.Data.Connection as Data
import qualified Brig.Data.MLS.KeyPackage as Data
import qualified Brig.Data.User as Data
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.CodeStore (CodeStore)
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import qualified Brig.IO.Intra as Intra
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Provider.API as Provider
import qualified Brig.Team.API as Team
import Brig.Team.DB (lookupInvitationByEmail)
import Brig.Team.Types (ShowOrHideInvitationUrl (..))
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Brig.Types.User
import Brig.Types.User.Event (UserEvent (UserUpdated), UserUpdatedData (eupSSOId, eupSSOIdRemoved), emptyUserUpdatedData)
import qualified Brig.User.API.Search as Search
import qualified Brig.User.EJPD
import qualified Brig.User.Search.Index as Index
import Control.Error hiding (bool)
import Control.Lens (view)
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.ByteString.Conversion as List
import Data.Handle
import Data.Id as Id
import qualified Data.Map.Strict as Map
import Data.Qualified
import qualified Data.Set as Set
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Routing hiding (toList)
import Network.Wai.Utilities as Utilities
import Network.Wai.Utilities.ZAuth (zauthConnId, zauthUserId)
import Polysemy
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.Swagger.Internal.Orphans ()
import qualified System.Logger.Class as Log
import UnliftIO.Async
import Wire.API.Connection
import Wire.API.Error
import qualified Wire.API.Error.Brig as E
import Wire.API.Federation.API
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.Routes.Internal.Brig
import qualified Wire.API.Routes.Internal.Brig as BrigIRoutes
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Named
import qualified Wire.API.Team.Feature as ApiFt
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client
import Wire.API.User.Password
import Wire.API.User.RichInfo

---------------------------------------------------------------------------
-- Sitemap (servant)

servantSitemap ::
  forall r p.
  ( Member BlacklistStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r
  ) =>
  ServerT BrigIRoutes.API (Handler r)
servantSitemap =
  ejpdAPI
    :<|> accountAPI
    :<|> mlsAPI
    :<|> getVerificationCode
    :<|> teamsAPI
    :<|> userAPI
    :<|> authAPI
    :<|> internalOauthAPI

ejpdAPI ::
  Member GalleyProvider r =>
  ServerT BrigIRoutes.EJPD_API (Handler r)
ejpdAPI =
  Brig.User.EJPD.ejpdRequest
    :<|> Named @"get-account-conference-calling-config" getAccountConferenceCallingConfig
    :<|> putAccountConferenceCallingConfig
    :<|> deleteAccountConferenceCallingConfig
    :<|> getConnectionsStatusUnqualified
    :<|> getConnectionsStatus

mlsAPI :: ServerT BrigIRoutes.MLSAPI (Handler r)
mlsAPI =
  ( \ref ->
      Named @"get-client-by-key-package-ref" (getClientByKeyPackageRef ref)
        :<|> ( Named @"put-conversation-by-key-package-ref" (putConvIdByKeyPackageRef ref)
                 :<|> Named @"get-conversation-by-key-package-ref" (getConvIdByKeyPackageRef ref)
             )
        :<|> Named @"put-key-package-ref" (putKeyPackageRef ref)
        :<|> Named @"post-key-package-ref" (postKeyPackageRef ref)
  )
    :<|> getMLSClients
    :<|> mapKeyPackageRefsInternal
    :<|> Named @"put-key-package-add" upsertKeyPackage

accountAPI ::
  ( Member BlacklistStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r
  ) =>
  ServerT BrigIRoutes.AccountAPI (Handler r)
accountAPI =
  Named @"createUserNoVerify" (callsFed (exposeAnnotations createUserNoVerify))
    :<|> Named @"createUserNoVerifySpar" (callsFed (exposeAnnotations createUserNoVerifySpar))

teamsAPI :: ServerT BrigIRoutes.TeamsAPI (Handler r)
teamsAPI = Named @"updateSearchVisibilityInbound" Index.updateSearchVisibilityInbound

userAPI :: ServerT BrigIRoutes.UserAPI (Handler r)
userAPI =
  updateLocale
    :<|> deleteLocale
    :<|> getDefaultUserLocale

authAPI :: (Member GalleyProvider r) => ServerT BrigIRoutes.AuthAPI (Handler r)
authAPI =
  Named @"legalhold-login" (callsFed (exposeAnnotations legalHoldLogin))
    :<|> Named @"sso-login" (callsFed (exposeAnnotations ssoLogin))
    :<|> Named @"login-code" getLoginCode
    :<|> Named @"reauthenticate" reauthenticate

-- | Responds with 'Nothing' if field is NULL in existing user or user does not exist.
getAccountConferenceCallingConfig :: UserId -> (Handler r) (ApiFt.WithStatusNoLock ApiFt.ConferenceCallingConfig)
getAccountConferenceCallingConfig uid =
  lift (wrapClient $ Data.lookupFeatureConferenceCalling uid)
    >>= maybe (ApiFt.forgetLock <$> view (settings . getAfcConferenceCallingDefNull)) pure

putAccountConferenceCallingConfig :: UserId -> ApiFt.WithStatusNoLock ApiFt.ConferenceCallingConfig -> (Handler r) NoContent
putAccountConferenceCallingConfig uid status =
  lift $ wrapClient $ Data.updateFeatureConferenceCalling uid (Just status) $> NoContent

deleteAccountConferenceCallingConfig :: UserId -> (Handler r) NoContent
deleteAccountConferenceCallingConfig uid =
  lift $ wrapClient $ Data.updateFeatureConferenceCalling uid Nothing $> NoContent

getClientByKeyPackageRef :: KeyPackageRef -> Handler r (Maybe ClientIdentity)
getClientByKeyPackageRef = runMaybeT . mapMaybeT wrapClientE . Data.derefKeyPackage

-- Used by galley to update conversation id in mls_key_package_ref
putConvIdByKeyPackageRef :: KeyPackageRef -> Qualified ConvId -> Handler r Bool
putConvIdByKeyPackageRef ref = lift . wrapClient . Data.keyPackageRefSetConvId ref

-- Used by galley to create a new record in mls_key_package_ref
putKeyPackageRef :: KeyPackageRef -> NewKeyPackageRef -> Handler r ()
putKeyPackageRef ref = lift . wrapClient . Data.addKeyPackageRef ref

-- Used by galley to retrieve conversation id from mls_key_package_ref
getConvIdByKeyPackageRef :: KeyPackageRef -> Handler r (Maybe (Qualified ConvId))
getConvIdByKeyPackageRef = runMaybeT . mapMaybeT wrapClientE . Data.keyPackageRefConvId

-- Used by galley to update key packages in mls_key_package_ref on commits with update_path
postKeyPackageRef :: KeyPackageRef -> KeyPackageRef -> Handler r ()
postKeyPackageRef ref = lift . wrapClient . Data.updateKeyPackageRef ref

-- Used by galley to update key package refs and also validate
upsertKeyPackage :: NewKeyPackage -> Handler r NewKeyPackageResult
upsertKeyPackage nkp = do
  kp <-
    either
      (const $ mlsProtocolError "upsertKeyPackage: Cannot decocode KeyPackage")
      pure
      $ decodeMLS' @(RawMLS KeyPackage) (kpData . nkpKeyPackage $ nkp)
  ref <- kpRef' kp & noteH "upsertKeyPackage: Unsupported CipherSuite"

  identity <-
    either
      (const $ mlsProtocolError "upsertKeyPackage: Cannot decode ClientIdentity")
      pure
      $ kpIdentity (rmValue kp)
  mp <- lift . wrapClient . runMaybeT $ Data.derefKeyPackage ref
  when (isNothing mp) $ do
    void $ validateKeyPackage identity kp
    lift . wrapClient $
      Data.addKeyPackageRef
        ref
        ( NewKeyPackageRef
            (fst <$> cidQualifiedClient identity)
            (ciClient identity)
            (nkpConversation nkp)
        )

  pure $ NewKeyPackageResult identity ref
  where
    noteH :: Text -> Maybe a -> Handler r a
    noteH errMsg Nothing = mlsProtocolError errMsg
    noteH _ (Just y) = pure y

getMLSClients :: UserId -> SignatureSchemeTag -> Handler r (Set ClientInfo)
getMLSClients usr _ss = do
  -- FUTUREWORK: check existence of key packages with a given ciphersuite
  lusr <- qualifyLocal usr
  allClients <- lift (wrapClient (API.lookupUsersClientIds (pure usr))) >>= getResult
  clientInfo <- lift . wrapClient $ pooledMapConcurrentlyN 16 (getValidity lusr) (toList allClients)
  pure . Set.fromList . map (uncurry ClientInfo) $ clientInfo
  where
    getResult [] = pure mempty
    getResult ((u, cs) : rs)
      | u == usr = pure cs
      | otherwise = getResult rs

    getValidity lusr cid =
      (cid,) . (> 0)
        <$> Data.countKeyPackages lusr cid

mapKeyPackageRefsInternal :: KeyPackageBundle -> Handler r ()
mapKeyPackageRefsInternal bundle = do
  wrapClientE $
    for_ (kpbEntries bundle) $ \e ->
      Data.mapKeyPackageRef (kpbeRef e) (kpbeUser e) (kpbeClient e)

getVerificationCode :: UserId -> VerificationAction -> Handler r (Maybe Code.Value)
getVerificationCode uid action = do
  user <- wrapClientE $ Api.lookupUser NoPendingInvitations uid
  maybe (pure Nothing) (lookupCode action) (userEmail =<< user)
  where
    lookupCode :: VerificationAction -> Email -> (Handler r) (Maybe Code.Value)
    lookupCode a e = do
      key <- Code.mkKey (Code.ForEmail e)
      code <- wrapClientE $ Code.lookup key (Code.scopeFromAction a)
      pure $ Code.codeValue <$> code

---------------------------------------------------------------------------
-- Sitemap (wai-route)

sitemap ::
  ( Member CodeStore r,
    Member PasswordResetStore r,
    Member BlacklistStore r,
    Member BlacklistPhonePrefixStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r
  ) =>
  Routes a (Handler r) ()
sitemap = unsafeCallsFed @'Brig @"on-user-deleted-connections" $ do
  get "/i/status" (continue $ const $ pure empty) true
  head "/i/status" (continue $ const $ pure empty) true

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
  delete "/i/users/:uid" (continue deleteUserNoAuthH) $
    capture "uid"

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
      .&. opt (param "skip_reauth")
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
  Search.routesInternal
  Team.routesInternal

---------------------------------------------------------------------------
-- Handlers

-- | Add a client without authentication checks
addClientInternalH ::
  (Member GalleyProvider r) =>
  UserId ::: Maybe Bool ::: JsonRequest NewClient ::: Maybe ConnId ::: JSON ->
  (Handler r) Response
addClientInternalH (usr ::: mSkipReAuth ::: req ::: connId ::: _) = do
  new <- parseJsonBody req
  setStatus status201 . json <$> addClientInternal usr mSkipReAuth new connId

addClientInternal ::
  (Member GalleyProvider r) =>
  UserId ->
  Maybe Bool ->
  NewClient ->
  Maybe ConnId ->
  (Handler r) Client
addClientInternal usr mSkipReAuth new connId = do
  let policy
        | mSkipReAuth == Just True = \_ _ -> False
        | otherwise = Data.reAuthForNewClients
  API.addClientWithReAuthPolicy policy usr connId Nothing new !>> clientError

legalHoldClientRequestedH :: UserId ::: JsonRequest LegalHoldClientRequest ::: JSON -> (Handler r) Response
legalHoldClientRequestedH (targetUser ::: req ::: _) = do
  clientRequest <- parseJsonBody req
  lift $ API.legalHoldClientRequested targetUser clientRequest
  pure $ setStatus status200 empty

removeLegalHoldClientH :: UserId ::: JSON -> (Handler r) Response
removeLegalHoldClientH (uid ::: _) = do
  lift $ API.removeLegalHoldClient uid
  pure $ setStatus status200 empty

internalListClientsH :: JSON ::: JsonRequest UserSet -> (Handler r) Response
internalListClientsH (_ ::: req) = do
  json <$> (lift . internalListClients =<< parseJsonBody req)

internalListClients :: UserSet -> (AppT r) UserClients
internalListClients (UserSet usrs) = do
  UserClients . Map.fromList
    <$> wrapClient (API.lookupUsersClientIds (Set.toList usrs))

internalListFullClientsH :: JSON ::: JsonRequest UserSet -> (Handler r) Response
internalListFullClientsH (_ ::: req) =
  json <$> (lift . internalListFullClients =<< parseJsonBody req)

internalListFullClients :: UserSet -> (AppT r) UserClientsFull
internalListFullClients (UserSet usrs) =
  UserClientsFull <$> wrapClient (Data.lookupClientsBulk (Set.toList usrs))

createUserNoVerify ::
  ( Member BlacklistStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r
  ) =>
  NewUser ->
  (Handler r) (Either RegisterError SelfProfile)
createUserNoVerify uData = lift . runExceptT $ do
  result <- API.createUser uData
  let acc = createdAccount result
  let usr = accountUser acc
  let uid = userId usr
  let eac = createdEmailActivation result
  let pac = createdPhoneActivation result
  for_ (catMaybes [eac, pac]) $ \adata ->
    let key = ActivateKey $ activationKey adata
        code = activationCode adata
     in API.activate key code (Just uid) !>> activationErrorToRegisterError
  pure . SelfProfile $ usr

createUserNoVerifySpar ::
  (Member GalleyProvider r) =>
  NewUserSpar ->
  (Handler r) (Either CreateUserSparError SelfProfile)
createUserNoVerifySpar uData =
  lift . runExceptT $ do
    result <- API.createUserSpar uData
    let acc = createdAccount result
    let usr = accountUser acc
    let uid = userId usr
    let eac = createdEmailActivation result
    let pac = createdPhoneActivation result
    for_ (catMaybes [eac, pac]) $ \adata ->
      let key = ActivateKey $ activationKey adata
          code = activationCode adata
       in API.activate key code (Just uid) !>> CreateUserSparRegistrationError . activationErrorToRegisterError
    pure . SelfProfile $ usr

deleteUserNoAuthH :: UserId -> (Handler r) Response
deleteUserNoAuthH uid = do
  r <- lift $ wrapHttp $ API.ensureAccountDeleted uid
  case r of
    NoUser -> throwStd (errorToWai @'E.UserNotFound)
    AccountAlreadyDeleted -> pure $ setStatus ok200 empty
    AccountDeleted -> pure $ setStatus accepted202 empty

changeSelfEmailMaybeSendH :: Member BlacklistStore r => UserId ::: Bool ::: JsonRequest EmailUpdate -> (Handler r) Response
changeSelfEmailMaybeSendH (u ::: validate ::: req) = do
  email <- euEmail <$> parseJsonBody req
  changeSelfEmailMaybeSend u (if validate then ActuallySendEmail else DoNotSendEmail) email API.AllowSCIMUpdates >>= \case
    ChangeEmailResponseIdempotent -> pure (setStatus status204 empty)
    ChangeEmailResponseNeedsActivation -> pure (setStatus status202 empty)

data MaybeSendEmail = ActuallySendEmail | DoNotSendEmail

changeSelfEmailMaybeSend :: Member BlacklistStore r => UserId -> MaybeSendEmail -> Email -> API.AllowSCIMUpdates -> (Handler r) ChangeEmailResponse
changeSelfEmailMaybeSend u ActuallySendEmail email allowScim = do
  API.changeSelfEmail u email allowScim
changeSelfEmailMaybeSend u DoNotSendEmail email allowScim = do
  API.changeEmail u email allowScim !>> changeEmailError >>= \case
    ChangeEmailIdempotent -> pure ChangeEmailResponseIdempotent
    ChangeEmailNeedsActivation _ -> pure ChangeEmailResponseNeedsActivation

listActivatedAccountsH :: JSON ::: Either (List UserId) (List Handle) ::: Bool -> (Handler r) Response
listActivatedAccountsH (_ ::: qry ::: includePendingInvitations) = do
  json <$> lift (listActivatedAccounts qry includePendingInvitations)

listActivatedAccounts :: Either (List UserId) (List Handle) -> Bool -> (AppT r) [UserAccount]
listActivatedAccounts elh includePendingInvitations = do
  Log.debug (Log.msg $ "listActivatedAccounts: " <> show (elh, includePendingInvitations))
  case elh of
    Left us -> byIds (fromList us)
    Right hs -> do
      us <- mapM (wrapClient . API.lookupHandle) (fromList hs)
      byIds (catMaybes us)
  where
    byIds :: [UserId] -> (AppT r) [UserAccount]
    byIds uids = wrapClient (API.lookupAccounts uids) >>= filterM accountValid

    accountValid :: UserAccount -> (AppT r) Bool
    accountValid account = case userIdentity . accountUser $ account of
      Nothing -> pure False
      Just ident ->
        case (accountStatus account, includePendingInvitations, emailIdentity ident) of
          (PendingInvitation, False, _) -> pure False
          (PendingInvitation, True, Just email) -> do
            hasInvitation <- isJust <$> wrapClient (lookupInvitationByEmail HideInvitationUrl email)
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

listAccountsByIdentityH :: JSON ::: Either Email Phone ::: Bool -> (Handler r) Response
listAccountsByIdentityH (_ ::: emailOrPhone ::: includePendingInvitations) =
  lift $
    json
      <$> API.lookupAccountsByIdentity emailOrPhone includePendingInvitations

getActivationCodeH :: JSON ::: Either Email Phone -> (Handler r) Response
getActivationCodeH (_ ::: emailOrPhone) = do
  json <$> getActivationCode emailOrPhone

getActivationCode :: Either Email Phone -> (Handler r) GetActivationCodeResp
getActivationCode emailOrPhone = do
  apair <- lift . wrapClient $ API.lookupActivationCode emailOrPhone
  maybe (throwStd activationKeyNotFound) (pure . GetActivationCodeResp) apair

newtype GetActivationCodeResp = GetActivationCodeResp (ActivationKey, ActivationCode)

instance ToJSON GetActivationCodeResp where
  toJSON (GetActivationCodeResp (k, c)) = object ["key" .= k, "code" .= c]

getPasswordResetCodeH ::
  ( Member CodeStore r,
    Member PasswordResetStore r
  ) =>
  JSON ::: Either Email Phone ->
  (Handler r) Response
getPasswordResetCodeH (_ ::: emailOrPhone) = do
  maybe (throwStd (errorToWai @'E.InvalidPasswordResetKey)) (pure . json) =<< lift (getPasswordResetCode emailOrPhone)

getPasswordResetCode ::
  ( Member CodeStore r,
    Member PasswordResetStore r
  ) =>
  Either Email Phone ->
  (AppT r) (Maybe GetPasswordResetCodeResp)
getPasswordResetCode emailOrPhone =
  GetPasswordResetCodeResp <$$> API.lookupPasswordResetCode emailOrPhone

newtype GetPasswordResetCodeResp = GetPasswordResetCodeResp (PasswordResetKey, PasswordResetCode)

instance ToJSON GetPasswordResetCodeResp where
  toJSON (GetPasswordResetCodeResp (k, c)) = object ["key" .= k, "code" .= c]

changeAccountStatusH :: UserId ::: JsonRequest AccountStatusUpdate -> (Handler r) Response
changeAccountStatusH (usr ::: req) = do
  status <- suStatus <$> parseJsonBody req
  wrapHttpClientE (API.changeSingleAccountStatus usr status) !>> accountStatusError
  pure empty

getAccountStatusH :: JSON ::: UserId -> (Handler r) Response
getAccountStatusH (_ ::: usr) = do
  status <- lift $ wrapClient $ API.lookupStatus usr
  pure $ case status of
    Just s -> json $ AccountStatusResp s
    Nothing -> setStatus status404 empty

getConnectionsStatusUnqualified :: ConnectionsStatusRequest -> Maybe Relation -> (Handler r) [ConnectionStatus]
getConnectionsStatusUnqualified ConnectionsStatusRequest {csrFrom, csrTo} flt = lift $ do
  r <- wrapClient $ maybe (API.lookupConnectionStatus' csrFrom) (API.lookupConnectionStatus csrFrom) csrTo
  pure $ maybe r (filterByRelation r) flt
  where
    filterByRelation l rel = filter ((== rel) . csStatus) l

getConnectionsStatus :: ConnectionsStatusRequestV2 -> (Handler r) [ConnectionStatusV2]
getConnectionsStatus (ConnectionsStatusRequestV2 froms mtos mrel) = do
  loc <- qualifyLocal ()
  conns <- lift $ case mtos of
    Nothing -> wrapClient . Data.lookupAllStatuses =<< qualifyLocal froms
    Just tos -> do
      let getStatusesForOneDomain =
            wrapClient
              <$> foldQualified
                loc
                (Data.lookupLocalConnectionStatuses froms)
                (Data.lookupRemoteConnectionStatuses froms)
      concat <$> mapM getStatusesForOneDomain (bucketQualified tos)
  pure $ maybe conns (filterByRelation conns) mrel
  where
    filterByRelation l rel = filter ((== rel) . csv2Status) l

revokeIdentityH :: Either Email Phone -> (Handler r) Response
revokeIdentityH emailOrPhone = do
  lift $ API.revokeIdentity emailOrPhone
  pure $ setStatus status200 empty

updateConnectionInternalH :: JSON ::: JsonRequest UpdateConnectionsInternal -> (Handler r) Response
updateConnectionInternalH (_ ::: req) = do
  updateConn <- parseJsonBody req
  API.updateConnectionInternal updateConn !>> connError
  pure $ setStatus status200 empty

checkBlacklistH :: Member BlacklistStore r => Either Email Phone -> (Handler r) Response
checkBlacklistH emailOrPhone = do
  bl <- lift $ API.isBlacklisted emailOrPhone
  pure $ setStatus (bool status404 status200 bl) empty

deleteFromBlacklistH :: Member BlacklistStore r => Either Email Phone -> (Handler r) Response
deleteFromBlacklistH emailOrPhone = do
  void . lift $ API.blacklistDelete emailOrPhone
  pure empty

addBlacklistH :: Member BlacklistStore r => Either Email Phone -> (Handler r) Response
addBlacklistH emailOrPhone = do
  void . lift $ API.blacklistInsert emailOrPhone
  pure empty

-- | Get any matching prefixes. Also try for shorter prefix matches,
-- i.e. checking for +123456 also checks for +12345, +1234, ...
getPhonePrefixesH :: Member BlacklistPhonePrefixStore r => PhonePrefix -> (Handler r) Response
getPhonePrefixesH prefix = do
  results <- lift $ API.phonePrefixGet prefix
  pure $ case results of
    [] -> setStatus status404 empty
    _ -> json results

-- | Delete a phone prefix entry (must be an exact match)
deleteFromPhonePrefixH :: Member BlacklistPhonePrefixStore r => PhonePrefix -> (Handler r) Response
deleteFromPhonePrefixH prefix = do
  void . lift $ API.phonePrefixDelete prefix
  pure empty

addPhonePrefixH :: Member BlacklistPhonePrefixStore r => JSON ::: JsonRequest ExcludedPrefix -> (Handler r) Response
addPhonePrefixH (_ ::: req) = do
  prefix :: ExcludedPrefix <- parseJsonBody req
  void . lift $ API.phonePrefixInsert prefix
  pure empty

updateSSOIdH :: UserId ::: JSON ::: JsonRequest UserSSOId -> (Handler r) Response
updateSSOIdH (uid ::: _ ::: req) = do
  ssoid :: UserSSOId <- parseJsonBody req
  success <- lift $ wrapClient $ Data.updateSSOId uid (Just ssoid)
  if success
    then do
      lift $ wrapHttpClient $ Intra.onUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOId = Just ssoid}))
      pure empty
    else pure . setStatus status404 $ plain "User does not exist or has no team."

deleteSSOIdH :: UserId ::: JSON -> (Handler r) Response
deleteSSOIdH (uid ::: _) = do
  success <- lift $ wrapClient $ Data.updateSSOId uid Nothing
  if success
    then do
      lift $ wrapHttpClient $ Intra.onUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOIdRemoved = True}))
      pure empty
    else pure . setStatus status404 $ plain "User does not exist or has no team."

updateManagedByH :: UserId ::: JSON ::: JsonRequest ManagedByUpdate -> (Handler r) Response
updateManagedByH (uid ::: _ ::: req) = do
  ManagedByUpdate managedBy <- parseJsonBody req
  lift $ wrapClient $ Data.updateManagedBy uid managedBy
  pure empty

updateRichInfoH :: UserId ::: JSON ::: JsonRequest RichInfoUpdate -> (Handler r) Response
updateRichInfoH (uid ::: _ ::: req) = do
  empty <$ (updateRichInfo uid =<< parseJsonBody req)

updateRichInfo :: UserId -> RichInfoUpdate -> (Handler r) ()
updateRichInfo uid rup = do
  let (unRichInfoAssocList -> richInfo) = normalizeRichInfoAssocList . riuRichInfo $ rup
  maxSize <- setRichInfoLimit <$> view settings
  when (richInfoSize (RichInfo (mkRichInfoAssocList richInfo)) > maxSize) $ throwStd tooLargeRichInfo
  -- FUTUREWORK: send an event
  -- Intra.onUserEvent uid (Just conn) (richInfoUpdate uid ri)
  lift $ wrapClient $ Data.updateRichInfo uid (mkRichInfoAssocList richInfo)

updateLocale :: UserId -> LocaleUpdate -> (Handler r) LocaleUpdate
updateLocale uid locale = do
  lift $ wrapClient $ Data.updateLocale uid (luLocale locale)
  pure locale

deleteLocale :: UserId -> (Handler r) NoContent
deleteLocale uid = do
  defLoc <- setDefaultUserLocale <$> view settings
  lift $ wrapClient $ Data.updateLocale uid defLoc $> NoContent

getDefaultUserLocale :: (Handler r) LocaleUpdate
getDefaultUserLocale = do
  defLocale <- setDefaultUserLocale <$> view settings
  pure $ LocaleUpdate defLocale

getRichInfoH :: UserId -> (Handler r) Response
getRichInfoH uid = json <$> getRichInfo uid

getRichInfo :: UserId -> (Handler r) RichInfo
getRichInfo uid = RichInfo . fromMaybe mempty <$> lift (wrapClient $ API.lookupRichInfo uid)

getRichInfoMultiH :: List UserId -> (Handler r) Response
getRichInfoMultiH uids = json <$> getRichInfoMulti (List.fromList uids)

getRichInfoMulti :: [UserId] -> (Handler r) [(UserId, RichInfo)]
getRichInfoMulti uids =
  lift (wrapClient $ API.lookupRichInfoMultiUsers uids)

updateHandleH :: UserId ::: JSON ::: JsonRequest HandleUpdate -> (Handler r) Response
updateHandleH (uid ::: _ ::: body) = empty <$ (updateHandle uid =<< parseJsonBody body)

updateHandle :: UserId -> HandleUpdate -> (Handler r) ()
updateHandle uid (HandleUpdate handleUpd) = do
  handle <- validateHandle handleUpd
  API.changeHandle uid Nothing handle API.AllowSCIMUpdates !>> changeHandleError

updateUserNameH :: UserId ::: JSON ::: JsonRequest NameUpdate -> (Handler r) Response
updateUserNameH (uid ::: _ ::: body) = empty <$ (updateUserName uid =<< parseJsonBody body)

updateUserName :: UserId -> NameUpdate -> (Handler r) ()
updateUserName uid (NameUpdate nameUpd) = do
  name <- either (const $ throwStd (errorToWai @'E.InvalidUser)) pure $ mkName nameUpd
  let uu =
        UserUpdate
          { uupName = Just name,
            uupPict = Nothing,
            uupAssets = Nothing,
            uupAccentId = Nothing
          }
  lift (wrapClient $ Data.lookupUser WithPendingInvitations uid) >>= \case
    Just _ -> API.updateUser uid Nothing uu API.AllowSCIMUpdates !>> updateProfileError
    Nothing -> throwStd (errorToWai @'E.InvalidUser)

checkHandleInternalH :: Text -> (Handler r) Response
checkHandleInternalH =
  API.checkHandle >=> \case
    API.CheckHandleInvalid -> throwE (StdError (errorToWai @'E.InvalidHandle))
    API.CheckHandleFound -> pure $ setStatus status200 empty
    API.CheckHandleNotFound -> pure $ setStatus status404 empty

getContactListH :: JSON ::: UserId -> (Handler r) Response
getContactListH (_ ::: uid) = do
  contacts <- lift . wrapClient $ API.lookupContactList uid
  pure $ json $ UserIds contacts
