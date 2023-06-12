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
import Data.CommaSeparatedList
import Data.Handle
import Data.Id as Id
import qualified Data.Map.Strict as Map
import Data.Qualified
import qualified Data.Set as Set
import Imports hiding (cs, head)
import qualified Imports
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Routing hiding (toList)
import Network.Wai.Utilities as Utilities
import Network.Wai.Utilities.ZAuth (zauthConnId)
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
import Wire.API.User.RichInfo

---------------------------------------------------------------------------
-- Sitemap (servant)

servantSitemap ::
  forall r p.
  ( Member BlacklistStore r,
    Member CodeStore r,
    Member BlacklistPhonePrefixStore r,
    Member PasswordResetStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r
  ) =>
  ServerT BrigIRoutes.API (Handler r)
servantSitemap =
  istatusAPI
    :<|> ejpdAPI
    :<|> accountAPI
    :<|> mlsAPI
    :<|> getVerificationCode
    :<|> teamsAPI
    :<|> userAPI
    :<|> authAPI
    :<|> internalOauthAPI
    :<|> internalSearchIndexAPI

istatusAPI :: forall r. ServerT BrigIRoutes.IStatusAPI (Handler r)
istatusAPI = Named @"get-status" (pure NoContent)

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
    Member CodeStore r,
    Member BlacklistPhonePrefixStore r,
    Member PasswordResetStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r
  ) =>
  ServerT BrigIRoutes.AccountAPI (Handler r)
accountAPI =
  Named @"createUserNoVerify" (callsFed (exposeAnnotations createUserNoVerify))
    :<|> Named @"createUserNoVerifySpar" (callsFed (exposeAnnotations createUserNoVerifySpar))
    :<|> Named @"putSelfEmail" changeSelfEmailMaybeSendH
    :<|> Named @"iDeleteUser" deleteUserNoAuthH
    :<|> Named @"iPutUserStatus" changeAccountStatusH
    :<|> Named @"iGetUserStatus" getAccountStatusH
    :<|> Named @"iGetUsersByEmailOrPhone" listAccountsByIdentityH
    :<|> Named @"iGetUsersByIdsOrHandles" listActivatedAccountsH
    :<|> Named @"iGetUserContacts" getContactListH
    :<|> Named @"iGetUserActivationCode" getActivationCodeH
    :<|> Named @"iGetUserPasswordResetCode" getPasswordResetCodeH
    :<|> Named @"iRevokeIdentity" revokeIdentityH
    :<|> Named @"iHeadBlacklist" checkBlacklistH
    :<|> Named @"iDeleteBlacklist" deleteFromBlacklistH
    :<|> Named @"iPostBlacklist" addBlacklistH
    :<|> Named @"iGetPhonePrefix" (callsFed (exposeAnnotations getPhonePrefixesH))
    :<|> Named @"iDeletePhonePrefix" deleteFromPhonePrefixH
    :<|> Named @"iPostPhonePrefix" addPhonePrefixH
    :<|> Named @"iPutUserSsoId" updateSSOIdH
    :<|> Named @"iDeleteUserSsoId" deleteSSOIdH
    :<|> Named @"iPutManagedBy" updateManagedByH
    :<|> Named @"iPutRichInfo" updateRichInfoH
    :<|> Named @"iPutHandle" updateHandleH
    :<|> Named @"iPutHandle" updateUserNameH
    :<|> Named @"iGetRichInfo" getRichInfoH
    :<|> Named @"iGetRichInfoMulti" getRichInfoMultiH
    :<|> Named @"iHeadHandle" checkHandleInternalH

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
  user <- wrapClientE $ API.lookupUser NoPendingInvitations uid
  maybe (pure Nothing) (lookupCode action) (userEmail =<< user)
  where
    lookupCode :: VerificationAction -> Email -> (Handler r) (Maybe Code.Value)
    lookupCode a e = do
      key <- Code.mkKey (Code.ForEmail e)
      code <- wrapClientE $ Code.lookup key (Code.scopeFromAction a)
      pure $ Code.codeValue <$> code

internalSearchIndexAPI :: forall r. ServerT BrigIRoutes.ISearchIndexAPI (Handler r)
internalSearchIndexAPI =
  Named @"indexRefresh" (NoContent <$ lift (wrapClient Search.refreshIndex))
    :<|> Named @"indexReindex" (NoContent <$ lift (wrapClient Search.reindexAll))
    :<|> Named @"indexReindexIfSameOrNewer" (NoContent <$ lift (wrapClient Search.reindexAllIfSameOrNewer))

---------------------------------------------------------------------------
-- Sitemap (wai-route)

sitemap ::
  ( Member BlacklistStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r
  ) =>
  Routes a (Handler r) ()
sitemap = unsafeCallsFed @'Brig @"on-user-deleted-connections" $ do
  put "/i/connections/connection-update" (continue updateConnectionInternalH) $
    accept "application" "json"
      .&. jsonRequest @UpdateConnectionsInternal

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

deleteUserNoAuthH :: UserId -> (Handler r) DeleteUserResponse
deleteUserNoAuthH uid = do
  r <- lift $ wrapHttp $ API.ensureAccountDeleted uid
  case r of
    NoUser -> throwStd (errorToWai @'E.UserNotFound)
    AccountAlreadyDeleted -> pure UserResponseAccountAlreadyDeleted
    AccountDeleted -> pure UserResponseAccountDeleted

changeSelfEmailMaybeSendH :: Member BlacklistStore r => UserId -> EmailUpdate -> Maybe Bool -> (Handler r) ChangeEmailResponse
changeSelfEmailMaybeSendH u body (fromMaybe False -> validate) = do
  let email = euEmail body
  changeSelfEmailMaybeSend u (if validate then ActuallySendEmail else DoNotSendEmail) email API.AllowSCIMUpdates

data MaybeSendEmail = ActuallySendEmail | DoNotSendEmail

changeSelfEmailMaybeSend :: Member BlacklistStore r => UserId -> MaybeSendEmail -> Email -> API.AllowSCIMUpdates -> (Handler r) ChangeEmailResponse
changeSelfEmailMaybeSend u ActuallySendEmail email allowScim = do
  API.changeSelfEmail u email allowScim
changeSelfEmailMaybeSend u DoNotSendEmail email allowScim = do
  API.changeEmail u email allowScim !>> changeEmailError >>= \case
    ChangeEmailIdempotent -> pure ChangeEmailResponseIdempotent
    ChangeEmailNeedsActivation _ -> pure ChangeEmailResponseNeedsActivation

listActivatedAccountsH :: Maybe (CommaSeparatedList UserId) -> Maybe (CommaSeparatedList Handle) -> Maybe Bool -> (Handler r) [UserAccount]
listActivatedAccountsH
  (maybe [] fromCommaSeparatedList -> uids)
  (maybe [] fromCommaSeparatedList -> handles)
  (fromMaybe False -> includePendingInvitations) = lift $ do
    u1 <- listActivatedAccounts (Left uids) includePendingInvitations
    u2 <- listActivatedAccounts (Right handles) includePendingInvitations
    pure $ u1 <> u2

listActivatedAccounts :: Either [UserId] [Handle] -> Bool -> (AppT r) [UserAccount]
listActivatedAccounts elh includePendingInvitations = do
  Log.debug (Log.msg $ "listActivatedAccounts: " <> show (elh, includePendingInvitations))
  case elh of
    Left us -> byIds us
    Right hs -> do
      us <- mapM (wrapClient . API.lookupHandle) hs
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

listAccountsByIdentityH :: Maybe Email -> Maybe Phone -> Maybe Bool -> (Handler r) [UserAccount]
listAccountsByIdentityH mbEmail mbPhone (fromMaybe False -> includePendingInvitations) =
  lift $ do
    u1 <- maybe (pure []) (\email -> API.lookupAccountsByIdentity (Left email) includePendingInvitations) mbEmail
    u2 <- maybe (pure []) (\phone -> API.lookupAccountsByIdentity (Right phone) includePendingInvitations) mbPhone
    pure $ u1 <> u2

getActivationCodeH :: Maybe Email -> Maybe Phone -> (Handler r) GetActivationCodeResp
getActivationCodeH (Just email) Nothing = getActivationCode (Left email)
getActivationCodeH Nothing (Just phone) = getActivationCode (Right phone)
getActivationCodeH bade badp = throwStd (badRequest ("need exactly one of email, phone: " <> Imports.cs (show (bade, badp))))

getActivationCode :: Either Email Phone -> (Handler r) GetActivationCodeResp
getActivationCode emailOrPhone = do
  apair <- lift . wrapClient $ API.lookupActivationCode emailOrPhone
  maybe (throwStd activationKeyNotFound) (pure . GetActivationCodeResp) apair

getPasswordResetCodeH ::
  ( Member CodeStore r,
    Member PasswordResetStore r
  ) =>
  Maybe Email ->
  Maybe Phone ->
  (Handler r) GetPasswordResetCodeResp
getPasswordResetCodeH (Just email) Nothing = getPasswordResetCode (Left email)
getPasswordResetCodeH Nothing (Just phone) = getPasswordResetCode (Right phone)
getPasswordResetCodeH bade badp = throwStd (badRequest ("need exactly one of email, phone: " <> Imports.cs (show (bade, badp))))

getPasswordResetCode ::
  ( Member CodeStore r,
    Member PasswordResetStore r
  ) =>
  Either Email Phone ->
  (Handler r) GetPasswordResetCodeResp
getPasswordResetCode emailOrPhone =
  (GetPasswordResetCodeResp <$$> lift (API.lookupPasswordResetCode emailOrPhone)) >>= maybe (throwStd (errorToWai @'E.InvalidPasswordResetKey)) pure

changeAccountStatusH :: UserId -> AccountStatusUpdate -> (Handler r) NoContent
changeAccountStatusH usr (suStatus -> status) = do
  wrapHttpClientE (API.changeSingleAccountStatus usr status) !>> accountStatusError -- FUTUREWORK: use CanThrow and related machinery
  pure NoContent

getAccountStatusH :: UserId -> (Handler r) AccountStatusResp
getAccountStatusH uid = do
  status <- lift $ wrapClient $ API.lookupStatus uid
  maybe
    (throwStd (errorToWai @'E.UserNotFound))
    (pure . AccountStatusResp)
    status

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

revokeIdentityH :: Maybe Email -> Maybe Phone -> (Handler r) NoContent
revokeIdentityH (Just email) Nothing = lift $ NoContent <$ API.revokeIdentity (Left email)
revokeIdentityH Nothing (Just phone) = lift $ NoContent <$ API.revokeIdentity (Right phone)
revokeIdentityH bade badp = throwStd (badRequest ("need exactly one of email, phone: " <> Imports.cs (show (bade, badp))))

updateConnectionInternalH :: JSON ::: JsonRequest UpdateConnectionsInternal -> (Handler r) Response
updateConnectionInternalH (_ ::: req) = do
  updateConn <- parseJsonBody req
  API.updateConnectionInternal updateConn !>> connError
  pure $ setStatus status200 empty

checkBlacklistH :: Member BlacklistStore r => Maybe Email -> Maybe Phone -> (Handler r) CheckBlacklistResponse
checkBlacklistH (Just email) Nothing = checkBlacklist (Left email)
checkBlacklistH Nothing (Just phone) = checkBlacklist (Right phone)
checkBlacklistH bade badp = throwStd (badRequest ("need exactly one of email, phone: " <> Imports.cs (show (bade, badp))))

checkBlacklist :: Member BlacklistStore r => Either Email Phone -> (Handler r) CheckBlacklistResponse
checkBlacklist emailOrPhone = lift $ bool NotBlacklisted YesBlacklisted <$> API.isBlacklisted emailOrPhone

deleteFromBlacklistH :: Member BlacklistStore r => Maybe Email -> Maybe Phone -> (Handler r) NoContent
deleteFromBlacklistH (Just email) Nothing = deleteFromBlacklist (Left email)
deleteFromBlacklistH Nothing (Just phone) = deleteFromBlacklist (Right phone)
deleteFromBlacklistH bade badp = throwStd (badRequest ("need exactly one of email, phone: " <> Imports.cs (show (bade, badp))))

deleteFromBlacklist :: Member BlacklistStore r => Either Email Phone -> (Handler r) NoContent
deleteFromBlacklist emailOrPhone = lift $ NoContent <$ API.blacklistDelete emailOrPhone

addBlacklistH :: Member BlacklistStore r => Maybe Email -> Maybe Phone -> (Handler r) NoContent
addBlacklistH (Just email) Nothing = addBlacklist (Left email)
addBlacklistH Nothing (Just phone) = addBlacklist (Right phone)
addBlacklistH bade badp = throwStd (badRequest ("need exactly one of email, phone: " <> Imports.cs (show (bade, badp))))

addBlacklist :: Member BlacklistStore r => Either Email Phone -> (Handler r) NoContent
addBlacklist emailOrPhone = lift $ NoContent <$ API.blacklistInsert emailOrPhone

-- | Get any matching prefixes. Also try for shorter prefix matches,
-- i.e. checking for +123456 also checks for +12345, +1234, ...
getPhonePrefixesH :: Member BlacklistPhonePrefixStore r => PhonePrefix -> (Handler r) GetPhonePrefixResponse
getPhonePrefixesH prefix = lift $ do
  results <- API.phonePrefixGet prefix
  pure $ case results of
    [] -> PhonePrefixNotFound
    (_ : _) -> PhonePrefixesFound results

-- | Delete a phone prefix entry (must be an exact match)
deleteFromPhonePrefixH :: Member BlacklistPhonePrefixStore r => PhonePrefix -> (Handler r) NoContent
deleteFromPhonePrefixH prefix = lift $ NoContent <$ API.phonePrefixDelete prefix

addPhonePrefixH :: Member BlacklistPhonePrefixStore r => ExcludedPrefix -> (Handler r) NoContent
addPhonePrefixH prefix = lift $ NoContent <$ API.phonePrefixInsert prefix

updateSSOIdH :: UserId -> UserSSOId -> (Handler r) UpdateSSOIdResponse
updateSSOIdH uid ssoid = do
  success <- lift $ wrapClient $ Data.updateSSOId uid (Just ssoid)
  if success
    then do
      lift $ wrapHttpClient $ Intra.onUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOId = Just ssoid}))
      pure UpdateSSOIdSuccess
    else pure UpdateSSOIdNotFound

deleteSSOIdH :: UserId -> (Handler r) UpdateSSOIdResponse
deleteSSOIdH uid = do
  success <- lift $ wrapClient $ Data.updateSSOId uid Nothing
  if success
    then do
      lift $ wrapHttpClient $ Intra.onUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOIdRemoved = True}))
      pure UpdateSSOIdSuccess
    else pure UpdateSSOIdNotFound

updateManagedByH :: UserId -> ManagedByUpdate -> (Handler r) NoContent
updateManagedByH uid (ManagedByUpdate managedBy) = do
  NoContent <$ (lift $ wrapClient $ Data.updateManagedBy uid managedBy)

updateRichInfoH :: UserId -> RichInfoUpdate -> (Handler r) NoContent
updateRichInfoH uid rup =
  NoContent <$ do
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

getRichInfoH :: UserId -> (Handler r) RichInfo
getRichInfoH uid = RichInfo . fromMaybe mempty <$> lift (wrapClient $ API.lookupRichInfo uid)

getRichInfoMultiH :: CommaSeparatedList UserId -> (Handler r) [(UserId, RichInfo)]
getRichInfoMultiH (CommaSeparatedList uids) =
  lift $ wrapClient $ API.lookupRichInfoMultiUsers uids

updateHandleH :: UserId -> HandleUpdate -> (Handler r) NoContent
updateHandleH uid (HandleUpdate handleUpd) =
  NoContent <$ do
    handle <- validateHandle handleUpd
    API.changeHandle uid Nothing handle API.AllowSCIMUpdates !>> changeHandleError

updateUserNameH :: UserId -> NameUpdate -> (Handler r) NoContent
updateUserNameH uid (NameUpdate nameUpd) =
  NoContent <$ do
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

checkHandleInternalH :: Handle -> (Handler r) CheckHandleResponse
checkHandleInternalH (Handle h) =
  API.checkHandle h >>= \case
    API.CheckHandleInvalid -> throwE (StdError (errorToWai @'E.InvalidHandle))
    API.CheckHandleFound -> pure CheckHandleResponseFound
    API.CheckHandleNotFound -> pure CheckHandleResponseNotFound

getContactListH :: UserId -> (Handler r) UserIds
getContactListH uid = lift . wrapClient $ UserIds <$> API.lookupContactList uid
