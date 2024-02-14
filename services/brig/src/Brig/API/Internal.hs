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
import Brig.API.Client qualified as API
import Brig.API.Connection qualified as API
import Brig.API.Error
import Brig.API.Handler
import Brig.API.MLS.KeyPackages.Validation
import Brig.API.OAuth (internalOauthAPI)
import Brig.API.Types
import Brig.API.User qualified as API
import Brig.API.Util
import Brig.App
import Brig.Code qualified as Code
import Brig.Data.Activation
import Brig.Data.Client qualified as Data
import Brig.Data.Connection qualified as Data
import Brig.Data.MLS.KeyPackage qualified as Data
import Brig.Data.User qualified as Data
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.CodeStore (CodeStore)
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.FederationConfigStore (AddFederationRemoteResult (..), AddFederationRemoteTeamResult (..), FederationConfigStore, UpdateFederationResult (..))
import Brig.Effects.FederationConfigStore qualified as E
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.IO.Intra qualified as Intra
import Brig.Options hiding (internalEvents, sesQueue)
import Brig.Provider.API qualified as Provider
import Brig.Team.API qualified as Team
import Brig.Team.DB (lookupInvitationByEmail)
import Brig.Team.Types (ShowOrHideInvitationUrl (..))
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Brig.Types.User
import Brig.Types.User.Event (UserEvent (UserUpdated), UserUpdatedData (eupSSOId, eupSSOIdRemoved), emptyUserUpdatedData)
import Brig.User.API.Search qualified as Search
import Brig.User.EJPD qualified
import Brig.User.Search.Index qualified as Index
import Control.Error hiding (bool)
import Control.Lens (view)
import Data.ByteString.Conversion (toByteString)
import Data.CommaSeparatedList
import Data.Domain (Domain)
import Data.Handle
import Data.Id as Id
import Data.Map.Strict qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System
import Imports hiding (head)
import Network.Wai.Routing hiding (toList)
import Network.Wai.Utilities as Utilities
import Polysemy
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.OpenApi.Internal.Orphans ()
import System.Logger.Class qualified as Log
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Connection
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Federation.API
import Wire.API.Federation.Error (FederationError (..))
import Wire.API.MLS.CipherSuite
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Routes.Internal.Brig qualified as BrigIRoutes
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Named
import Wire.API.Team.Feature qualified as ApiFt
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client
import Wire.API.User.RichInfo
import Wire.NotificationSubsystem
import Wire.Sem.Concurrency
import Wire.Sem.Paging.Cassandra (InternalPaging)

---------------------------------------------------------------------------
-- Sitemap (servant)

servantSitemap ::
  forall r p.
  ( Member BlacklistStore r,
    Member CodeStore r,
    Member BlacklistPhonePrefixStore r,
    Member PasswordResetStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r,
    Member FederationConfigStore r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Concurrency 'Unsafe) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
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
    :<|> clientAPI
    :<|> authAPI
    :<|> internalOauthAPI
    :<|> internalSearchIndexAPI
    :<|> federationRemotesAPI

istatusAPI :: forall r. ServerT BrigIRoutes.IStatusAPI (Handler r)
istatusAPI = Named @"get-status" (pure NoContent)

ejpdAPI ::
  (Member GalleyProvider r, Member NotificationSubsystem r) =>
  ServerT BrigIRoutes.EJPD_API (Handler r)
ejpdAPI =
  Brig.User.EJPD.ejpdRequest
    :<|> Named @"get-account-conference-calling-config" getAccountConferenceCallingConfig
    :<|> putAccountConferenceCallingConfig
    :<|> deleteAccountConferenceCallingConfig
    :<|> getConnectionsStatusUnqualified
    :<|> getConnectionsStatus

mlsAPI :: ServerT BrigIRoutes.MLSAPI (Handler r)
mlsAPI = getMLSClients

accountAPI ::
  ( Member BlacklistStore r,
    Member CodeStore r,
    Member BlacklistPhonePrefixStore r,
    Member PasswordResetStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  ServerT BrigIRoutes.AccountAPI (Handler r)
accountAPI =
  Named @"createUserNoVerify" (callsFed (exposeAnnotations createUserNoVerify))
    :<|> Named @"createUserNoVerifySpar" (callsFed (exposeAnnotations createUserNoVerifySpar))
    :<|> Named @"putSelfEmail" changeSelfEmailMaybeSendH
    :<|> Named @"iDeleteUser" deleteUserNoAuthH
    :<|> Named @"iPutUserStatus" changeAccountStatusH
    :<|> Named @"iGetUserStatus" getAccountStatusH
    :<|> Named @"iGetUsersByVariousKeys" listActivatedAccountsH
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
    :<|> Named @"iConnectionUpdate" updateConnectionInternalH
    :<|> Named @"iListClients" internalListClientsH
    :<|> Named @"iListClientsFull" internalListFullClientsH
    :<|> Named @"iAddClient" addClientInternalH
    :<|> Named @"iLegalholdAddClient" legalHoldClientRequestedH
    :<|> Named @"iLegalholdDeleteClient" removeLegalHoldClientH

teamsAPI ::
  ( Member GalleyProvider r,
    Member (UserPendingActivationStore p) r,
    Member BlacklistStore r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Concurrency 'Unsafe) r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  ServerT BrigIRoutes.TeamsAPI (Handler r)
teamsAPI =
  Named @"updateSearchVisibilityInbound" Index.updateSearchVisibilityInbound
    :<|> Named @"get-invitation-by-email" Team.getInvitationByEmail
    :<|> Named @"get-invitation-code" Team.getInvitationCode
    :<|> Named @"suspend-team" Team.suspendTeam
    :<|> Named @"unsuspend-team" Team.unsuspendTeam
    :<|> Named @"team-size" Team.teamSize
    :<|> Named @"create-invitations-via-scim" Team.createInvitationViaScim

userAPI :: ServerT BrigIRoutes.UserAPI (Handler r)
userAPI =
  updateLocale
    :<|> deleteLocale
    :<|> getDefaultUserLocale

clientAPI :: ServerT BrigIRoutes.ClientAPI (Handler r)
clientAPI = Named @"update-client-last-active" updateClientLastActive

authAPI ::
  ( Member GalleyProvider r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  ServerT BrigIRoutes.AuthAPI (Handler r)
authAPI =
  Named @"legalhold-login" (callsFed (exposeAnnotations legalHoldLogin))
    :<|> Named @"sso-login" (callsFed (exposeAnnotations ssoLogin))
    :<|> Named @"login-code" getLoginCode
    :<|> Named @"reauthenticate" reauthenticate

federationRemotesAPI :: (Member FederationConfigStore r) => ServerT BrigIRoutes.FederationRemotesAPI (Handler r)
federationRemotesAPI =
  Named @"add-federation-remotes" addFederationRemote
    :<|> Named @"get-federation-remotes" getFederationRemotes
    :<|> Named @"update-federation-remotes" updateFederationRemote
    :<|> Named @"add-federation-remote-team" addFederationRemoteTeam
    :<|> Named @"get-federation-remote-teams" getFederationRemoteTeams
    :<|> Named @"delete-federation-remote-team" deleteFederationRemoteTeam

deleteFederationRemoteTeam :: (Member FederationConfigStore r) => Domain -> TeamId -> (Handler r) ()
deleteFederationRemoteTeam domain teamId =
  lift $ liftSem $ E.removeFederationRemoteTeam domain teamId

getFederationRemoteTeams :: (Member FederationConfigStore r) => Domain -> (Handler r) [FederationRemoteTeam]
getFederationRemoteTeams domain =
  lift $ liftSem $ E.getFederationRemoteTeams domain

addFederationRemoteTeam :: (Member FederationConfigStore r) => Domain -> FederationRemoteTeam -> (Handler r) ()
addFederationRemoteTeam domain rt =
  lift (liftSem $ E.addFederationRemoteTeam domain rt.teamId) >>= \case
    AddFederationRemoteTeamSuccess -> pure ()
    AddFederationRemoteTeamDomainNotFound ->
      throwError . fedError . FederationUnexpectedError $
        "Federation domain does not exist.  Please add it first."
    AddFederationRemoteTeamRestrictionAllowAll ->
      throwError . fedError . FederationUnexpectedError $
        "Federation is not configured to be restricted by teams. Therefore adding a team to a \
        \remote domain is not allowed."

getFederationRemotes :: (Member FederationConfigStore r) => (Handler r) FederationDomainConfigs
getFederationRemotes = lift $ liftSem $ E.getFederationConfigs

addFederationRemote :: (Member FederationConfigStore r) => FederationDomainConfig -> (Handler r) ()
addFederationRemote fedDomConf = do
  lift (liftSem $ E.addFederationConfig fedDomConf) >>= \case
    AddFederationRemoteSuccess -> pure ()
    AddFederationRemoteMaxRemotesReached ->
      throwError . fedError . FederationUnexpectedError $
        "Maximum number of remote backends reached.  If you need to create more connections, \
        \please contact wire.com."
    AddFederationRemoteDivergingConfig cfg ->
      throwError . fedError . FederationUnexpectedError $
        "keeping track of remote domains in the brig config file is deprecated, but as long as we \
        \do that, adding a domain with different settings than in the config file is not allowed.  want "
          <> ( "Just "
                 <> cs (show fedDomConf)
                 <> "or Nothing, "
             )
          <> ( "got "
                 <> cs (show (Map.lookup (domain fedDomConf) cfg))
             )

updateFederationRemote :: (Member FederationConfigStore r) => Domain -> FederationDomainConfig -> (Handler r) ()
updateFederationRemote dom fedcfg = do
  if (dom /= fedcfg.domain)
    then
      throwError . fedError . FederationUnexpectedError . cs $
        "federation domain of a given peer cannot be changed from " <> show (domain fedcfg) <> " to " <> show dom <> "."
    else
      lift (liftSem (E.updateFederationConfig fedcfg)) >>= \case
        UpdateFederationSuccess -> pure ()
        UpdateFederationRemoteNotFound ->
          throwError . fedError . FederationUnexpectedError . cs $
            "federation domain does not exist and cannot be updated: " <> show (dom, fedcfg)
        UpdateFederationRemoteDivergingConfig ->
          throwError . fedError . FederationUnexpectedError $
            "keeping track of remote domains in the brig config file is deprecated, but as long as we \
            \do that, removing or updating items listed in the config file is not allowed."

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

getMLSClients :: UserId -> CipherSuite -> Handler r (Set ClientInfo)
getMLSClients usr suite = do
  lusr <- qualifyLocal usr
  suiteTag <- maybe (mlsProtocolError "Unknown ciphersuite") pure (cipherSuiteTag suite)
  allClients <- lift (wrapClient (API.lookupUsersClientIds (pure usr))) >>= getResult
  clientInfo <- lift . wrapClient $ UnliftIO.Async.pooledMapConcurrentlyN 16 (\c -> getValidity lusr c suiteTag) (toList allClients)
  pure . Set.fromList . map (uncurry ClientInfo) $ clientInfo
  where
    getResult [] = pure mempty
    getResult ((u, cs') : rs)
      | u == usr = pure cs'
      | otherwise = getResult rs

    getValidity lusr cid suiteTag =
      (cid,) . (> 0)
        <$> Data.countKeyPackages lusr cid suiteTag

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
  ( Member GalleyProvider r
  ) =>
  Routes a (Handler r) ()
sitemap = unsafeCallsFed @'Brig @"on-user-deleted-connections" $ do
  Provider.routesInternal

---------------------------------------------------------------------------
-- Handlers

-- | Add a client without authentication checks
addClientInternalH ::
  ( Member GalleyProvider r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  Maybe Bool ->
  NewClient ->
  Maybe ConnId ->
  (Handler r) Client
addClientInternalH usr mSkipReAuth new connId = do
  let policy
        | mSkipReAuth == Just True = \_ _ -> False
        | otherwise = Data.reAuthForNewClients
  API.addClientWithReAuthPolicy policy usr connId new !>> clientError

legalHoldClientRequestedH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  LegalHoldClientRequest ->
  (Handler r) NoContent
legalHoldClientRequestedH targetUser clientRequest = do
  lift $ NoContent <$ API.legalHoldClientRequested targetUser clientRequest

removeLegalHoldClientH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  (Handler r) NoContent
removeLegalHoldClientH uid = do
  lift $ NoContent <$ API.removeLegalHoldClient uid

internalListClientsH :: UserSet -> (Handler r) UserClients
internalListClientsH (UserSet usrs) = lift $ do
  UserClients . Map.fromList
    <$> wrapClient (API.lookupUsersClientIds (Set.toList usrs))

internalListFullClientsH :: UserSet -> (Handler r) UserClientsFull
internalListFullClientsH (UserSet usrs) = lift $ do
  UserClientsFull <$> wrapClient (Data.lookupClientsBulk (Set.toList usrs))

createUserNoVerify ::
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
  ( Member GalleyProvider r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
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

deleteUserNoAuthH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  (Handler r) DeleteUserResponse
deleteUserNoAuthH uid = do
  r <- lift $ API.ensureAccountDeleted uid
  case r of
    NoUser -> throwStd (errorToWai @'E.UserNotFound)
    AccountAlreadyDeleted -> pure UserResponseAccountAlreadyDeleted
    AccountDeleted -> pure UserResponseAccountDeleted

changeSelfEmailMaybeSendH :: (Member BlacklistStore r) => UserId -> EmailUpdate -> Maybe Bool -> (Handler r) ChangeEmailResponse
changeSelfEmailMaybeSendH u body (fromMaybe False -> validate) = do
  let email = euEmail body
  changeSelfEmailMaybeSend u (if validate then ActuallySendEmail else DoNotSendEmail) email API.AllowSCIMUpdates

data MaybeSendEmail = ActuallySendEmail | DoNotSendEmail

changeSelfEmailMaybeSend :: (Member BlacklistStore r) => UserId -> MaybeSendEmail -> Email -> API.AllowSCIMUpdates -> (Handler r) ChangeEmailResponse
changeSelfEmailMaybeSend u ActuallySendEmail email allowScim = do
  API.changeSelfEmail u email allowScim
changeSelfEmailMaybeSend u DoNotSendEmail email allowScim = do
  API.changeEmail u email allowScim !>> changeEmailError >>= \case
    ChangeEmailIdempotent -> pure ChangeEmailResponseIdempotent
    ChangeEmailNeedsActivation _ -> pure ChangeEmailResponseNeedsActivation

-- Historically, this end-point was two end-points with distinct matching routes
-- (distinguished by query params), and it was only allowed to pass one param per call.  This
-- handler allows up to 4 lists of various user keys, and returns the union of the lookups.
-- Empty list is forbidden for backwards compatibility.
listActivatedAccountsH ::
  Maybe (CommaSeparatedList UserId) ->
  Maybe (CommaSeparatedList Handle) ->
  Maybe (CommaSeparatedList Email) ->
  Maybe (CommaSeparatedList Phone) ->
  Maybe Bool ->
  (Handler r) [UserAccount]
listActivatedAccountsH
  (maybe [] fromCommaSeparatedList -> uids)
  (maybe [] fromCommaSeparatedList -> handles)
  (maybe [] fromCommaSeparatedList -> emails)
  (maybe [] fromCommaSeparatedList -> phones)
  (fromMaybe False -> includePendingInvitations) = do
    when (length uids + length handles + length emails + length phones == 0) $ do
      throwStd (notFound "no user keys")
    lift $ do
      u1 <- listActivatedAccounts (Left uids) includePendingInvitations
      u2 <- listActivatedAccounts (Right handles) includePendingInvitations
      u3 <- (\email -> API.lookupAccountsByIdentity (Left email) includePendingInvitations) `mapM` emails
      u4 <- (\phone -> API.lookupAccountsByIdentity (Right phone) includePendingInvitations) `mapM` phones
      pure $ u1 <> u2 <> join u3 <> join u4

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

changeAccountStatusH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  AccountStatusUpdate ->
  (Handler r) NoContent
changeAccountStatusH usr (suStatus -> status) = do
  Log.info $ (Log.msg (Log.val "Change Account Status")) . Log.field "usr" (toByteString usr) . Log.field "status" (show status)
  API.changeSingleAccountStatus usr status !>> accountStatusError -- FUTUREWORK: use CanThrow and related machinery
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

revokeIdentityH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  Maybe Email ->
  Maybe Phone ->
  (Handler r) NoContent
revokeIdentityH (Just email) Nothing = lift $ NoContent <$ API.revokeIdentity (Left email)
revokeIdentityH Nothing (Just phone) = lift $ NoContent <$ API.revokeIdentity (Right phone)
revokeIdentityH bade badp = throwStd (badRequest ("need exactly one of email, phone: " <> Imports.cs (show (bade, badp))))

updateConnectionInternalH ::
  ( Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r
  ) =>
  UpdateConnectionsInternal ->
  (Handler r) NoContent
updateConnectionInternalH updateConn = do
  API.updateConnectionInternal updateConn !>> connError
  pure NoContent

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

updateSSOIdH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  UserSSOId ->
  (Handler r) UpdateSSOIdResponse
updateSSOIdH uid ssoid = do
  success <- lift $ wrapClient $ Data.updateSSOId uid (Just ssoid)
  if success
    then do
      lift $ liftSem $ Intra.onUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOId = Just ssoid}))
      pure UpdateSSOIdSuccess
    else pure UpdateSSOIdNotFound

deleteSSOIdH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  (Handler r) UpdateSSOIdResponse
deleteSSOIdH uid = do
  success <- lift $ wrapClient $ Data.updateSSOId uid Nothing
  if success
    then do
      lift $ liftSem $ Intra.onUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOIdRemoved = True}))
      pure UpdateSSOIdSuccess
    else pure UpdateSSOIdNotFound

updateManagedByH :: UserId -> ManagedByUpdate -> (Handler r) NoContent
updateManagedByH uid (ManagedByUpdate managedBy) = do
  NoContent <$ lift (wrapClient $ Data.updateManagedBy uid managedBy)

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

updateClientLastActive :: UserId -> ClientId -> Handler r ()
updateClientLastActive u c = do
  sysTime <- liftIO getSystemTime
  -- round up to the next multiple of a week
  let week = 604800
  let now =
        systemToUTCTime $
          sysTime
            { systemSeconds = systemSeconds sysTime + (week - systemSeconds sysTime `mod` week),
              systemNanoseconds = 0
            }
  lift . wrapClient $ Data.updateClientLastActive u c now

getRichInfoH :: UserId -> (Handler r) RichInfo
getRichInfoH uid = RichInfo . fromMaybe mempty <$> lift (wrapClient $ API.lookupRichInfo uid)

getRichInfoMultiH :: Maybe (CommaSeparatedList UserId) -> (Handler r) [(UserId, RichInfo)]
getRichInfoMultiH (maybe [] fromCommaSeparatedList -> uids) =
  lift $ wrapClient $ API.lookupRichInfoMultiUsers uids

updateHandleH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member GalleyProvider r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  HandleUpdate ->
  (Handler r) NoContent
updateHandleH uid (HandleUpdate handleUpd) =
  NoContent <$ do
    handle <- validateHandle handleUpd
    API.changeHandle uid Nothing handle API.AllowSCIMUpdates !>> changeHandleError

updateUserNameH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member GalleyProvider r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  NameUpdate ->
  (Handler r) NoContent
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
