{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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
  ( servantSitemap,
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
import Brig.App
import Brig.Data.Activation
import Brig.Data.Client qualified as Data
import Brig.Data.Connection qualified as Data
import Brig.Data.MLS.KeyPackage qualified as Data
import Brig.Data.User qualified as Data
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Options hiding (internalEvents)
import Brig.Provider.API qualified as Provider
import Brig.Team.API qualified as Team
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Brig.Types.User
import Brig.User.EJPD qualified
import Brig.User.Search.Index qualified as Search
import Control.Error hiding (bool)
import Control.Lens (preview, to, _Just)
import Data.ByteString.Conversion (toByteString)
import Data.Code qualified as Code
import Data.CommaSeparatedList
import Data.Default
import Data.Domain (Domain)
import Data.Handle
import Data.HavePendingInvitations
import Data.Id as Id
import Data.Map.Strict qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock.System
import Imports hiding (head)
import Network.Wai.Utilities as Utilities
import Polysemy
import Polysemy.Input (Input, input)
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
import Wire.API.Team.Feature
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client
import Wire.API.User.RichInfo
import Wire.API.UserEvent
import Wire.AuthenticationSubsystem (AuthenticationSubsystem)
import Wire.BlockListStore (BlockListStore)
import Wire.DeleteQueue (DeleteQueue)
import Wire.EmailSubsystem (EmailSubsystem)
import Wire.Events (Events)
import Wire.Events qualified as Events
import Wire.FederationConfigStore
  ( AddFederationRemoteResult (..),
    AddFederationRemoteTeamResult (..),
    FederationConfigStore,
    UpdateFederationResult (..),
  )
import Wire.FederationConfigStore qualified as E
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.InvitationCodeStore
import Wire.NotificationSubsystem
import Wire.PasswordResetCodeStore (PasswordResetCodeStore)
import Wire.PropertySubsystem
import Wire.Rpc
import Wire.Sem.Concurrency
import Wire.TeamInvitationSubsystem
import Wire.UserKeyStore
import Wire.UserStore
import Wire.UserSubsystem
import Wire.UserSubsystem qualified as UserSubsystem
import Wire.VerificationCode
import Wire.VerificationCodeGen
import Wire.VerificationCodeSubsystem

servantSitemap ::
  forall r p.
  ( Member BlockListStore r,
    Member DeleteQueue r,
    Member (Concurrency 'Unsafe) r,
    Member (Embed HttpClientIO) r,
    Member FederationConfigStore r,
    Member AuthenticationSubsystem r,
    Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member UserSubsystem r,
    Member TeamInvitationSubsystem r,
    Member UserStore r,
    Member InvitationCodeStore r,
    Member UserKeyStore r,
    Member Rpc r,
    Member TinyLog r,
    Member (UserPendingActivationStore p) r,
    Member EmailSubsystem r,
    Member VerificationCodeSubsystem r,
    Member Events r,
    Member PasswordResetCodeStore r,
    Member PropertySubsystem r,
    Member (Input (Local ())) r
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
    :<|> Provider.internalProviderAPI

istatusAPI :: forall r. ServerT BrigIRoutes.IStatusAPI (Handler r)
istatusAPI = Named @"get-status" (pure NoContent)

ejpdAPI ::
  ( Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member UserStore r,
    Member Rpc r
  ) =>
  ServerT BrigIRoutes.EJPDRequest (Handler r)
ejpdAPI =
  Brig.User.EJPD.ejpdRequest

mlsAPI :: ServerT BrigIRoutes.MLSAPI (Handler r)
mlsAPI = getMLSClients

accountAPI ::
  ( Member BlockListStore r,
    Member GalleyAPIAccess r,
    Member AuthenticationSubsystem r,
    Member DeleteQueue r,
    Member (UserPendingActivationStore p) r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member UserSubsystem r,
    Member UserKeyStore r,
    Member (Input (Local ())) r,
    Member UserStore r,
    Member TinyLog r,
    Member EmailSubsystem r,
    Member VerificationCodeSubsystem r,
    Member PropertySubsystem r,
    Member Events r,
    Member PasswordResetCodeStore r,
    Member InvitationCodeStore r
  ) =>
  ServerT BrigIRoutes.AccountAPI (Handler r)
accountAPI =
  Named @"get-account-conference-calling-config" getAccountConferenceCallingConfig
    :<|> putAccountConferenceCallingConfig
    :<|> deleteAccountConferenceCallingConfig
    :<|> getConnectionsStatusUnqualified
    :<|> getConnectionsStatus
    :<|> Named @"createUserNoVerify" (callsFed (exposeAnnotations createUserNoVerify))
    :<|> Named @"createUserNoVerifySpar" (callsFed (exposeAnnotations createUserNoVerifySpar))
    :<|> Named @"putSelfEmail" changeSelfEmailMaybeSendH
    :<|> Named @"iDeleteUser" deleteUserNoAuthH
    :<|> Named @"iPutUserStatus" changeAccountStatusH
    :<|> Named @"iGetUserStatus" getAccountStatusH
    :<|> Named @"iGetUsersByVariousKeys" listActivatedAccountsH
    :<|> Named @"iGetUserContacts" getContactListH
    :<|> Named @"iGetUserActivationCode" getActivationCode
    :<|> Named @"iGetUserPasswordResetCode" getPasswordResetCodeH
    :<|> Named @"iRevokeIdentity" revokeIdentityH
    :<|> Named @"iHeadBlacklist" checkBlacklist
    :<|> Named @"iDeleteBlacklist" deleteFromBlacklist
    :<|> Named @"iPostBlacklist" addBlacklist
    :<|> Named @"iPutUserSsoId" updateSSOIdH
    :<|> Named @"iDeleteUserSsoId" deleteSSOIdH
    :<|> Named @"iPutManagedBy" updateManagedByH
    :<|> Named @"iPutRichInfo" updateRichInfoH
    :<|> Named @"iPutHandle" updateHandleH
    :<|> Named @"iPutUserName" updateUserNameH
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
  ( Member GalleyAPIAccess r,
    Member (UserPendingActivationStore p) r,
    Member BlockListStore r,
    Member (Embed HttpClientIO) r,
    Member UserKeyStore r,
    Member (Concurrency 'Unsafe) r,
    Member TinyLog r,
    Member InvitationCodeStore r,
    Member TeamInvitationSubsystem r,
    Member UserSubsystem r,
    Member Events r,
    Member (Input (Local ())) r
  ) =>
  ServerT BrigIRoutes.TeamsAPI (Handler r)
teamsAPI =
  Named @"updateSearchVisibilityInbound" (lift . liftSem . updateTeamSearchVisibilityInbound)
    :<|> Named @"get-invitation-by-email" (lift . liftSem . getInvitationByEmail)
    :<|> Named @"get-invitation-code" Team.getInvitationCode
    :<|> Named @"suspend-team" Team.suspendTeam
    :<|> Named @"unsuspend-team" Team.unsuspendTeam
    :<|> Named @"team-size" Team.teamSize
    :<|> Named @"create-invitations-via-scim" Team.createInvitationViaScim

userAPI :: (Member UserSubsystem r) => ServerT BrigIRoutes.UserAPI (Handler r)
userAPI =
  updateLocale
    :<|> deleteLocale
    :<|> getDefaultUserLocale

clientAPI :: ServerT BrigIRoutes.ClientAPI (Handler r)
clientAPI = Named @"update-client-last-active" updateClientLastActive

authAPI ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member Events r,
    Member UserSubsystem r,
    Member VerificationCodeSubsystem r
  ) =>
  ServerT BrigIRoutes.AuthAPI (Handler r)
authAPI =
  Named @"legalhold-login" (callsFed (exposeAnnotations legalHoldLogin))
    :<|> Named @"sso-login" (callsFed (exposeAnnotations ssoLogin))
    :<|> Named @"login-code" getLoginCode
    :<|> Named @"reauthenticate"
      ( \uid reauth ->
          -- changing this end-point would involve providing a `Local` type from a user id that is
          -- captured from the path, not pulled from the http header.  this is certainly feasible,
          -- but running qualifyLocal here is easier.
          qualifyLocal uid >>= \luid -> reauthenticate luid reauth
      )

federationRemotesAPI ::
  ( Member FederationConfigStore r
  ) =>
  ServerT BrigIRoutes.FederationRemotesAPI (Handler r)
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

addFederationRemoteTeam ::
  ( Member FederationConfigStore r
  ) =>
  Domain ->
  FederationRemoteTeam ->
  (Handler r) ()
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

addFederationRemote ::
  ( Member FederationConfigStore r
  ) =>
  FederationDomainConfig ->
  (Handler r) ()
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
                 <> T.pack (show fedDomConf)
                 <> "or Nothing, "
             )
          <> ( "got "
                 <> T.pack (show (Map.lookup (domain fedDomConf) cfg))
             )

updateFederationRemote :: (Member FederationConfigStore r) => Domain -> FederationDomainConfig -> (Handler r) ()
updateFederationRemote dom fedcfg = do
  if (dom /= fedcfg.domain)
    then
      throwError . fedError . FederationUnexpectedError . T.pack $
        "federation domain of a given peer cannot be changed from " <> show (domain fedcfg) <> " to " <> show dom <> "."
    else
      lift (liftSem (E.updateFederationConfig fedcfg)) >>= \case
        UpdateFederationSuccess -> pure ()
        UpdateFederationRemoteNotFound ->
          throwError . fedError . FederationUnexpectedError . T.pack $
            "federation domain does not exist and cannot be updated: " <> show (dom, fedcfg)
        UpdateFederationRemoteDivergingConfig ->
          throwError . fedError . FederationUnexpectedError $
            "keeping track of remote domains in the brig config file is deprecated, but as long as we \
            \do that, removing or updating items listed in the config file is not allowed."

getAccountConferenceCallingConfig :: UserId -> Handler r (Feature ConferenceCallingConfig)
getAccountConferenceCallingConfig uid = do
  mStatus <- lift $ wrapClient $ Data.lookupFeatureConferenceCalling uid
  mDefStatus <- preview (settingsLens . featureFlagsLens . _Just . to conferenceCalling . to forNull)
  pure $ def {status = mStatus <|> mDefStatus ?: (def :: LockableFeature ConferenceCallingConfig).status}

putAccountConferenceCallingConfig :: UserId -> Feature ConferenceCallingConfig -> Handler r NoContent
putAccountConferenceCallingConfig uid feat = do
  lift $ wrapClient $ Data.updateFeatureConferenceCalling uid (Just feat.status) $> NoContent

deleteAccountConferenceCallingConfig :: UserId -> Handler r NoContent
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

getVerificationCode :: forall r. (Member VerificationCodeSubsystem r) => UserId -> VerificationAction -> Handler r (Maybe Code.Value)
getVerificationCode uid action = runMaybeT do
  user <- MaybeT . wrapClientE $ API.lookupUser NoPendingInvitations uid
  email <- MaybeT . pure $ userEmail user
  let key = mkKey email
  code <- MaybeT . lift . liftSem $ internalLookupCode key (scopeFromAction action)
  pure code.codeValue

internalSearchIndexAPI :: forall r. ServerT BrigIRoutes.ISearchIndexAPI (Handler r)
internalSearchIndexAPI =
  Named @"indexRefresh" (NoContent <$ lift (wrapClient Search.refreshIndex))

---------------------------------------------------------------------------
-- Handlers

-- | Add a client without authentication checks
addClientInternalH ::
  ( Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member DeleteQueue r,
    Member EmailSubsystem r,
    Member Events r,
    Member UserSubsystem r,
    Member VerificationCodeSubsystem r
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
  lusr <- qualifyLocal usr
  API.addClientWithReAuthPolicy policy lusr connId new !>> clientError

legalHoldClientRequestedH :: (Member Events r) => UserId -> LegalHoldClientRequest -> (Handler r) NoContent
legalHoldClientRequestedH targetUser clientRequest = do
  lift $ NoContent <$ API.legalHoldClientRequested targetUser clientRequest

removeLegalHoldClientH :: (Member DeleteQueue r, Member Events r) => UserId -> (Handler r) NoContent
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
  ( Member BlockListStore r,
    Member GalleyAPIAccess r,
    Member (UserPendingActivationStore p) r,
    Member TinyLog r,
    Member Events r,
    Member InvitationCodeStore r,
    Member UserKeyStore r,
    Member UserSubsystem r,
    Member (Input (Local ())) r,
    Member PasswordResetCodeStore r
  ) =>
  NewUser ->
  (Handler r) (Either RegisterError SelfProfile)
createUserNoVerify uData = lift . runExceptT $ do
  result <- API.createUser uData
  let acc = createdAccount result
  let usr = accountUser acc
  let uid = userId usr
  let eac = createdEmailActivation result
  for_ eac $ \adata ->
    let key = ActivateKey $ activationKey adata
        code = activationCode adata
     in API.activate key code (Just uid) !>> activationErrorToRegisterError
  pure . SelfProfile $ usr

createUserNoVerifySpar ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member Events r,
    Member PasswordResetCodeStore r
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
    for_ eac $ \adata ->
      let key = ActivateKey $ activationKey adata
          code = activationCode adata
       in API.activate key code (Just uid) !>> CreateUserSparRegistrationError . activationErrorToRegisterError
    pure . SelfProfile $ usr

deleteUserNoAuthH ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member UserStore r,
    Member TinyLog r,
    Member UserKeyStore r,
    Member Events r,
    Member UserSubsystem r,
    Member PropertySubsystem r
  ) =>
  UserId ->
  (Handler r) DeleteUserResponse
deleteUserNoAuthH uid = do
  luid <- qualifyLocal uid
  r <- lift $ API.ensureAccountDeleted luid
  case r of
    NoUser -> throwStd (errorToWai @'E.UserNotFound)
    AccountAlreadyDeleted -> pure UserResponseAccountAlreadyDeleted
    AccountDeleted -> pure UserResponseAccountDeleted

changeSelfEmailMaybeSendH ::
  ( Member BlockListStore r,
    Member UserKeyStore r,
    Member EmailSubsystem r,
    Member UserSubsystem r
  ) =>
  UserId ->
  EmailUpdate ->
  Maybe Bool ->
  (Handler r) ChangeEmailResponse
changeSelfEmailMaybeSendH u body (fromMaybe False -> validate) = do
  let email = euEmail body
  changeSelfEmailMaybeSend u (if validate then ActuallySendEmail else DoNotSendEmail) email UpdateOriginScim

data MaybeSendEmail = ActuallySendEmail | DoNotSendEmail

changeSelfEmailMaybeSend ::
  ( Member BlockListStore r,
    Member UserKeyStore r,
    Member EmailSubsystem r,
    Member UserSubsystem r
  ) =>
  UserId ->
  MaybeSendEmail ->
  EmailAddress ->
  UpdateOriginType ->
  (Handler r) ChangeEmailResponse
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
  ( Member (Input (Local ())) r,
    Member UserSubsystem r
  ) =>
  Maybe (CommaSeparatedList UserId) ->
  Maybe (CommaSeparatedList Handle) ->
  Maybe (CommaSeparatedList EmailAddress) ->
  Maybe Bool ->
  Handler r [ExtendedUserAccount]
listActivatedAccountsH
  (maybe [] fromCommaSeparatedList -> uids)
  (maybe [] fromCommaSeparatedList -> handles)
  (maybe [] fromCommaSeparatedList -> emails)
  (maybe NoPendingInvitations fromBool -> include) = do
    when (length uids + length handles + length emails == 0) $ do
      throwStd (notFound "no user keys")
    lift $ liftSem do
      loc <- input
      byEmails <- getExtendedAccountsByEmailNoFilter $ loc $> emails
      others <-
        getExtendedAccountsBy $
          loc
            $> def
              { includePendingInvitations = include,
                getByUserId = uids,
                getByHandle = handles
              }
      pure $ others <> byEmails

getActivationCode :: EmailAddress -> Handler r GetActivationCodeResp
getActivationCode email = do
  apair <- lift . wrapClient $ API.lookupActivationCode email
  maybe (throwStd activationKeyNotFound) (pure . GetActivationCodeResp) apair

getPasswordResetCodeH ::
  ( Member AuthenticationSubsystem r
  ) =>
  EmailAddress ->
  Handler r GetPasswordResetCodeResp
getPasswordResetCodeH email = getPasswordResetCode email

getPasswordResetCode ::
  ( Member AuthenticationSubsystem r
  ) =>
  EmailAddress ->
  Handler r GetPasswordResetCodeResp
getPasswordResetCode email =
  (GetPasswordResetCodeResp <$$> lift (API.lookupPasswordResetCode email))
    >>= maybe (throwStd (errorToWai @'E.InvalidPasswordResetKey)) pure

changeAccountStatusH ::
  ( Member UserSubsystem r,
    Member Events r
  ) =>
  UserId ->
  AccountStatusUpdate ->
  (Handler r) NoContent
changeAccountStatusH usr (suStatus -> status) = do
  Log.info $ (Log.msg (Log.val "Change Account Status")) . Log.field "usr" (toByteString usr) . Log.field "status" (show status)
  API.changeSingleAccountStatus usr status !>> accountStatusError -- FUTUREWORK: use CanThrow and related machinery
  pure NoContent

getAccountStatusH :: (Member UserStore r) => UserId -> (Handler r) AccountStatusResp
getAccountStatusH uid = do
  status <- lift $ liftSem $ lookupStatus uid
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
  ( Member UserSubsystem r,
    Member UserKeyStore r
  ) =>
  EmailAddress ->
  Handler r NoContent
revokeIdentityH email = lift $ NoContent <$ API.revokeIdentity email

updateConnectionInternalH ::
  ( Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r
  ) =>
  UpdateConnectionsInternal ->
  (Handler r) NoContent
updateConnectionInternalH updateConn = do
  API.updateConnectionInternal updateConn !>> connError
  pure NoContent

checkBlacklist :: (Member BlockListStore r) => EmailAddress -> Handler r CheckBlacklistResponse
checkBlacklist email = lift $ bool NotBlacklisted YesBlacklisted <$> API.isBlacklisted email

deleteFromBlacklist :: (Member BlockListStore r) => EmailAddress -> Handler r NoContent
deleteFromBlacklist email = lift $ NoContent <$ API.blacklistDelete email

addBlacklist :: (Member BlockListStore r) => EmailAddress -> Handler r NoContent
addBlacklist email = lift $ NoContent <$ API.blacklistInsert email

updateSSOIdH ::
  ( Member UserSubsystem r,
    Member Events r
  ) =>
  UserId ->
  UserSSOId ->
  (Handler r) UpdateSSOIdResponse
updateSSOIdH uid ssoid = lift $ do
  success <- wrapClient $ Data.updateSSOId uid (Just ssoid)
  liftSem $
    if success
      then do
        UserSubsystem.internalUpdateSearchIndex uid
        Events.generateUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOId = Just ssoid}))
        pure UpdateSSOIdSuccess
      else pure UpdateSSOIdNotFound

deleteSSOIdH ::
  ( Member UserSubsystem r,
    Member Events r
  ) =>
  UserId ->
  (Handler r) UpdateSSOIdResponse
deleteSSOIdH uid = lift $ do
  success <- wrapClient $ Data.updateSSOId uid Nothing
  if success
    then liftSem $ do
      UserSubsystem.internalUpdateSearchIndex uid
      Events.generateUserEvent uid Nothing (UserUpdated ((emptyUserUpdatedData uid) {eupSSOIdRemoved = True}))
      pure UpdateSSOIdSuccess
    else pure UpdateSSOIdNotFound

updateManagedByH :: UserId -> ManagedByUpdate -> (Handler r) NoContent
updateManagedByH uid (ManagedByUpdate managedBy) = do
  NoContent <$ lift (wrapClient $ Data.updateManagedBy uid managedBy)

updateRichInfoH :: UserId -> RichInfoUpdate -> (Handler r) NoContent
updateRichInfoH uid rup =
  NoContent <$ do
    let (unRichInfoAssocList -> richInfo) = normalizeRichInfoAssocList . riuRichInfo $ rup
    maxSize <- asks (.settings.richInfoLimit)
    when (richInfoSize (RichInfo (mkRichInfoAssocList richInfo)) > maxSize) $ throwStd tooLargeRichInfo
    -- FUTUREWORK: send an event
    -- Intra.onUserEvent uid (Just conn) (richInfoUpdate uid ri)
    lift $ wrapClient $ Data.updateRichInfo uid (mkRichInfoAssocList richInfo)

updateLocale :: (Member UserSubsystem r) => UserId -> LocaleUpdate -> (Handler r) LocaleUpdate
updateLocale uid upd@(LocaleUpdate locale) = do
  qUid <- qualifyLocal uid
  lift . liftSem $ updateUserProfile qUid Nothing UpdateOriginScim def {locale = Just locale}
  pure upd

deleteLocale :: (Member UserSubsystem r) => UserId -> (Handler r) NoContent
deleteLocale uid = do
  defLoc <- setDefaultUserLocale <$> asks (.settings)
  qUid <- qualifyLocal uid
  lift . liftSem $ updateUserProfile qUid Nothing UpdateOriginScim def {locale = Just defLoc}
  pure NoContent

getDefaultUserLocale :: (Handler r) LocaleUpdate
getDefaultUserLocale = do
  defLocale <- setDefaultUserLocale <$> asks (.settings)
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
  (Member UserSubsystem r) =>
  UserId ->
  HandleUpdate ->
  Handler r NoContent
updateHandleH uid (HandleUpdate handleUpd) =
  NoContent <$ do
    quid <- qualifyLocal uid
    lift . liftSem $ UserSubsystem.updateHandle quid Nothing UpdateOriginScim handleUpd

updateUserNameH ::
  (Member UserSubsystem r) =>
  UserId ->
  NameUpdate ->
  (Handler r) NoContent
updateUserNameH uid (NameUpdate nameUpd) =
  NoContent <$ do
    luid <- qualifyLocal uid
    name <- either (const $ throwStd (errorToWai @'E.InvalidUser)) pure $ mkName nameUpd
    lift (wrapClient $ Data.lookupUser WithPendingInvitations uid) >>= \case
      Just _ -> lift . liftSem $ updateUserProfile luid Nothing UpdateOriginScim (def {name = Just name})
      Nothing -> throwStd (errorToWai @'E.InvalidUser)

checkHandleInternalH :: (Member UserSubsystem r) => Handle -> Handler r CheckHandleResponse
checkHandleInternalH h = lift $ liftSem do
  API.checkHandle (fromHandle h) <&> \case
    API.CheckHandleFound -> CheckHandleResponseFound
    API.CheckHandleNotFound -> CheckHandleResponseNotFound

getContactListH :: UserId -> (Handler r) UserIds
getContactListH uid = lift . wrapClient $ UserIds <$> API.lookupContactList uid
