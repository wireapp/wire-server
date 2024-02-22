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

module Galley.API.Internal
  ( waiInternalSitemap,
    internalAPI,
    InternalAPI,
    deleteLoop,
    safeForever,
  )
where

import Control.Exception.Safe (catchAny)
import Control.Lens hiding (Getter, Setter, (.=))
import Data.Id as Id
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Map qualified as Map
import Data.Qualified
import Data.Range
import Data.Singletons
import Data.Time
import Galley.API.Action
import Galley.API.Clients qualified as Clients
import Galley.API.Create qualified as Create
import Galley.API.CustomBackend qualified as CustomBackend
import Galley.API.Error
import Galley.API.LegalHold (unsetTeamLegalholdWhitelistedH)
import Galley.API.LegalHold.Conflicts
import Galley.API.MLS.Removal
import Galley.API.One2One
import Galley.API.Public
import Galley.API.Public.Servant
import Galley.API.Query qualified as Query
import Galley.API.Teams (uncheckedDeleteTeamMember)
import Galley.API.Teams qualified as Teams
import Galley.API.Teams.Features
import Galley.API.Update qualified as Update
import Galley.API.Util
import Galley.App
import Galley.Data.Conversation qualified as Data
import Galley.Effects
import Galley.Effects.BackendNotificationQueueAccess
import Galley.Effects.ClientStore
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.LegalHoldStore as LegalHoldStore
import Galley.Effects.MemberStore qualified as E
import Galley.Effects.TeamStore
import Galley.Effects.TeamStore qualified as E
import Galley.Monad
import Galley.Options hiding (brig)
import Galley.Queue qualified as Q
import Galley.Types.Bot (AddBot, RemoveBot)
import Galley.Types.Bot.Service
import Galley.Types.Conversations.Members (RemoteMember (rmId))
import Galley.Types.UserList
import Gundeck.Types.Push.V2 qualified as PushV2
import Imports hiding (head)
import Network.AMQP qualified as Q
import Network.Wai.Predicate hiding (Error, err, result, setStatus)
import Network.Wai.Routing hiding (App, route, toList)
import Network.Wai.Utilities hiding (Error)
import Network.Wai.Utilities.ZAuth
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import Servant hiding (JSON, WithStatus)
import System.Logger.Class hiding (Path, name)
import System.Logger.Class qualified as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.CustomBackend
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Provider.Service hiding (Service)
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Galley
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Routes.MultiTablePaging (mtpHasMore, mtpPagingState, mtpResults)
import Wire.API.Team.Feature hiding (setStatus)
import Wire.API.User.Client
import Wire.NotificationSubsystem
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra

internalAPI :: API InternalAPI GalleyEffects
internalAPI =
  hoistAPI @InternalAPIBase Imports.id $
    mkNamedAPI @"status" (pure ())
      <@> mkNamedAPI @"delete-user" (callsFed (exposeAnnotations rmUser))
      <@> mkNamedAPI @"connect" (callsFed (exposeAnnotations Create.createConnectConversation))
      <@> mkNamedAPI @"get-conversation-clients" iGetMLSClientListForConv
      <@> mkNamedAPI @"guard-legalhold-policy-conflicts" guardLegalholdPolicyConflictsH
      <@> legalholdWhitelistedTeamsAPI
      <@> iTeamsAPI
      <@> mkNamedAPI @"upsert-one2one" iUpsertOne2OneConversation
      <@> featureAPI
      <@> federationAPI
      <@> conversationAPI

federationAPI :: API IFederationAPI GalleyEffects
federationAPI =
  mkNamedAPI @"get-federation-status" (const getFederationStatus)

conversationAPI :: API IConversationAPI GalleyEffects
conversationAPI =
  mkNamedAPI @"conversation-get-member" Query.internalGetMember
    <@> mkNamedAPI @"conversation-accept-v2" Update.acceptConv
    <@> mkNamedAPI @"conversation-block-unqualified" Update.blockConvUnqualified
    <@> mkNamedAPI @"conversation-block" Update.blockConv
    <@> mkNamedAPI @"conversation-unblock" Update.unblockConv
    <@> mkNamedAPI @"conversation-meta" Query.getConversationMeta
    <@> mkNamedAPI @"conversation-mls-one-to-one" Query.getMLSOne2OneConversation

legalholdWhitelistedTeamsAPI :: API ILegalholdWhitelistedTeamsAPI GalleyEffects
legalholdWhitelistedTeamsAPI = mkAPI $ \tid -> hoistAPIHandler Imports.id (base tid)
  where
    base :: TeamId -> API ILegalholdWhitelistedTeamsAPIBase GalleyEffects
    base tid =
      mkNamedAPI @"set-team-legalhold-whitelisted" (LegalHoldStore.setTeamLegalholdWhitelisted tid)
        <@> mkNamedAPI @"unset-team-legalhold-whitelisted" (unsetTeamLegalholdWhitelistedH tid)
        <@> mkNamedAPI @"get-team-legalhold-whitelisted" (LegalHoldStore.isTeamLegalholdWhitelisted tid)

iTeamsAPI :: API ITeamsAPI GalleyEffects
iTeamsAPI = mkAPI $ \tid -> hoistAPIHandler Imports.id (base tid)
  where
    hoistAPISegment ::
      (ServerT (seg :> inner) (Sem r) ~ ServerT inner (Sem r)) =>
      API inner r ->
      API (seg :> inner) r
    hoistAPISegment = hoistAPI Imports.id

    base :: TeamId -> API ITeamsAPIBase GalleyEffects
    base tid =
      mkNamedAPI @"get-team-internal" (Teams.getTeamInternalH tid)
        <@> mkNamedAPI @"create-binding-team" (Teams.createBindingTeam tid)
        <@> mkNamedAPI @"delete-binding-team" (Teams.internalDeleteBindingTeam tid)
        <@> mkNamedAPI @"get-team-name" (Teams.getTeamNameInternalH tid)
        <@> mkNamedAPI @"update-team-status" (Teams.updateTeamStatus tid)
        <@> hoistAPISegment
          ( mkNamedAPI @"unchecked-add-team-member" (Teams.uncheckedAddTeamMember tid)
              <@> mkNamedAPI @"unchecked-get-team-members" (Teams.uncheckedGetTeamMembersH tid)
              <@> mkNamedAPI @"unchecked-get-team-member" (Teams.uncheckedGetTeamMember tid)
              <@> mkNamedAPI @"can-user-join-team" (Teams.canUserJoinTeam tid)
              <@> mkNamedAPI @"unchecked-update-team-member" (Teams.uncheckedUpdateTeamMember Nothing Nothing tid)
          )
        <@> mkNamedAPI @"user-is-team-owner" (Teams.userIsTeamOwner tid)
        <@> hoistAPISegment
          ( mkNamedAPI @"get-search-visibility-internal" (Teams.getSearchVisibilityInternal tid)
              <@> mkNamedAPI @"set-search-visibility-internal" (Teams.setSearchVisibilityInternal (featureEnabledForTeam @SearchVisibilityAvailableConfig) tid)
          )

featureAPI :: API IFeatureAPI GalleyEffects
featureAPI =
  mkNamedAPI @'("iget", SSOConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", SSOConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", SSOConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", LegalholdConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", LegalholdConfig) (callsFed (exposeAnnotations setFeatureStatusInternal))
    <@> mkNamedAPI @'("ipatch", LegalholdConfig) (callsFed (exposeAnnotations patchFeatureStatusInternal))
    <@> mkNamedAPI @'("iget", SearchVisibilityAvailableConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", SearchVisibilityAvailableConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", SearchVisibilityAvailableConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", ValidateSAMLEmailsConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", ValidateSAMLEmailsConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", ValidateSAMLEmailsConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", DigitalSignaturesConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", DigitalSignaturesConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", DigitalSignaturesConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", AppLockConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", AppLockConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", AppLockConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", FileSharingConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", FileSharingConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", FileSharingConfig) (updateLockStatus @FileSharingConfig)
    <@> mkNamedAPI @'("ipatch", FileSharingConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", ConferenceCallingConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", ConferenceCallingConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", ConferenceCallingConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", SelfDeletingMessagesConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", SelfDeletingMessagesConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", SelfDeletingMessagesConfig) (updateLockStatus @SelfDeletingMessagesConfig)
    <@> mkNamedAPI @'("ipatch", SelfDeletingMessagesConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", GuestLinksConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", GuestLinksConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", GuestLinksConfig) (updateLockStatus @GuestLinksConfig)
    <@> mkNamedAPI @'("ipatch", GuestLinksConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", SndFactorPasswordChallengeConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", SndFactorPasswordChallengeConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", SndFactorPasswordChallengeConfig) (updateLockStatus @SndFactorPasswordChallengeConfig)
    <@> mkNamedAPI @'("ipatch", SndFactorPasswordChallengeConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", SearchVisibilityInboundConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", SearchVisibilityInboundConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", SearchVisibilityInboundConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("igetmulti", SearchVisibilityInboundConfig) getFeatureStatusMulti
    <@> mkNamedAPI @'("iget", ClassifiedDomainsConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iget", MLSConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", MLSConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", MLSConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", MLSConfig) (updateLockStatus @MLSConfig)
    <@> mkNamedAPI @'("iget", ExposeInvitationURLsToTeamAdminConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", ExposeInvitationURLsToTeamAdminConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", ExposeInvitationURLsToTeamAdminConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", SearchVisibilityInboundConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", SearchVisibilityInboundConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", SearchVisibilityInboundConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("iget", OutlookCalIntegrationConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", OutlookCalIntegrationConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", OutlookCalIntegrationConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", OutlookCalIntegrationConfig) (updateLockStatus @OutlookCalIntegrationConfig)
    <@> mkNamedAPI @'("iget", MlsE2EIdConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", MlsE2EIdConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", MlsE2EIdConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", MlsE2EIdConfig) (updateLockStatus @MlsE2EIdConfig)
    <@> mkNamedAPI @'("iget", MlsMigrationConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", MlsMigrationConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", MlsMigrationConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", MlsMigrationConfig) (updateLockStatus @MlsMigrationConfig)
    <@> mkNamedAPI @'("iget", EnforceFileDownloadLocationConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", EnforceFileDownloadLocationConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", EnforceFileDownloadLocationConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @'("ilock", EnforceFileDownloadLocationConfig) (updateLockStatus @EnforceFileDownloadLocationConfig)
    <@> mkNamedAPI @'("iget", LimitedEventFanoutConfig) (getFeatureStatus DontDoAuth)
    <@> mkNamedAPI @'("iput", LimitedEventFanoutConfig) setFeatureStatusInternal
    <@> mkNamedAPI @'("ipatch", LimitedEventFanoutConfig) patchFeatureStatusInternal
    <@> mkNamedAPI @"feature-configs-internal" (maybe getAllFeatureConfigsForServer getAllFeatureConfigsForUser)

waiInternalSitemap :: Routes a (Sem GalleyEffects) ()
waiInternalSitemap = unsafeCallsFed @'Galley @"on-client-removed" $ unsafeCallsFed @'Galley @"on-mls-message-sent" $ do
  -- Misc API (internal) ------------------------------------------------

  get "/i/users/:uid/team/members" (continueE Teams.getBindingTeamMembersH) $
    capture "uid"

  get "/i/users/:uid/team" (continueE Teams.getBindingTeamIdH) $
    capture "uid"

  get "/i/test/clients" (continueE Clients.getClientsH) $
    zauthUserId
  -- eg. https://github.com/wireapp/wire-server/blob/3bdca5fc8154e324773802a0deb46d884bd09143/services/brig/test/integration/API/User/Client.hs#L319

  post "/i/clients/:client" (continue Clients.addClientH) $
    zauthUserId
      .&. capture "client"

  delete "/i/clients/:client" (continue Clients.rmClientH) $
    zauthUserId
      .&. capture "client"

  post "/i/services" (continue Update.addServiceH) $
    jsonRequest @Service

  delete "/i/services" (continue Update.rmServiceH) $
    jsonRequest @ServiceRef

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to members
  post "/i/bots" (continueE Update.addBotH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @AddBot

  -- This endpoint can lead to the following events being sent:
  -- - MemberLeave event to members
  delete "/i/bots" (continueE Update.rmBotH) $
    zauthUserId
      .&. opt zauthConnId
      .&. jsonRequest @RemoveBot

  put "/i/custom-backend/by-domain/:domain" (continue CustomBackend.internalPutCustomBackendByDomainH) $
    capture "domain"
      .&. jsonRequest @CustomBackend

  delete "/i/custom-backend/by-domain/:domain" (continue CustomBackend.internalDeleteCustomBackendByDomainH) $
    capture "domain"
      .&. accept "application" "json"

rmUser ::
  forall p1 p2 r.
  ( p1 ~ CassandraPaging,
    p2 ~ InternalPaging,
    Member BackendNotificationQueueAccess r,
    Member ClientStore r,
    Member ConversationStore r,
    Member (Error DynError) r,
    Member (Error InternalError) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member (ListItems p1 ConvId) r,
    Member (ListItems p1 (Remote ConvId)) r,
    Member (ListItems p2 TeamId) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member P.TinyLog r,
    Member SubConversationStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Sem r ()
rmUser lusr conn = do
  let nRange1000 = toRange (Proxy @1000) :: Range 1 1000 Int32
  tids <- listTeams (tUnqualified lusr) Nothing maxBound
  leaveTeams tids
  allConvIds <- Query.conversationIdsPageFrom lusr (GetPaginatedConversationIds Nothing nRange1000)
  goConvPages nRange1000 allConvIds

  deleteClients (tUnqualified lusr)
  where
    goConvPages :: Range 1 1000 Int32 -> ConvIdsPage -> Sem r ()
    goConvPages range page = do
      let (localConvs, remoteConvs) = partitionQualified lusr (mtpResults page)
      leaveLocalConversations localConvs
      traverse_ leaveRemoteConversations (rangedChunks remoteConvs)
      when (mtpHasMore page) $ do
        let nextState = mtpPagingState page
            nextQuery = GetPaginatedConversationIds (Just nextState) range
        newCids <- Query.conversationIdsPageFrom lusr nextQuery
        goConvPages range newCids

    leaveTeams page = for_ (pageItems page) $ \tid -> do
      toNotify <-
        handleImpossibleErrors $
          getFeatureStatus @LimitedEventFanoutConfig DontDoAuth tid
            >>= ( \case
                    FeatureStatusEnabled -> Left <$> E.getTeamAdmins tid
                    FeatureStatusDisabled -> Right <$> getTeamMembersForFanout tid
                )
              . wsStatus
      uncheckedDeleteTeamMember lusr conn tid (tUnqualified lusr) toNotify
      page' <- listTeams @p2 (tUnqualified lusr) (Just (pageState page)) maxBound
      leaveTeams page'

    -- The @'NotATeamMember@ and @'TeamNotFound@ errors cannot happen at this
    -- point: the user is a team member because we fetched the list of teams
    -- they are member of, and conversely the list of teams was fetched exactly
    -- for this user so it cannot be that the team is not found. Therefore, this
    -- helper just drops the errors.
    handleImpossibleErrors ::
      Sem
        ( ErrorS 'NotATeamMember
            ': ErrorS 'TeamNotFound
            ': r
        )
        a ->
      Sem r a
    handleImpossibleErrors action =
      mapToDynamicError @'TeamNotFound (mapToDynamicError @'NotATeamMember action)

    leaveLocalConversations :: [ConvId] -> Sem r ()
    leaveLocalConversations ids = do
      let qUser = tUntagged lusr
      cc <- getConversations ids
      now <- input
      pp <- for cc $ \c -> case Data.convType c of
        SelfConv -> pure Nothing
        One2OneConv -> E.deleteMembers (Data.convId c) (UserList [tUnqualified lusr] []) $> Nothing
        ConnectConv -> E.deleteMembers (Data.convId c) (UserList [tUnqualified lusr] []) $> Nothing
        RegularConv
          | tUnqualified lusr `isMember` Data.convLocalMembers c -> do
              runError (removeUser (qualifyAs lusr c) RemoveUserIncludeMain (tUntagged lusr)) >>= \case
                Left e -> P.err $ Log.msg ("failed to send remove proposal: " <> internalErrorDescription e)
                Right _ -> pure ()
              E.deleteMembers (Data.convId c) (UserList [tUnqualified lusr] [])
              let e =
                    Event
                      (tUntagged (qualifyAs lusr (Data.convId c)))
                      Nothing
                      (tUntagged lusr)
                      now
                      (EdMembersLeave EdReasonDeleted (QualifiedUserIdList [qUser]))
              for_ (bucketRemote (fmap rmId (Data.convRemoteMembers c))) $ notifyRemoteMembers now qUser (Data.convId c)
              pure $
                newPushLocal (tUnqualified lusr) (toJSONObject e) (localMemberToRecipient <$> Data.convLocalMembers c)
                  <&> set pushConn conn
                    . set pushRoute PushV2.RouteDirect
          | otherwise -> pure Nothing

      pushNotifications (catMaybes pp)

    -- FUTUREWORK: This could be optimized to reduce the number of RPCs
    -- made. When a team is deleted the burst of RPCs created here could
    -- lead to performance issues. We should cover this in a performance
    -- test.
    notifyRemoteMembers :: UTCTime -> Qualified UserId -> ConvId -> Remote [UserId] -> Sem r ()
    notifyRemoteMembers now qUser cid remotes = do
      let convUpdate =
            ConversationUpdate
              { cuTime = now,
                cuOrigUserId = qUser,
                cuConvId = cid,
                cuAlreadyPresentUsers = tUnqualified remotes,
                cuAction = SomeConversationAction (sing @'ConversationLeaveTag) ()
              }
      let rpc = fedClient @'Galley @"on-conversation-updated" convUpdate
      runFederatedEither remotes rpc
        >>= logAndIgnoreError "Error in onConversationUpdated call" (qUnqualified qUser)

    leaveRemoteConversations :: Range 1 UserDeletedNotificationMaxConvs [Remote ConvId] -> Sem r ()
    leaveRemoteConversations cids =
      for_ (bucketRemote (fromRange cids)) $ \remoteConvs -> do
        let userDelete = UserDeletedConversationsNotification (tUnqualified lusr) (unsafeRange (tUnqualified remoteConvs))
        let rpc = void $ fedQueueClient @'OnUserDeletedConversationsTag userDelete
        enqueueNotification remoteConvs Q.Persistent rpc

    -- FUTUREWORK: Add a retry mechanism if there are federation errrors.
    -- See https://wearezeta.atlassian.net/browse/SQCORE-1091
    logAndIgnoreError :: Text -> UserId -> Either FederationError a -> Sem r ()
    logAndIgnoreError message usr res = do
      case res of
        Left federationError ->
          P.err
            ( Log.msg
                ( "Federation error while notifying remote backends of a user deletion (Galley). "
                    <> message
                    <> " "
                    <> (cs . show $ federationError)
                )
                . Log.field "user" (show usr)
            )
        Right _ -> pure ()

deleteLoop :: App ()
deleteLoop = do
  q <- view deleteQueue
  safeForever "deleteLoop" $ do
    i@(TeamItem tid usr con) <- Q.pop q
    env <- ask
    liftIO (evalGalleyToIO env (doDelete usr con tid))
      `catchAny` someError q i
  where
    someError q i x = do
      err $ "error" .= show x ~~ msg (val "failed to delete")
      ok <- Q.tryPush q i
      unless ok $
        err (msg (val "delete queue is full, dropping item") ~~ "item" .= show i)
      liftIO $ threadDelay 1000000

    doDelete usr con tid = do
      lusr <- qualifyLocal usr
      Teams.uncheckedDeleteTeam lusr con tid

safeForever :: String -> App () -> App ()
safeForever funName action =
  forever $
    action `catchAny` \exc -> do
      err $ "error" .= show exc ~~ msg (val $ cs funName <> " failed")
      threadDelay 60000000 -- pause to keep worst-case noise in logs manageable

guardLegalholdPolicyConflictsH ::
  ( Member BrigAccess r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'MissingLegalholdConsentOldClients) r
  ) =>
  GuardLegalholdPolicyConflicts ->
  Sem r ()
guardLegalholdPolicyConflictsH glh = do
  mapError @LegalholdConflicts (const $ Tagged @'MissingLegalholdConsent ()) $
    mapError @LegalholdConflictsOldClients (const $ Tagged @'MissingLegalholdConsentOldClients ()) $
      guardLegalholdPolicyConflicts (glhProtectee glh) (glhUserClients glh)

-- | Get an MLS conversation client list
iGetMLSClientListForConv ::
  forall r.
  Members
    '[ MemberStore,
       ErrorS 'ConvNotFound
     ]
    r =>
  GroupId ->
  Sem r ClientList
iGetMLSClientListForConv gid = do
  cm <- E.lookupMLSClients gid
  pure $ ClientList (concatMap (Map.keys . snd) (Map.assocs cm))
