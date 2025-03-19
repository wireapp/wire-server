{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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
  ( internalAPI,
    InternalAPI,
    deleteLoop,
    safeForever,
  )
where

import Control.Exception.Safe (catchAny)
import Control.Lens hiding (Getter, Setter, (.=))
import Data.ByteString.UTF8 qualified as UTF8
import Data.Default
import Data.Id as Id
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Map qualified as Map
import Data.Qualified
import Data.Range
import Data.Singletons
import Data.Time
import Galley.API.Action
import Galley.API.Cells
import Galley.API.Clients qualified as Clients
import Galley.API.Create qualified as Create
import Galley.API.Error
import Galley.API.LegalHold (unsetTeamLegalholdWhitelistedH)
import Galley.API.LegalHold.Conflicts
import Galley.API.MLS.Removal
import Galley.API.One2One
import Galley.API.Public.Servant
import Galley.API.Query qualified as Query
import Galley.API.Teams
import Galley.API.Teams qualified as Teams
import Galley.API.Teams.Features
import Galley.API.Teams.Features.Get
import Galley.API.Update qualified as Update
import Galley.API.Util
import Galley.App
import Galley.Data.Conversation qualified as Data
import Galley.Effects
import Galley.Effects.BackendNotificationQueueAccess
import Galley.Effects.ClientStore
import Galley.Effects.ConversationStore
import Galley.Effects.CustomBackendStore
import Galley.Effects.LegalHoldStore as LegalHoldStore
import Galley.Effects.MemberStore qualified as E
import Galley.Effects.ServiceStore
import Galley.Effects.TeamStore
import Galley.Effects.TeamStore qualified as E
import Galley.Monad
import Galley.Options hiding (brig)
import Galley.Queue qualified as Q
import Galley.Types.Conversations.Members (RemoteMember (rmId))
import Galley.Types.UserList
import Imports hiding (head)
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import Servant
import System.Logger.Class hiding (Path, name)
import System.Logger.Class qualified as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Brig.EJPD
import Wire.API.Routes.Internal.Galley
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Routes.MultiTablePaging (mtpHasMore, mtpPagingState, mtpResults)
import Wire.API.Routes.MultiTablePaging qualified as MTP
import Wire.API.Team.Feature
import Wire.API.User.Client
import Wire.NotificationSubsystem
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra

internalAPI :: API InternalAPI GalleyEffects
internalAPI =
  hoistAPI @InternalAPIBase Imports.id $
    mkNamedAPI @"status" (pure ())
      <@> mkNamedAPI @"delete-user" rmUser
      <@> mkNamedAPI @"connect" Create.createConnectConversation
      <@> mkNamedAPI @"get-conversation-clients" iGetMLSClientListForConv
      <@> mkNamedAPI @"guard-legalhold-policy-conflicts" guardLegalholdPolicyConflictsH
      <@> legalholdWhitelistedTeamsAPI
      <@> iTeamsAPI
      <@> miscAPI
      <@> mkNamedAPI @"upsert-one2one" iUpsertOne2OneConversation
      <@> featureAPI
      <@> federationAPI
      <@> conversationAPI
      <@> iEJPDAPI
      <@> cellsAPI

iEJPDAPI :: API IEJPDAPI GalleyEffects
iEJPDAPI = mkNamedAPI @"get-conversations-by-user" ejpdGetConvInfo

-- | An unpaginated, internal http interface to `Query.conversationIdsPageFrom`.  Used for
-- EJPD reports.  Called locally with very little data for each conv, so we don't expect
-- pagination to ever be needed.
ejpdGetConvInfo ::
  forall r p.
  ( p ~ CassandraPaging,
    Member ConversationStore r,
    Member (Error InternalError) r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
    Member (ListItems p ConvId) r,
    Member (ListItems p (Remote ConvId)) r,
    Member P.TinyLog r
  ) =>
  UserId ->
  Sem r [EJPDConvInfo]
ejpdGetConvInfo uid = do
  luid <- qualifyLocal uid
  firstPage <- Query.conversationIdsPageFrom luid initialPageRequest
  getPages luid firstPage
  where
    initialPageRequest = mkPageRequest (MTP.MultiTablePagingState MTP.PagingLocals Nothing)
    mkPageRequest = MTP.GetMultiTablePageRequest (toRange (Proxy @1000)) . Just

    getPages :: Local UserId -> ConvIdsPage -> Sem r [EJPDConvInfo]
    getPages luid page = do
      let convids = MTP.mtpResults page
          mk :: Data.Conversation -> Maybe EJPDConvInfo
          mk conv = do
            let convType = conv.convMetadata.cnvmType
                ejpdConvInfo = EJPDConvInfo (fromMaybe "n/a" conv.convMetadata.cnvmName) (tUntagged $ qualifyAs luid conv.convId)
            -- we don't want self conversations as they don't tell us anything about connections
            -- we don't want connect conversations, because the peer has not responded yet
            case convType of
              RegularConv -> Just ejpdConvInfo
              -- FUTUREWORK(mangoiv): with GHC 9.12 we can refactor this to or-patterns
              One2OneConv -> Nothing
              SelfConv -> Nothing
              ConnectConv -> Nothing
      renderedPage <- mapMaybe mk <$> getConversations (fst $ partitionQualified luid convids)
      if MTP.mtpHasMore page
        then do
          newPage <- Query.conversationIdsPageFrom luid (mkPageRequest . MTP.mtpPagingState $ page)
          morePages <- getPages luid newPage
          pure $ renderedPage <> morePages
        else pure renderedPage

federationAPI :: API IFederationAPI GalleyEffects
federationAPI =
  mkNamedAPI @"get-federation-status" (const getFederationStatus)

conversationAPI :: API IConversationAPI GalleyEffects
conversationAPI =
  mkNamedAPI @"conversation-get-member" Query.internalGetMember
    <@> mkNamedAPI @"conversation-accept-v2" Update.acceptConv
    <@> mkNamedAPI @"conversation-block" Update.blockConv
    <@> mkNamedAPI @"conversation-unblock" Update.unblockConv
    <@> mkNamedAPI @"conversation-meta" Query.getConversationMeta
    <@> mkNamedAPI @"conversation-mls-one-to-one" Query.getMLSOne2OneConversationInternal
    <@> mkNamedAPI @"conversation-mls-one-to-one-established" Query.isMLSOne2OneEstablished

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

miscAPI :: API IMiscAPI GalleyEffects
miscAPI =
  mkNamedAPI @"get-team-members" Teams.getBindingTeamMembers
    <@> mkNamedAPI @"get-team-id" lookupBindingTeam
    <@> mkNamedAPI @"test-get-clients" Clients.getClients
    <@> mkNamedAPI @"test-add-client" createClient
    <@> mkNamedAPI @"test-delete-client" Clients.rmClient
    <@> mkNamedAPI @"add-service" createService
    <@> mkNamedAPI @"delete-service" deleteService
    <@> mkNamedAPI @"i-add-bot" Update.addBot
    <@> mkNamedAPI @"delete-bot" Update.rmBot
    <@> mkNamedAPI @"put-custom-backend" setCustomBackend
    <@> mkNamedAPI @"delete-custom-backend" deleteCustomBackend

featureAPI1Full ::
  forall cfg r.
  (_) =>
  API (IFeatureAPI1Full cfg) r
featureAPI1Full =
  mkNamedAPI @'("iget", cfg) getFeatureInternal
    <@> mkNamedAPI @'("iput", cfg) setFeatureInternal
    <@> mkNamedAPI @'("ipatch", cfg) patchFeatureInternal

featureAPI1Get ::
  forall cfg r.
  (_) =>
  API (IFeatureStatusGet cfg) r
featureAPI1Get = mkNamedAPI @'("iget", cfg) getFeatureInternal

allFeaturesAPI :: API (IAllFeaturesAPI Features) GalleyEffects
allFeaturesAPI =
  featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Get
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full
    <@> featureAPI1Full

featureAPI :: API IFeatureAPI GalleyEffects
featureAPI =
  allFeaturesAPI
    -- legacy endpoints
    <@> mkNamedAPI @'("ilock", FileSharingConfig) (updateLockStatus @FileSharingConfig)
    <@> mkNamedAPI @'("ilock", ConferenceCallingConfig) (updateLockStatus @ConferenceCallingConfig)
    <@> mkNamedAPI @'("ilock", SelfDeletingMessagesConfig) (updateLockStatus @SelfDeletingMessagesConfig)
    <@> mkNamedAPI @'("ilock", GuestLinksConfig) (updateLockStatus @GuestLinksConfig)
    <@> mkNamedAPI @'("ilock", SndFactorPasswordChallengeConfig) (updateLockStatus @SndFactorPasswordChallengeConfig)
    <@> mkNamedAPI @'("ilock", MLSConfig) (updateLockStatus @MLSConfig)
    <@> mkNamedAPI @'("ilock", OutlookCalIntegrationConfig) (updateLockStatus @OutlookCalIntegrationConfig)
    <@> mkNamedAPI @'("ilock", MlsE2EIdConfig) (updateLockStatus @MlsE2EIdConfig)
    <@> mkNamedAPI @'("ilock", MlsMigrationConfig) (updateLockStatus @MlsMigrationConfig)
    <@> mkNamedAPI @'("ilock", EnforceFileDownloadLocationConfig) (updateLockStatus @EnforceFileDownloadLocationConfig)
    <@> mkNamedAPI @'("ilock", DomainRegistrationConfig) (updateLockStatus @DomainRegistrationConfig)
    <@> mkNamedAPI @'("ilock", ChannelsConfig) (updateLockStatus @ChannelsConfig)
    <@> mkNamedAPI @'("ilock", CellsConfig) (updateLockStatus @CellsConfig)
    -- all features
    <@> mkNamedAPI @"feature-configs-internal" (maybe getAllTeamFeaturesForServer getAllTeamFeaturesForUser)

cellsAPI :: API ICellsAPI GalleyEffects
cellsAPI = mkNamedAPI @"set-cells-state" Update.updateCellsState

rmUser ::
  forall p1 p2 r.
  ( p1 ~ CassandraPaging,
    p2 ~ InternalPaging,
    Member BackendNotificationQueueAccess r,
    Member ClientStore r,
    Member ConversationStore r,
    Member (Error DynError) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member ExternalAccess r,
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
    Member Random r,
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
          getFeatureForTeam @LimitedEventFanoutConfig tid
            >>= ( \case
                    FeatureStatusEnabled -> Left <$> E.getTeamAdmins tid
                    FeatureStatusDisabled -> Right <$> getTeamMembersForFanout tid
                )
              . (.status)
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
              pure . Just $
                def
                  { origin = Just (tUnqualified lusr),
                    json = toJSONObject e,
                    recipients = map localMemberToRecipient (Data.convLocalMembers c),
                    isCellsEvent = shouldPushToCells c.convMetadata (evtType e),
                    conn,
                    route = PushV2.RouteDirect
                  }
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
              { time = now,
                origUserId = qUser,
                convId = cid,
                alreadyPresentUsers = tUnqualified remotes,
                action = SomeConversationAction (sing @'ConversationLeaveTag) ()
              }
      enqueueNotification Q.Persistent remotes $ do
        makeConversationUpdateBundle convUpdate
          >>= sendBundle

    leaveRemoteConversations :: Range 1 UserDeletedNotificationMaxConvs [Remote ConvId] -> Sem r ()
    leaveRemoteConversations cids =
      for_ (bucketRemote (fromRange cids)) $ \remoteConvs -> do
        let userDelete = UserDeletedConversationsNotification (tUnqualified lusr) (unsafeRange (tUnqualified remoteConvs))
        let rpc = fedQueueClient @'OnUserDeletedConversationsTag userDelete
        enqueueNotification Q.Persistent remoteConvs rpc

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
      err $ "error" .= show exc ~~ msg (val $ UTF8.fromString funName <> " failed")
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
  ( Members
      '[ MemberStore,
         ErrorS 'ConvNotFound
       ]
      r
  ) =>
  GroupId ->
  Sem r ClientList
iGetMLSClientListForConv gid = do
  cm <- E.lookupMLSClients gid
  pure $ ClientList (concatMap (Map.keys . snd) (Map.assocs cm))
