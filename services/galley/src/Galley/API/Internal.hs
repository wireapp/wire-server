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
  ( internalSitemap,
    internalAPI,
    InternalAPI,
    deleteLoop,
    safeForever,
  )
where

import Control.Exception.Safe (catchAny)
import Control.Lens hiding (Getter, Setter, (.=))
import Data.Id as Id
import Data.List1 (maybeList1)
import Data.Qualified
import Data.Range
import Data.Singletons
import Data.String.Conversions (cs)
import Data.Time
import qualified Galley.API.Clients as Clients
import qualified Galley.API.Create as Create
import qualified Galley.API.CustomBackend as CustomBackend
import Galley.API.Error
import Galley.API.LegalHold (unsetTeamLegalholdWhitelistedH)
import Galley.API.LegalHold.Conflicts
import Galley.API.MLS.Removal
import Galley.API.One2One
import Galley.API.Public
import Galley.API.Public.Servant
import qualified Galley.API.Query as Query
import Galley.API.Teams (uncheckedDeleteTeamMember)
import qualified Galley.API.Teams as Teams
import Galley.API.Teams.Features
import qualified Galley.API.Update as Update
import Galley.API.Util
import Galley.App
import Galley.Cassandra.TeamFeatures
import qualified Galley.Data.Conversation as Data
import Galley.Effects
import Galley.Effects.ClientStore
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Galley.Effects.LegalHoldStore as LegalHoldStore
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Effects.TeamStore
import qualified Galley.Intra.Push as Intra
import Galley.Monad
import Galley.Options
import qualified Galley.Queue as Q
import Galley.Types.Bot (AddBot, RemoveBot)
import Galley.Types.Bot.Service
import Galley.Types.Conversations.Members (RemoteMember (rmId))
import Galley.Types.UserList
import Imports hiding (head)
import Network.Wai.Predicate hiding (Error, err)
import qualified Network.Wai.Predicate as Predicate
import Network.Wai.Routing hiding (App, route, toList)
import Network.Wai.Utilities hiding (Error)
import Network.Wai.Utilities.ZAuth
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import Servant hiding (JSON, WithStatus)
import System.Logger.Class hiding (Path, name)
import qualified System.Logger.Class as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.CustomBackend
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Provider.Service hiding (Service)
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Galley
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Routes.MultiTablePaging (mtpHasMore, mtpPagingState, mtpResults)
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra

internalAPI :: API InternalAPI GalleyEffects
internalAPI =
  hoistAPI @InternalAPIBase id $
    mkNamedAPI @"status" (pure ())
      <@> mkNamedAPI @"delete-user" (callsFed (exposeAnnotations rmUser))
      <@> mkNamedAPI @"connect" (callsFed (exposeAnnotations Create.createConnectConversation))
      <@> mkNamedAPI @"guard-legalhold-policy-conflicts" guardLegalholdPolicyConflictsH
      <@> legalholdWhitelistedTeamsAPI
      <@> iTeamsAPI
      <@> mkNamedAPI @"upsert-one2one" iUpsertOne2OneConversation
      <@> featureAPI

legalholdWhitelistedTeamsAPI :: API ILegalholdWhitelistedTeamsAPI GalleyEffects
legalholdWhitelistedTeamsAPI = mkAPI $ \tid -> hoistAPIHandler id (base tid)
  where
    base :: TeamId -> API ILegalholdWhitelistedTeamsAPIBase GalleyEffects
    base tid =
      mkNamedAPI @"set-team-legalhold-whitelisted" (LegalHoldStore.setTeamLegalholdWhitelisted tid)
        <@> mkNamedAPI @"unset-team-legalhold-whitelisted" (unsetTeamLegalholdWhitelistedH tid)
        <@> mkNamedAPI @"get-team-legalhold-whitelisted" (LegalHoldStore.isTeamLegalholdWhitelisted tid)

iTeamsAPI :: API ITeamsAPI GalleyEffects
iTeamsAPI = mkAPI $ \tid -> hoistAPIHandler id (base tid)
  where
    hoistAPISegment ::
      (ServerT (seg :> inner) (Sem r) ~ ServerT inner (Sem r)) =>
      API inner r ->
      API (seg :> inner) r
    hoistAPISegment = hoistAPI id

    base :: TeamId -> API ITeamsAPIBase GalleyEffects
    base tid =
      mkNamedAPI @"get-team-internal" (Teams.getTeamInternalH tid)
        <@> mkNamedAPI @"create-binding-team" (Teams.createBindingTeam tid)
        <@> mkNamedAPI @"delete-binding-team" (Teams.internalDeleteBindingTeam tid)
        <@> mkNamedAPI @"get-team-name" (Teams.getTeamNameInternalH tid)
        <@> mkNamedAPI @"update-team-status" (Teams.updateTeamStatus tid)
        <@> hoistAPISegment
          ( mkNamedAPI @"unchecked-add-team-member" (Teams.uncheckedAddTeamMember @Cassandra tid)
              <@> mkNamedAPI @"unchecked-get-team-members" (Teams.uncheckedGetTeamMembersH tid)
              <@> mkNamedAPI @"unchecked-get-team-member" (Teams.uncheckedGetTeamMember tid)
              <@> mkNamedAPI @"can-user-join-team" (Teams.canUserJoinTeam @Cassandra tid)
              <@> mkNamedAPI @"unchecked-update-team-member" (Teams.uncheckedUpdateTeamMember Nothing Nothing tid)
          )
        <@> mkNamedAPI @"user-is-team-owner" (Teams.userIsTeamOwner tid)
        <@> hoistAPISegment
          ( mkNamedAPI @"get-search-visibility-internal" (Teams.getSearchVisibilityInternal tid)
              <@> mkNamedAPI @"set-search-visibility-internal" (Teams.setSearchVisibilityInternal (featureEnabledForTeam @Cassandra @SearchVisibilityAvailableConfig) tid)
          )

featureAPI :: API IFeatureAPI GalleyEffects
featureAPI =
  mkNamedAPI @'("iget", SSOConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", SSOConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", SSOConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", LegalholdConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", LegalholdConfig) (callsFed (exposeAnnotations (setFeatureStatusInternal @Cassandra)))
    <@> mkNamedAPI @'("ipatch", LegalholdConfig) (callsFed (exposeAnnotations (patchFeatureStatusInternal @Cassandra)))
    <@> mkNamedAPI @'("iget", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", SearchVisibilityAvailableConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", SearchVisibilityAvailableConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", ValidateSAMLEmailsConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", ValidateSAMLEmailsConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", DigitalSignaturesConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", DigitalSignaturesConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", DigitalSignaturesConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", AppLockConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", AppLockConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", AppLockConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", FileSharingConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", FileSharingConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ilock", FileSharingConfig) (updateLockStatus @Cassandra @FileSharingConfig)
    <@> mkNamedAPI @'("ipatch", FileSharingConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", ConferenceCallingConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", ConferenceCallingConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", ConferenceCallingConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", SelfDeletingMessagesConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", SelfDeletingMessagesConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ilock", SelfDeletingMessagesConfig) (updateLockStatus @Cassandra @SelfDeletingMessagesConfig)
    <@> mkNamedAPI @'("ipatch", SelfDeletingMessagesConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", GuestLinksConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", GuestLinksConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ilock", GuestLinksConfig) (updateLockStatus @Cassandra @GuestLinksConfig)
    <@> mkNamedAPI @'("ipatch", GuestLinksConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", SndFactorPasswordChallengeConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", SndFactorPasswordChallengeConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ilock", SndFactorPasswordChallengeConfig) (updateLockStatus @Cassandra @SndFactorPasswordChallengeConfig)
    <@> mkNamedAPI @'("ipatch", SndFactorPasswordChallengeConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", SearchVisibilityInboundConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", SearchVisibilityInboundConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", SearchVisibilityInboundConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("igetmulti", SearchVisibilityInboundConfig) (getFeatureStatusMulti @Cassandra)
    <@> mkNamedAPI @'("iget", ClassifiedDomainsConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iget", MLSConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", MLSConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", MLSConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", ExposeInvitationURLsToTeamAdminConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", ExposeInvitationURLsToTeamAdminConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", ExposeInvitationURLsToTeamAdminConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", SearchVisibilityInboundConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", SearchVisibilityInboundConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", SearchVisibilityInboundConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("iget", OutlookCalIntegrationConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", OutlookCalIntegrationConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", OutlookCalIntegrationConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ilock", OutlookCalIntegrationConfig) (updateLockStatus @Cassandra @OutlookCalIntegrationConfig)
    <@> mkNamedAPI @'("iget", MlsE2EIdConfig) (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI @'("iput", MlsE2EIdConfig) (setFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ipatch", MlsE2EIdConfig) (patchFeatureStatusInternal @Cassandra)
    <@> mkNamedAPI @'("ilock", MlsE2EIdConfig) (updateLockStatus @Cassandra @MlsE2EIdConfig)
    <@> mkNamedAPI @"feature-configs-internal" (maybe getAllFeatureConfigsForServer (getAllFeatureConfigsForUser @Cassandra))

internalSitemap :: Routes a (Sem GalleyEffects) ()
internalSitemap = unsafeCallsFed @'Galley @"on-client-removed" $ unsafeCallsFed @'Galley @"on-mls-message-sent" $ do
  -- Conversation API (internal) ----------------------------------------
  put "/i/conversations/:cnv/channel" (continue $ const (pure empty)) $
    zauthUserId
      .&. (capture "cnv" :: HasCaptures r => Predicate r Predicate.Error ConvId)
      .&. request

  get "/i/conversations/:cnv/members/:usr" (continue Query.internalGetMemberH) $
    capture "cnv"
      .&. capture "usr"

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to you, if the conversation existed and had < 2 members before
  -- - MemberJoin event to other, if the conversation existed and only the other was member
  --   before
  put "/i/conversations/:cnv/accept/v2" (continueE Update.acceptConvH) $
    zauthUserId
      .&. opt zauthConnId
      .&. capture "cnv"

  put "/i/conversations/:cnv/block" (continueE Update.blockConvH) $
    zauthUserId
      .&. capture "cnv"

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to you, if the conversation existed and had < 2 members before
  -- - MemberJoin event to other, if the conversation existed and only the other was member
  --   before
  put "/i/conversations/:cnv/unblock" (continueE Update.unblockConvH) $
    zauthUserId
      .&. opt zauthConnId
      .&. capture "cnv"

  get "/i/conversations/:cnv/meta" (continue Query.getConversationMetaH) $
    capture "cnv"

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
    ( Member BrigAccess r,
      Member ClientStore r,
      Member ConversationStore r,
      Member (Error InternalError) r,
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member GundeckAccess r,
      Member (Input Env) r,
      Member (Input (Local ())) r,
      Member (Input UTCTime) r,
      Member (ListItems p1 ConvId) r,
      Member (ListItems p1 (Remote ConvId)) r,
      Member (ListItems p2 TeamId) r,
      Member MemberStore r,
      Member ProposalStore r,
      Member P.TinyLog r,
      Member TeamStore r
    )
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
      mems <- getTeamMembersForFanout tid
      uncheckedDeleteTeamMember lusr conn tid (tUnqualified lusr) mems
      page' <- listTeams @p2 (tUnqualified lusr) (Just (pageState page)) maxBound
      leaveTeams page'

    leaveLocalConversations :: [ConvId] -> Sem r ()
    leaveLocalConversations ids = do
      let qUser = tUntagged lusr
      cc <- getConversations ids
      now <- input
      pp <- for cc $ \c -> case Data.convType c of
        SelfConv -> pure Nothing
        One2OneConv -> deleteMembers (Data.convId c) (UserList [tUnqualified lusr] []) $> Nothing
        ConnectConv -> deleteMembers (Data.convId c) (UserList [tUnqualified lusr] []) $> Nothing
        RegularConv
          | tUnqualified lusr `isMember` Data.convLocalMembers c -> do
              runError (removeUser (qualifyAs lusr c) (tUntagged lusr)) >>= \case
                Left e -> P.err $ Log.msg ("failed to send remove proposal: " <> internalErrorDescription e)
                Right _ -> pure ()
              deleteMembers (Data.convId c) (UserList [tUnqualified lusr] [])
              let e =
                    Event
                      (tUntagged (qualifyAs lusr (Data.convId c)))
                      Nothing
                      (tUntagged lusr)
                      now
                      (EdMembersLeave (QualifiedUserIdList [qUser]))
              for_ (bucketRemote (fmap rmId (Data.convRemoteMembers c))) $ notifyRemoteMembers now qUser (Data.convId c)
              pure $
                Intra.newPushLocal ListComplete (tUnqualified lusr) (Intra.ConvEvent e) (Intra.recipient <$> Data.convLocalMembers c)
                  <&> set Intra.pushConn conn
                    . set Intra.pushRoute Intra.RouteDirect
          | otherwise -> pure Nothing

      for_
        (maybeList1 (catMaybes pp))
        push

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
    leaveRemoteConversations cids = do
      for_ (bucketRemote (fromRange cids)) $ \remoteConvs -> do
        let userDelete = UserDeletedConversationsNotification (tUnqualified lusr) (unsafeRange (tUnqualified remoteConvs))
        let rpc = fedClient @'Galley @"on-user-deleted-conversations" userDelete
        runFederatedEither remoteConvs rpc
          >>= logAndIgnoreError "Error in onUserDeleted call" (tUnqualified lusr)

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
    Member (ErrorS 'MissingLegalholdConsent) r
  ) =>
  GuardLegalholdPolicyConflicts ->
  Sem r ()
guardLegalholdPolicyConflictsH glh = do
  mapError @LegalholdConflicts (const $ Tagged @'MissingLegalholdConsent ()) $
    guardLegalholdPolicyConflicts (glhProtectee glh) (glhUserClients glh)
