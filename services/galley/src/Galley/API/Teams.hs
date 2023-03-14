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
{-# LANGUAGE LambdaCase #-}

module Galley.API.Teams
  ( createBindingTeam,
    createNonBindingTeamH,
    updateTeamH,
    updateTeamStatus,
    getTeamH,
    getTeamInternalH,
    getTeamNameInternalH,
    getBindingTeamIdH,
    getBindingTeamMembersH,
    getManyTeams,
    deleteTeam,
    uncheckedDeleteTeam,
    addTeamMember,
    getTeamConversationRoles,
    getTeamMembers,
    getTeamMembersCSV,
    bulkGetTeamMembers,
    getTeamMember,
    deleteTeamMember,
    deleteNonBindingTeamMember,
    updateTeamMember,
    getTeamConversations,
    getTeamConversation,
    deleteTeamConversation,
    getSearchVisibility,
    setSearchVisibility,
    getSearchVisibilityInternal,
    setSearchVisibilityInternal,
    uncheckedAddTeamMember,
    uncheckedGetTeamMember,
    uncheckedGetTeamMembersH,
    uncheckedDeleteTeamMember,
    uncheckedUpdateTeamMember,
    userIsTeamOwner,
    canUserJoinTeam,
    ensureNotTooLargeForLegalHold,
    ensureNotTooLargeToActivateLegalHold,
    internalDeleteBindingTeam,
  )
where

import Brig.Types.Intra (accountUser)
import Brig.Types.Team (TeamSize (..))
import Cassandra (PageWithState (pwsResults), pwsHasMore)
import qualified Cassandra as C
import Control.Lens
import Data.ByteString.Builder (lazyByteString)
import Data.ByteString.Conversion (List, toByteString)
import qualified Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Csv (EncodeOptions (..), Quoting (QuoteAll), encodeDefaultOrderedByNameWith)
import qualified Data.Handle as Handle
import Data.Id
import qualified Data.LegalHold as LH
import qualified Data.List.Extra as List
import Data.List1 (list1)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Misc (HttpsUrl, mkHttpsUrl)
import Data.Proxy
import Data.Qualified
import Data.Range as Range
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import Galley.API.Error as Galley
import Galley.API.LegalHold
import qualified Galley.API.Teams.Notifications as APITeamQueue
import qualified Galley.API.Update as API
import Galley.API.Util
import Galley.App
import qualified Galley.Data.Conversation as Data
import Galley.Data.Services (BotMember)
import Galley.Effects
import qualified Galley.Effects.BrigAccess as E
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.ExternalAccess as E
import qualified Galley.Effects.GundeckAccess as E
import qualified Galley.Effects.LegalHoldStore as Data
import qualified Galley.Effects.ListItems as E
import qualified Galley.Effects.MemberStore as E
import qualified Galley.Effects.Queue as E
import qualified Galley.Effects.SearchVisibilityStore as SearchVisibilityData
import qualified Galley.Effects.SparAccess as Spar
import Galley.Effects.TeamFeatureStore (FeaturePersistentConstraint)
import qualified Galley.Effects.TeamMemberStore as E
import qualified Galley.Effects.TeamStore as E
import qualified Galley.Intra.Journal as Journal
import Galley.Intra.Push
import Galley.Options
import qualified Galley.Types.Conversations.Members as Conv
import Galley.Types.Teams
import Galley.Types.UserList
import Imports hiding (forkIO)
import Network.Wai
import Network.Wai.Utilities hiding (Error)
import Polysemy
import Polysemy.Error
import Polysemy.Final
import Polysemy.Input
import Polysemy.Output
import qualified Polysemy.TinyLog as P
import qualified SAML2.WebSSO as SAML
import System.Logger (Msg)
import qualified System.Logger.Class as Log
import Wire.API.Conversation.Role (Action (DeleteConversation), wireConvRoles)
import qualified Wire.API.Conversation.Role as Public
import Wire.API.Error
import Wire.API.Error.Galley
import qualified Wire.API.Event.Conversation as Conv
import Wire.API.Event.Team
import Wire.API.Federation.Error
import qualified Wire.API.Message as Conv
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Routes.MultiTablePaging (MultiTablePage (MultiTablePage), MultiTablePagingState (mtpsState))
import Wire.API.Routes.Public.Galley.TeamMember
import Wire.API.Team
import qualified Wire.API.Team as Public
import Wire.API.Team.Conversation
import qualified Wire.API.Team.Conversation as Public
import Wire.API.Team.Export (TeamExportUser (..))
import Wire.API.Team.Feature
import Wire.API.Team.Member
import qualified Wire.API.Team.Member as Public
import Wire.API.Team.Permission (Perm (..), Permissions (..), SPerm (..), copy, fullPermissions, self)
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import qualified Wire.API.Team.SearchVisibility as Public
import Wire.API.User (ScimUserInfo (..), User, UserIdList, UserSSOId (UserScimExternalId), userSCIMExternalId, userSSOId)
import qualified Wire.API.User as U
import Wire.API.User.Identity (UserSSOId (UserSSOId))
import Wire.API.User.RichInfo (RichInfo)
import qualified Wire.Sem.Paging as E
import Wire.Sem.Paging.Cassandra

getTeamH ::
  forall r.
  (Member (ErrorS 'TeamNotFound) r, Member (Queue DeleteItem) r, Member TeamStore r) =>
  UserId ->
  TeamId ->
  Sem r Public.Team
getTeamH zusr tid =
  maybe (throwS @'TeamNotFound) pure =<< lookupTeam zusr tid

getTeamInternalH ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r TeamData
getTeamInternalH tid =
  E.getTeam tid >>= noteS @'TeamNotFound

getTeamNameInternalH ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r TeamName
getTeamNameInternalH tid =
  getTeamNameInternal tid >>= noteS @'TeamNotFound

getTeamNameInternal :: Member TeamStore r => TeamId -> Sem r (Maybe TeamName)
getTeamNameInternal = fmap (fmap TeamName) . E.getTeamName

-- | DEPRECATED.
--
-- The endpoint was designed to query non-binding teams. However, non-binding teams is a feature
-- that has never been adopted by clients, but the endpoint also returns the binding team of a user and it is
-- possible that this is being used by a client, even though unlikely.
--
-- The following functionality has been changed: query parameters will be ignored, which has the effect
-- that regardless of the parameters the response will always contain the binding team of the user if
-- it exists. Even though they are ignored, the use of query parameters will not result in an error.
--
-- (If you want to be pedantic, the `size` parameter is still honored: its allowed range is
-- between 1 and 100, and that will always be an upper bound of the result set of size 0 or
-- one.)
getManyTeams ::
  ( Member TeamStore r,
    Member (Queue DeleteItem) r,
    Member (ListItems LegacyPaging TeamId) r
  ) =>
  UserId ->
  Sem r Public.TeamList
getManyTeams zusr =
  withTeamIds zusr Nothing (toRange (Proxy @100)) $ \more ids -> do
    teams <- mapM (lookupTeam zusr) ids
    pure (Public.newTeamList (catMaybes teams) more)

lookupTeam ::
  ( Member TeamStore r,
    Member (Queue DeleteItem) r
  ) =>
  UserId ->
  TeamId ->
  Sem r (Maybe Public.Team)
lookupTeam zusr tid = do
  tm <- E.getTeamMember tid zusr
  if isJust tm
    then do
      t <- E.getTeam tid
      when (Just PendingDelete == (tdStatus <$> t)) $ do
        void $ E.tryPush (TeamItem tid zusr Nothing)
      pure (tdTeam <$> t)
    else pure Nothing

createNonBindingTeamH ::
  forall r.
  ( Member BrigAccess r,
    Member (ErrorS 'UserBindingExists) r,
    Member (ErrorS 'NotConnected) r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member P.TinyLog r,
    Member TeamStore r
  ) =>
  UserId ->
  ConnId ->
  Public.NonBindingNewTeam ->
  Sem r TeamId
createNonBindingTeamH zusr zcon (Public.NonBindingNewTeam body) = do
  let owner = Public.mkTeamMember zusr fullPermissions Nothing LH.defUserLegalHoldStatus
  let others =
        filter ((zusr /=) . view userId)
          . maybe [] fromRange
          $ body ^. newTeamMembers
  let zothers = map (view userId) others
  ensureUnboundUsers (zusr : zothers)
  ensureConnectedToLocals zusr zothers
  P.debug $
    Log.field "targets" (toByteString . show $ toByteString <$> zothers)
      . Log.field "action" (Log.val "Teams.createNonBindingTeam")
  team <-
    E.createTeam
      Nothing
      zusr
      (body ^. newTeamName)
      (body ^. newTeamIcon)
      (body ^. newTeamIconKey)
      NonBinding
  finishCreateTeam team owner others (Just zcon)
  pure (team ^. teamId)

createBindingTeam ::
  ( Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  TeamId ->
  UserId ->
  BindingNewTeam ->
  Sem r TeamId
createBindingTeam tid zusr (BindingNewTeam body) = do
  let owner = Public.mkTeamMember zusr fullPermissions Nothing LH.defUserLegalHoldStatus
  team <-
    E.createTeam (Just tid) zusr (body ^. newTeamName) (body ^. newTeamIcon) (body ^. newTeamIconKey) Binding
  finishCreateTeam team owner [] Nothing
  pure tid

updateTeamStatus ::
  ( Member BrigAccess r,
    Member (ErrorS 'InvalidTeamStatusUpdate) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member P.TinyLog r,
    Member TeamStore r
  ) =>
  TeamId ->
  TeamStatusUpdate ->
  Sem r ()
updateTeamStatus tid (TeamStatusUpdate newStatus cur) = do
  oldStatus <- fmap tdStatus $ E.getTeam tid >>= noteS @'TeamNotFound
  valid <- validateTransition (oldStatus, newStatus)
  when valid $ do
    journal newStatus cur
    E.setTeamStatus tid newStatus
  where
    journal Suspended _ = Journal.teamSuspend tid
    journal Active c = do
      teamCreationTime <- E.getTeamCreationTime tid
      -- When teams are created, they are activated immediately. In this situation, Brig will
      -- most likely report team size as 0 due to ES taking some time to index the team creator.
      -- This is also very difficult to test, so is not tested.
      (TeamSize possiblyStaleSize) <- E.getSize tid
      let size =
            if possiblyStaleSize == 0
              then 1
              else possiblyStaleSize
      Journal.teamActivate tid size c teamCreationTime
    journal _ _ = throwS @'InvalidTeamStatusUpdate
    validateTransition :: Member (ErrorS 'InvalidTeamStatusUpdate) r => (TeamStatus, TeamStatus) -> Sem r Bool
    validateTransition = \case
      (PendingActive, Active) -> pure True
      (Active, Active) -> pure False
      (Active, Suspended) -> pure True
      (Suspended, Active) -> pure True
      (Suspended, Suspended) -> pure False
      (_, _) -> throwS @'InvalidTeamStatusUpdate

updateTeamH ::
  ( Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS ('MissingPermission ('Just 'SetTeamData))) r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  UserId ->
  ConnId ->
  TeamId ->
  Public.TeamUpdateData ->
  Sem r ()
updateTeamH zusr zcon tid updateData = do
  zusrMembership <- E.getTeamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "Teams.updateTeam")
  void $ permissionCheckS SSetTeamData zusrMembership
  E.setTeamData tid updateData
  now <- input
  memList <- getTeamMembersForFanout tid
  let e = newEvent tid now (EdTeamUpdate updateData)
  let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) (memList ^. teamMembers))
  E.push1 $ newPushLocal1 (memList ^. teamMemberListType) zusr (TeamEvent e) r & pushConn ?~ zcon

deleteTeam ::
  forall r.
  ( Member BrigAccess r,
    Member (Error AuthenticationError) r,
    Member (ErrorS 'DeleteQueueFull) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Queue DeleteItem) r,
    Member TeamStore r
  ) =>
  UserId ->
  ConnId ->
  TeamId ->
  Public.TeamDeleteData ->
  Sem r ()
deleteTeam zusr zcon tid body = do
  team <- E.getTeam tid >>= noteS @'TeamNotFound
  case tdStatus team of
    Deleted -> throwS @'TeamNotFound
    PendingDelete ->
      queueTeamDeletion tid zusr (Just zcon)
    _ -> do
      checkPermissions team
      queueTeamDeletion tid zusr (Just zcon)
  where
    checkPermissions team = do
      void $ permissionCheck DeleteTeam =<< E.getTeamMember tid zusr
      when (tdTeam team ^. teamBinding == Binding) $ do
        ensureReAuthorised zusr (body ^. tdAuthPassword) (body ^. tdVerificationCode) (Just U.DeleteTeam)

-- This can be called by stern
internalDeleteBindingTeam ::
  ( Member (ErrorS 'NoBindingTeam) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NotAOneMemberTeam) r,
    Member (ErrorS 'DeleteQueueFull) r,
    Member (Queue DeleteItem) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Bool ->
  Sem r ()
internalDeleteBindingTeam tid force = do
  mbTeamData <- E.getTeam tid
  case tdTeam <$> mbTeamData of
    Nothing -> throwS @'TeamNotFound
    Just team | team ^. teamBinding /= Binding -> throwS @'NoBindingTeam
    Just team -> do
      mems <- E.getTeamMembersWithLimit tid (unsafeRange 2)
      case mems ^. teamMembers of
        [mem] -> queueTeamDeletion tid (mem ^. userId) Nothing
        -- if the team has more than one member (and deletion is forced) or no members we use the team creator's userId for deletion events
        xs | null xs || force -> queueTeamDeletion tid (team ^. teamCreator) Nothing
        _ -> throwS @'NotAOneMemberTeam

-- This function is "unchecked" because it does not validate that the user has the `DeleteTeam` permission.
uncheckedDeleteTeam ::
  forall r.
  ( Member BrigAccess r,
    Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member SparAccess r,
    Member TeamStore r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  TeamId ->
  Sem r ()
uncheckedDeleteTeam lusr zcon tid = do
  team <- E.getTeam tid
  when (isJust team) $ do
    Spar.deleteTeam tid
    now <- input
    convs <- E.getTeamConversations tid
    -- Even for LARGE TEAMS, we _DO_ want to fetch all team members here because we
    -- want to generate conversation deletion events for non-team users. This should
    -- be fine as it is done once during the life team of a team and we still do not
    -- fanout this particular event to all team members anyway. And this is anyway
    -- done asynchronously
    membs <- E.getTeamMembers tid
    (ue, be) <- foldrM (createConvDeleteEvents now membs) ([], []) convs
    let e = newEvent tid now EdTeamDelete
    pushDeleteEvents membs e ue
    E.deliverAsync be
    -- TODO: we don't delete bots here, but we should do that, since
    -- every bot user can only be in a single conversation. Just
    -- deleting conversations from the database is not enough.
    when ((view teamBinding . tdTeam <$> team) == Just Binding) $ do
      mapM_ (E.deleteUser . view userId) membs
      Journal.teamDelete tid
    Data.unsetTeamLegalholdWhitelisted tid
    E.deleteTeam tid
  where
    pushDeleteEvents :: [TeamMember] -> Event -> [Push] -> Sem r ()
    pushDeleteEvents membs e ue = do
      o <- inputs (view optSettings)
      let r = list1 (userRecipient (tUnqualified lusr)) (membersToRecipients (Just (tUnqualified lusr)) membs)
      -- To avoid DoS on gundeck, send team deletion events in chunks
      let chunkSize = fromMaybe defConcurrentDeletionEvents (o ^. setConcurrentDeletionEvents)
      let chunks = List.chunksOf chunkSize (toList r)
      forM_ chunks $ \case
        [] -> pure ()
        -- push TeamDelete events. Note that despite having a complete list, we are guaranteed in the
        -- push module to never fan this out to more than the limit
        x : xs -> E.push1 (newPushLocal1 ListComplete (tUnqualified lusr) (TeamEvent e) (list1 x xs) & pushConn .~ zcon)
      -- To avoid DoS on gundeck, send conversation deletion events slowly
      E.pushSlowly ue
    createConvDeleteEvents ::
      UTCTime ->
      [TeamMember] ->
      TeamConversation ->
      ([Push], [(BotMember, Conv.Event)]) ->
      Sem r ([Push], [(BotMember, Conv.Event)])
    createConvDeleteEvents now teamMembs c (pp, ee) = do
      let qconvId = tUntagged $ qualifyAs lusr (c ^. conversationId)
      (bots, convMembs) <- localBotsAndUsers <$> E.getLocalMembers (c ^. conversationId)
      -- Only nonTeamMembers need to get any events, since on team deletion,
      -- all team users are deleted immediately after these events are sent
      -- and will thus never be able to see these events in practice.
      let mm = nonTeamMembers convMembs teamMembs
      let e = Conv.Event qconvId Nothing (tUntagged lusr) now Conv.EdConvDelete
      -- This event always contains all the required recipients
      let p = newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (map recipient mm)
      let ee' = bots `zip` repeat e
      let pp' = maybe pp (\x -> (x & pushConn .~ zcon) : pp) p
      pure (pp', ee' ++ ee)

getTeamConversationRoles ::
  ( Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r
  ) =>
  UserId ->
  TeamId ->
  Sem r Public.ConversationRolesList
getTeamConversationRoles zusr tid = do
  void $ E.getTeamMember tid zusr >>= noteS @'NotATeamMember
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ Public.ConversationRolesList wireConvRoles

getTeamMembers ::
  ( Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r,
    Member (TeamMemberStore CassandraPaging) r
  ) =>
  Local UserId ->
  TeamId ->
  Maybe (Range 1 Public.HardTruncationLimit Int32) ->
  Maybe TeamMembersPagingState ->
  Sem r TeamMembersPage
getTeamMembers lzusr tid mbMaxResults mbPagingState = do
  member <- E.getTeamMember tid (tUnqualified lzusr) >>= noteS @'NotATeamMember
  let mState = C.PagingState . LBS.fromStrict <$> (mbPagingState >>= mtpsState)
  let mLimit = fromMaybe (unsafeRange Public.hardTruncationLimit) mbMaxResults
  E.listTeamMembers @CassandraPaging tid mState mLimit <&> toTeamMembersPage member
  where
    toTeamMembersPage :: TeamMember -> C.PageWithState TeamMember -> TeamMembersPage
    toTeamMembersPage member p =
      let withPerms = (member `canSeePermsOf`)
       in TeamMembersPage $
            MultiTablePage
              (map (setOptionalPerms withPerms) $ pwsResults p)
              (pwsHasMore p)
              (teamMemberPagingState p)

outputToStreamingBody :: Member (Final IO) r => Sem (Output LByteString ': r) () -> Sem r StreamingBody
outputToStreamingBody action = withWeavingToFinal @IO $ \state weave _inspect ->
  pure . (<$ state) $ \write flush -> do
    let writeChunk c = embedFinal $ do
          write (lazyByteString c)
          flush
    void . weave . (<$ state) $ runOutputSem writeChunk action

getTeamMembersCSV ::
  ( Member BrigAccess r,
    Member (ErrorS 'AccessDenied) r,
    Member (TeamMemberStore InternalPaging) r,
    Member TeamStore r,
    Member (Final IO) r,
    Member SparAccess r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r StreamingBody
getTeamMembersCSV lusr tid = do
  E.getTeamMember tid (tUnqualified lusr) >>= \case
    Nothing -> throwS @'AccessDenied
    Just member -> unless (member `hasPermission` DownloadTeamMembersCsv) $ throwS @'AccessDenied

  -- In case an exception is thrown inside the StreamingBody of responseStream
  -- the response will not contain a correct error message, but rather be an
  -- http error such as 'InvalidChunkHeaders'. The exception however still
  -- reaches the middleware and is being tracked in logging and metrics.
  outputToStreamingBody $ do
    output headerLine
    E.withChunks (\mps -> E.listTeamMembers @InternalPaging tid mps maxBound) $
      \members -> do
        let uids = fmap (view userId) members
        teamExportUser <-
          mkTeamExportUser
            <$> (lookupUser <$> E.lookupActivatedUsers uids)
            <*> lookupInviterHandle members
            <*> (lookupRichInfo <$> E.getRichInfoMultiUser uids)
            <*> (lookupClients <$> E.lookupClients uids)
            <*> (lookupScimUserInfo <$> Spar.lookupScimUserInfos uids)
        output @LByteString
          ( encodeDefaultOrderedByNameWith
              defaultEncodeOptions
              (mapMaybe teamExportUser members)
          )
  where
    headerLine :: LByteString
    headerLine = encodeDefaultOrderedByNameWith (defaultEncodeOptions {encIncludeHeader = True}) ([] :: [TeamExportUser])

    defaultEncodeOptions :: EncodeOptions
    defaultEncodeOptions =
      EncodeOptions
        { encDelimiter = fromIntegral (ord ','),
          encUseCrLf = True, -- to be compatible with Mac and Windows
          encIncludeHeader = False, -- (so we can flush when the header is on the wire)
          encQuoting = QuoteAll
        }

    mkTeamExportUser ::
      (UserId -> Maybe User) ->
      (UserId -> Maybe Handle.Handle) ->
      (UserId -> Maybe RichInfo) ->
      (UserId -> Int) ->
      (UserId -> Maybe ScimUserInfo) ->
      TeamMember ->
      Maybe TeamExportUser
    mkTeamExportUser users inviters richInfos numClients scimUserInfo member = do
      let uid = member ^. userId
      user <- users uid
      pure $
        TeamExportUser
          { tExportDisplayName = U.userDisplayName user,
            tExportHandle = U.userHandle user,
            tExportEmail = U.userIdentity user >>= U.emailIdentity,
            tExportRole = permissionsRole . view permissions $ member,
            tExportCreatedOn = maybe (scimUserInfo uid >>= suiCreatedOn) (Just . snd) (view invitation member),
            tExportInvitedBy = inviters . fst =<< member ^. invitation,
            tExportIdpIssuer = userToIdPIssuer user,
            tExportManagedBy = U.userManagedBy user,
            tExportSAMLNamedId = fromMaybe "" (samlNamedId user),
            tExportSCIMExternalId = fromMaybe "" (userSCIMExternalId user),
            tExportSCIMRichInfo = richInfos uid,
            tExportUserId = U.userId user,
            tExportNumDevices = numClients uid
          }

    lookupInviterHandle :: Member BrigAccess r => [TeamMember] -> Sem r (UserId -> Maybe Handle.Handle)
    lookupInviterHandle members = do
      let inviterIds :: [UserId]
          inviterIds = nub $ mapMaybe (fmap fst . view invitation) members

      userList :: [User] <- accountUser <$$> E.getUsers inviterIds

      let userMap :: M.Map UserId Handle.Handle
          userMap = M.fromList (mapMaybe extract userList)
            where
              extract u = (U.userId u,) <$> U.userHandle u

      pure (`M.lookup` userMap)

    userToIdPIssuer :: U.User -> Maybe HttpsUrl
    userToIdPIssuer usr = case (U.userIdentity >=> U.ssoIdentity) usr of
      Just (U.UserSSOId (SAML.UserRef issuer _)) -> either (const Nothing) Just . mkHttpsUrl $ issuer ^. SAML.fromIssuer
      Just _ -> Nothing
      Nothing -> Nothing

    lookupScimUserInfo :: [ScimUserInfo] -> (UserId -> Maybe ScimUserInfo)
    lookupScimUserInfo infos = (`M.lookup` M.fromList (infos <&> (\sui -> (suiUserId sui, sui))))

    lookupUser :: [U.User] -> (UserId -> Maybe U.User)
    lookupUser users = (`M.lookup` M.fromList (users <&> \user -> (U.userId user, user)))

    lookupRichInfo :: [(UserId, RichInfo)] -> (UserId -> Maybe RichInfo)
    lookupRichInfo pairs = (`M.lookup` M.fromList pairs)

    lookupClients :: Conv.UserClients -> UserId -> Int
    lookupClients userClients uid = maybe 0 length (M.lookup uid (Conv.userClients userClients))

    samlNamedId :: User -> Maybe Text
    samlNamedId =
      userSSOId >=> \case
        (UserSSOId (SAML.UserRef _idp nameId)) -> Just . CI.original . SAML.unsafeShowNameID $ nameId
        (UserScimExternalId _) -> Nothing

-- | like 'getTeamMembers', but with an explicit list of users we are to return.
bulkGetTeamMembers ::
  ( Member (ErrorS 'BulkGetMemberLimitExceeded) r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  Maybe (Range 1 HardTruncationLimit Int32) ->
  UserIdList ->
  Sem r TeamMemberListOptPerms
bulkGetTeamMembers lzusr tid mbMaxResults uids = do
  unless (length (U.mUsers uids) <= fromIntegral (fromRange (fromMaybe (unsafeRange Public.hardTruncationLimit) mbMaxResults))) $
    throwS @'BulkGetMemberLimitExceeded
  m <- E.getTeamMember tid (tUnqualified lzusr) >>= noteS @'NotATeamMember
  mems <- E.selectTeamMembers tid (U.mUsers uids)
  let withPerms = (m `canSeePermsOf`)
      hasMore = ListComplete
  pure $ setOptionalPermsMany withPerms (newTeamMemberList mems hasMore)

getTeamMember ::
  ( Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Sem r TeamMemberOptPerms
getTeamMember lzusr tid uid = do
  m <-
    E.getTeamMember tid (tUnqualified lzusr)
      >>= noteS @'NotATeamMember
  let withPerms = (m `canSeePermsOf`)
  member <- E.getTeamMember tid uid >>= noteS @'TeamMemberNotFound
  pure $ setOptionalPerms withPerms member

uncheckedGetTeamMember ::
  ( Member (ErrorS 'TeamMemberNotFound) r,
    Member TeamStore r
  ) =>
  TeamId ->
  UserId ->
  Sem r TeamMember
uncheckedGetTeamMember tid uid =
  E.getTeamMember tid uid >>= noteS @'TeamMemberNotFound

uncheckedGetTeamMembersH ::
  Member TeamStore r =>
  TeamId ->
  Maybe (Range 1 HardTruncationLimit Int32) ->
  Sem r TeamMemberList
uncheckedGetTeamMembersH tid mMaxResults =
  uncheckedGetTeamMembers tid (fromMaybe (unsafeRange hardTruncationLimit) mMaxResults)

uncheckedGetTeamMembers ::
  Member TeamStore r =>
  TeamId ->
  Range 1 HardTruncationLimit Int32 ->
  Sem r TeamMemberList
uncheckedGetTeamMembers = E.getTeamMembersWithLimit

addTeamMember ::
  forall db r.
  ( Member BrigAccess r,
    Member GundeckAccess r,
    Member (ErrorS 'InvalidPermissions) r,
    Member (ErrorS 'NoAddToBinding) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'TooManyTeamMembers) r,
    Member (ErrorS 'UserBindingExists) r,
    Member (ErrorS 'TooManyTeamMembersOnTeamWithLegalhold) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member (TeamFeatureStore db) r,
    Member TeamNotificationStore r,
    Member TeamStore r,
    Member P.TinyLog r,
    FeaturePersistentConstraint db LegalholdConfig
  ) =>
  Local UserId ->
  ConnId ->
  TeamId ->
  Public.NewTeamMember ->
  Sem r ()
addTeamMember lzusr zcon tid nmem = do
  let zusr = tUnqualified lzusr
  let uid = nmem ^. nUserId
  P.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "Teams.addTeamMember")
  -- verify permissions
  zusrMembership <-
    E.getTeamMember tid zusr
      >>= permissionCheck AddTeamMember
  let targetPermissions = nmem ^. nPermissions
  targetPermissions `ensureNotElevated` zusrMembership
  ensureNonBindingTeam tid
  ensureUnboundUsers [uid]
  ensureConnectedToLocals zusr [uid]
  (TeamSize sizeBeforeJoin) <- E.getSize tid
  ensureNotTooLargeForLegalHold @db tid (fromIntegral sizeBeforeJoin + 1)
  memList <- getTeamMembersForFanout tid
  void $ addTeamMemberInternal tid (Just zusr) (Just zcon) nmem memList

-- This function is "unchecked" because there is no need to check for user binding (invite only).
uncheckedAddTeamMember ::
  forall db r.
  ( Member BrigAccess r,
    Member GundeckAccess r,
    Member (ErrorS 'TooManyTeamMembers) r,
    Member (ErrorS 'TooManyTeamMembersOnTeamWithLegalhold) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member P.TinyLog r,
    Member (TeamFeatureStore db) r,
    Member TeamNotificationStore r,
    Member TeamStore r,
    FeaturePersistentConstraint db LegalholdConfig
  ) =>
  TeamId ->
  NewTeamMember ->
  Sem r ()
uncheckedAddTeamMember tid nmem = do
  mems <- getTeamMembersForFanout tid
  (TeamSize sizeBeforeJoin) <- E.getSize tid
  ensureNotTooLargeForLegalHold @db tid (fromIntegral sizeBeforeJoin + 1)
  (TeamSize sizeBeforeAdd) <- addTeamMemberInternal tid Nothing Nothing nmem mems
  billingUserIds <- Journal.getBillingUserIds tid $ Just $ newTeamMemberList (ntmNewTeamMember nmem : mems ^. teamMembers) (mems ^. teamMemberListType)
  Journal.teamUpdate tid (sizeBeforeAdd + 1) billingUserIds

uncheckedUpdateTeamMember ::
  forall r.
  ( Member BrigAccess r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member GundeckAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member P.TinyLog r,
    Member TeamStore r
  ) =>
  Maybe (Local UserId) ->
  Maybe ConnId ->
  TeamId ->
  NewTeamMember ->
  Sem r ()
uncheckedUpdateTeamMember mlzusr mZcon tid newMember = do
  let mZusr = tUnqualified <$> mlzusr
  let targetMember = ntmNewTeamMember newMember
  let targetId = targetMember ^. userId
      targetPermissions = targetMember ^. permissions
  P.debug $
    Log.field "targets" (toByteString targetId)
      . Log.field "action" (Log.val "Teams.updateTeamMember")

  team <- fmap tdTeam $ E.getTeam tid >>= noteS @'TeamNotFound

  previousMember <-
    E.getTeamMember tid targetId >>= noteS @'TeamMemberNotFound

  -- update target in Cassandra
  E.setTeamMemberPermissions (previousMember ^. permissions) tid targetId targetPermissions

  updatedMembers <- getTeamMembersForFanout tid
  updateJournal team updatedMembers
  updatePeers mZusr targetId targetMember targetPermissions updatedMembers
  where
    updateJournal :: Team -> TeamMemberList -> Sem r ()
    updateJournal team mems = do
      when (team ^. teamBinding == Binding) $ do
        (TeamSize size) <- E.getSize tid
        billingUserIds <- Journal.getBillingUserIds tid $ Just mems
        Journal.teamUpdate tid size billingUserIds

    updatePeers :: Maybe UserId -> UserId -> TeamMember -> Permissions -> TeamMemberList -> Sem r ()
    updatePeers zusr targetId targetMember targetPermissions updatedMembers = do
      -- inform members of the team about the change
      -- some (privileged) users will be informed about which change was applied
      let privileged = filter (`canSeePermsOf` targetMember) (updatedMembers ^. teamMembers)
          mkUpdate = EdMemberUpdate targetId
          privilegedUpdate = mkUpdate $ Just targetPermissions
          privilegedRecipients = membersToRecipients Nothing privileged
      now <- input
      let ePriv = newEvent tid now privilegedUpdate
      -- push to all members (user is privileged)
      let pushPriv = newPush (updatedMembers ^. teamMemberListType) zusr (TeamEvent ePriv) $ privilegedRecipients
      for_ pushPriv (\p -> E.push1 (p & pushConn .~ mZcon))

updateTeamMember ::
  forall r.
  ( Member BrigAccess r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'InvalidPermissions) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member GundeckAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member P.TinyLog r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  TeamId ->
  NewTeamMember ->
  Sem r ()
updateTeamMember lzusr zcon tid newMember = do
  let zusr = tUnqualified lzusr
  let targetMember = ntmNewTeamMember newMember
  let targetId = targetMember ^. userId
      targetPermissions = targetMember ^. permissions
  P.debug $
    Log.field "targets" (toByteString targetId)
      . Log.field "action" (Log.val "Teams.updateTeamMember")

  -- get the team and verify permissions
  user <-
    E.getTeamMember tid zusr
      >>= permissionCheck SetMemberPermissions

  -- user may not elevate permissions
  targetPermissions `ensureNotElevated` user
  previousMember <-
    E.getTeamMember tid targetId >>= noteS @'TeamMemberNotFound
  when
    ( downgradesOwner previousMember targetPermissions
        && not (canDowngradeOwner user previousMember)
    )
    $ throwS @'AccessDenied

  uncheckedUpdateTeamMember (Just lzusr) (Just zcon) tid newMember
  where
    canDowngradeOwner = canDeleteMember

    downgradesOwner :: TeamMember -> Permissions -> Bool
    downgradesOwner previousMember targetPermissions =
      permissionsRole (previousMember ^. permissions) == Just RoleOwner
        && permissionsRole targetPermissions /= Just RoleOwner

deleteTeamMember ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error AuthenticationError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member GundeckAccess r,
    Member MemberStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  TeamId ->
  UserId ->
  Public.TeamMemberDeleteData ->
  Sem r TeamMemberDeleteResult
deleteTeamMember lusr zcon tid remove body = deleteTeamMember' lusr zcon tid remove (Just body)

deleteNonBindingTeamMember ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error AuthenticationError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member GundeckAccess r,
    Member MemberStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  TeamId ->
  UserId ->
  Sem r TeamMemberDeleteResult
deleteNonBindingTeamMember lusr zcon tid remove = deleteTeamMember' lusr zcon tid remove Nothing

-- | 'TeamMemberDeleteData' is only required for binding teams
deleteTeamMember' ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error AuthenticationError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member GundeckAccess r,
    Member MemberStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  TeamId ->
  UserId ->
  Maybe Public.TeamMemberDeleteData ->
  Sem r TeamMemberDeleteResult
deleteTeamMember' lusr zcon tid remove mBody = do
  P.debug $
    Log.field "targets" (toByteString remove)
      . Log.field "action" (Log.val "Teams.deleteTeamMember")
  zusrMember <- E.getTeamMember tid (tUnqualified lusr)
  targetMember <- E.getTeamMember tid remove
  void $ permissionCheck RemoveTeamMember zusrMember
  do
    dm <- noteS @'NotATeamMember zusrMember
    tm <- noteS @'TeamMemberNotFound targetMember
    unless (canDeleteMember dm tm) $ throwS @'AccessDenied
  team <- fmap tdTeam $ E.getTeam tid >>= noteS @'TeamNotFound
  mems <- getTeamMembersForFanout tid
  if team ^. teamBinding == Binding && isJust targetMember
    then do
      body <- mBody & note (InvalidPayload "missing request body")
      ensureReAuthorised (tUnqualified lusr) (body ^. tmdAuthPassword) Nothing Nothing
      (TeamSize sizeBeforeDelete) <- E.getSize tid
      -- TeamSize is 'Natural' and subtracting from  0 is an error
      -- TeamSize could be reported as 0 if team members are added and removed very quickly,
      -- which happens in tests
      let sizeAfterDelete =
            if sizeBeforeDelete == 0
              then 0
              else sizeBeforeDelete - 1
      E.deleteUser remove
      billingUsers <- Journal.getBillingUserIds tid (Just mems)
      Journal.teamUpdate tid sizeAfterDelete $ filter (/= remove) billingUsers
      pure TeamMemberDeleteAccepted
    else do
      uncheckedDeleteTeamMember lusr (Just zcon) tid remove mems
      pure TeamMemberDeleteCompleted

-- This function is "unchecked" because it does not validate that the user has the `RemoveTeamMember` permission.
uncheckedDeleteTeamMember ::
  forall r.
  ( Member ConversationStore r,
    Member GundeckAccess r,
    Member ExternalAccess r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  TeamId ->
  UserId ->
  TeamMemberList ->
  Sem r ()
uncheckedDeleteTeamMember lusr zcon tid remove mems = do
  now <- input
  pushMemberLeaveEvent now
  E.deleteTeamMember tid remove
  removeFromConvsAndPushConvLeaveEvent now
  where
    -- notify all team members.
    pushMemberLeaveEvent :: UTCTime -> Sem r ()
    pushMemberLeaveEvent now = do
      let e = newEvent tid now (EdMemberLeave remove)
      let r =
            list1
              (userRecipient (tUnqualified lusr))
              (membersToRecipients (Just (tUnqualified lusr)) (mems ^. teamMembers))
      E.push1 $
        newPushLocal1 (mems ^. teamMemberListType) (tUnqualified lusr) (TeamEvent e) r & pushConn .~ zcon
    -- notify all conversation members not in this team.
    removeFromConvsAndPushConvLeaveEvent :: UTCTime -> Sem r ()
    removeFromConvsAndPushConvLeaveEvent now = do
      -- This may not make sense if that list has been truncated. In such cases, we still want to
      -- remove the user from conversations but never send out any events. We assume that clients
      -- handle nicely these missing events, regardless of whether they are in the same team or not
      let tmids = Set.fromList $ map (view userId) (mems ^. teamMembers)
      let edata = Conv.EdMembersLeave (Conv.QualifiedUserIdList [tUntagged (qualifyAs lusr remove)])
      cc <- E.getTeamConversations tid
      for_ cc $ \c ->
        E.getConversation (c ^. conversationId) >>= \conv ->
          for_ conv $ \dc -> when (remove `isMember` Data.convLocalMembers dc) $ do
            E.deleteMembers (c ^. conversationId) (UserList [remove] [])
            -- If the list was truncated, then the tmids list is incomplete so we simply drop these events
            unless (mems ^. teamMemberListType == ListTruncated) $
              pushEvent tmids edata now dc
    pushEvent :: Set UserId -> Conv.EventData -> UTCTime -> Data.Conversation -> Sem r ()
    pushEvent exceptTo edata now dc = do
      let qconvId = tUntagged $ qualifyAs lusr (Data.convId dc)
      let (bots, users) = localBotsAndUsers (Data.convLocalMembers dc)
      let x = filter (\m -> not (Conv.lmId m `Set.member` exceptTo)) users
      let y = Conv.Event qconvId Nothing (tUntagged lusr) now edata
      for_ (newPushLocal (mems ^. teamMemberListType) (tUnqualified lusr) (ConvEvent y) (recipient <$> x)) $ \p ->
        E.push1 $ p & pushConn .~ zcon
      E.deliverAsync (bots `zip` repeat y)

getTeamConversations ::
  ( Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member TeamStore r
  ) =>
  UserId ->
  TeamId ->
  Sem r Public.TeamConversationList
getTeamConversations zusr tid = do
  tm <-
    E.getTeamMember tid zusr
      >>= noteS @'NotATeamMember
  unless (tm `hasPermission` GetTeamConversations) $
    throwS @OperationDenied
  Public.newTeamConversationList <$> E.getTeamConversations tid

getTeamConversation ::
  ( Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member TeamStore r
  ) =>
  UserId ->
  TeamId ->
  ConvId ->
  Sem r Public.TeamConversation
getTeamConversation zusr tid cid = do
  tm <-
    E.getTeamMember tid zusr
      >>= noteS @'NotATeamMember
  unless (tm `hasPermission` GetTeamConversations) $
    throwS @OperationDenied
  E.getTeamConversation tid cid
    >>= noteS @'ConvNotFound

deleteTeamConversation ::
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS ('ActionDenied 'DeleteConversation)) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member (P.Logger (Msg -> Msg)) r
  ) =>
  Local UserId ->
  ConnId ->
  TeamId ->
  ConvId ->
  Sem r ()
deleteTeamConversation lusr zcon _tid cid = do
  let lconv = qualifyAs lusr cid
  void $ API.deleteLocalConversation lusr zcon lconv

getSearchVisibility ::
  ( Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member SearchVisibilityStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r TeamSearchVisibilityView
getSearchVisibility luid tid = do
  zusrMembership <- E.getTeamMember tid (tUnqualified luid)
  void $ permissionCheck ViewTeamSearchVisibility zusrMembership
  getSearchVisibilityInternal tid

setSearchVisibility ::
  forall r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamSearchVisibilityNotEnabled) r,
    Member SearchVisibilityStore r,
    Member TeamStore r
  ) =>
  (TeamId -> Sem r Bool) ->
  Local UserId ->
  TeamId ->
  Public.TeamSearchVisibilityView ->
  Sem r ()
setSearchVisibility availableForTeam luid tid req = do
  zusrMembership <- E.getTeamMember tid (tUnqualified luid)
  void $ permissionCheck ChangeTeamSearchVisibility zusrMembership
  setSearchVisibilityInternal availableForTeam tid req

-- Internal -----------------------------------------------------------------

-- | Invoke the given continuation 'k' with a list of team IDs
-- which are looked up based on:
--
-- * just limited by size
-- * an (exclusive) starting point (team ID) and size
-- * a list of team IDs
--
-- The last case returns those team IDs which have an associated
-- user. Additionally 'k' is passed in a 'hasMore' indication (which is
-- always false if the third lookup-case is used).
--
-- FUTUREWORK: avoid CPS
withTeamIds ::
  (Member TeamStore r, Member (ListItems LegacyPaging TeamId) r) =>
  UserId ->
  Maybe (Either (Range 1 32 (List TeamId)) TeamId) ->
  Range 1 100 Int32 ->
  (Bool -> [TeamId] -> Sem r a) ->
  Sem r a
withTeamIds usr range size k = case range of
  Nothing -> do
    r <- E.listItems usr Nothing (rcast size)
    k (resultSetType r == ResultSetTruncated) (resultSetResult r)
  Just (Right c) -> do
    r <- E.listItems usr (Just c) (rcast size)
    k (resultSetType r == ResultSetTruncated) (resultSetResult r)
  Just (Left (fromRange -> cc)) -> do
    ids <- E.selectTeams usr (Data.ByteString.Conversion.fromList cc)
    k False ids
{-# INLINE withTeamIds #-}

ensureUnboundUsers ::
  ( Member (ErrorS 'UserBindingExists) r,
    Member TeamStore r
  ) =>
  [UserId] ->
  Sem r ()
ensureUnboundUsers uids = do
  -- We check only 1 team because, by definition, users in binding teams
  -- can only be part of one team.
  teams <- Map.elems <$> E.getUsersTeams uids
  binds <- E.getTeamsBindings teams
  when (Binding `elem` binds) $
    throwS @'UserBindingExists

ensureNonBindingTeam ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NoAddToBinding) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r ()
ensureNonBindingTeam tid = do
  team <- noteS @'TeamNotFound =<< E.getTeam tid
  when (tdTeam team ^. teamBinding == Binding) $
    throwS @'NoAddToBinding

-- ensure that the permissions are not "greater" than the user's copy permissions
-- this is used to ensure users cannot "elevate" permissions
ensureNotElevated :: Member (ErrorS 'InvalidPermissions) r => Permissions -> TeamMember -> Sem r ()
ensureNotElevated targetPermissions member =
  unless
    ( (targetPermissions ^. self)
        `Set.isSubsetOf` (member ^. permissions . copy)
    )
    $ throwS @'InvalidPermissions

ensureNotTooLarge ::
  ( Member BrigAccess r,
    Member (ErrorS 'TooManyTeamMembers) r,
    Member (Input Opts) r
  ) =>
  TeamId ->
  Sem r TeamSize
ensureNotTooLarge tid = do
  o <- input
  (TeamSize size) <- E.getSize tid
  unless (size < fromIntegral (o ^. optSettings . setMaxTeamSize)) $
    throwS @'TooManyTeamMembers
  pure $ TeamSize size

-- | Ensure that a team doesn't exceed the member count limit for the LegalHold
-- feature. A team with more members than the fanout limit is too large, because
-- the fanout limit would prevent turning LegalHold feature _off_ again (for
-- details see 'Galley.API.LegalHold.removeSettings').
--
-- If LegalHold is configured for whitelisted teams only we consider the team
-- size unlimited, because we make the assumption that these teams won't turn
-- LegalHold off after activation.
--  FUTUREWORK: Find a way around the fanout limit.
ensureNotTooLargeForLegalHold ::
  forall db r.
  ( Member LegalHoldStore r,
    Member TeamStore r,
    Member (TeamFeatureStore db) r,
    Member (ErrorS 'TooManyTeamMembersOnTeamWithLegalhold) r,
    FeaturePersistentConstraint db LegalholdConfig
  ) =>
  TeamId ->
  Int ->
  Sem r ()
ensureNotTooLargeForLegalHold tid teamSize =
  whenM (isLegalHoldEnabledForTeam @db tid) $
    unlessM (teamSizeBelowLimit teamSize) $
      throwS @'TooManyTeamMembersOnTeamWithLegalhold

ensureNotTooLargeToActivateLegalHold ::
  ( Member BrigAccess r,
    Member (ErrorS 'CannotEnableLegalHoldServiceLargeTeam) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r ()
ensureNotTooLargeToActivateLegalHold tid = do
  (TeamSize teamSize) <- E.getSize tid
  unlessM (teamSizeBelowLimit (fromIntegral teamSize)) $
    throwS @'CannotEnableLegalHoldServiceLargeTeam

teamSizeBelowLimit :: Member TeamStore r => Int -> Sem r Bool
teamSizeBelowLimit teamSize = do
  limit <- fromIntegral . fromRange <$> E.fanoutLimit
  let withinLimit = teamSize <= limit
  E.getLegalHoldFlag >>= \case
    FeatureLegalHoldDisabledPermanently -> pure withinLimit
    FeatureLegalHoldDisabledByDefault -> pure withinLimit
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      -- unlimited, see docs of 'ensureNotTooLargeForLegalHold'
      pure True

addTeamMemberInternal ::
  ( Member BrigAccess r,
    Member (ErrorS 'TooManyTeamMembers) r,
    Member GundeckAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamNotificationStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  TeamId ->
  Maybe UserId ->
  Maybe ConnId ->
  NewTeamMember ->
  TeamMemberList ->
  Sem r TeamSize
addTeamMemberInternal tid origin originConn (ntmNewTeamMember -> new) memList = do
  P.debug $
    Log.field "targets" (toByteString (new ^. userId))
      . Log.field "action" (Log.val "Teams.addTeamMemberInternal")
  sizeBeforeAdd <- ensureNotTooLarge tid
  E.createTeamMember tid new
  now <- input
  let e = newEvent tid now (EdMemberJoin (new ^. userId))
  E.push1 $
    newPushLocal1 (memList ^. teamMemberListType) (new ^. userId) (TeamEvent e) (recipients origin new) & pushConn .~ originConn
  APITeamQueue.pushTeamEvent tid e
  pure sizeBeforeAdd
  where
    recipients (Just o) n =
      list1
        (userRecipient o)
        (membersToRecipients (Just o) (n : memList ^. teamMembers))
    recipients Nothing n =
      list1
        (userRecipient (n ^. userId))
        (membersToRecipients Nothing (memList ^. teamMembers))

finishCreateTeam ::
  ( Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Team ->
  TeamMember ->
  [TeamMember] ->
  Maybe ConnId ->
  Sem r ()
finishCreateTeam team owner others zcon = do
  let zusr = owner ^. userId
  for_ (owner : others) $
    E.createTeamMember (team ^. teamId)
  now <- input
  let e = newEvent (team ^. teamId) now (EdTeamCreate team)
  let r = membersToRecipients Nothing others
  E.push1 $ newPushLocal1 ListComplete zusr (TeamEvent e) (list1 (userRecipient zusr) r) & pushConn .~ zcon

getBindingTeamIdH ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member TeamStore r
  ) =>
  UserId ->
  Sem r Response
getBindingTeamIdH = fmap json . E.lookupBindingTeam

getBindingTeamMembersH ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member TeamStore r
  ) =>
  UserId ->
  Sem r Response
getBindingTeamMembersH = fmap json . getBindingTeamMembers

getBindingTeamMembers ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member TeamStore r
  ) =>
  UserId ->
  Sem r TeamMemberList
getBindingTeamMembers zusr = do
  tid <- E.lookupBindingTeam zusr
  getTeamMembersForFanout tid

-- This could be extended for more checks, for now we test only legalhold
--
-- Brig's `POST /register` endpoint throws the errors returned by this endpoint
-- verbatim.
--
-- FUTUREWORK: When this enpoint gets Servantified, it should have a more
-- precise list of errors, LegalHoldError is too wide, currently this can
-- actaully only error with TooManyTeamMembersOnTeamWithLegalhold. Once we have
-- a more precise list of errors and the endpoint is servantified, we can use
-- those to enrich 'Wire.API.User.RegisterError' and ensure that these errors
-- also show up in swagger. Currently, the error returned by this endpoint is
-- thrown in IO, we could then refactor that to be thrown in `ExceptT
-- RegisterError`.
canUserJoinTeam ::
  forall db r.
  ( Member BrigAccess r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member (TeamFeatureStore db) r,
    Member (ErrorS 'TooManyTeamMembersOnTeamWithLegalhold) r,
    FeaturePersistentConstraint db LegalholdConfig
  ) =>
  TeamId ->
  Sem r ()
canUserJoinTeam tid = do
  lhEnabled <- isLegalHoldEnabledForTeam @db tid
  when lhEnabled $ do
    (TeamSize sizeBeforeJoin) <- E.getSize tid
    ensureNotTooLargeForLegalHold @db tid (fromIntegral sizeBeforeJoin + 1)

-- | Modify and get visibility type for a team (internal, no user permission checks)
getSearchVisibilityInternal ::
  Member SearchVisibilityStore r =>
  TeamId ->
  Sem r TeamSearchVisibilityView
getSearchVisibilityInternal =
  fmap TeamSearchVisibilityView
    . SearchVisibilityData.getSearchVisibility

setSearchVisibilityInternal ::
  forall r.
  ( Member (ErrorS 'TeamSearchVisibilityNotEnabled) r,
    Member SearchVisibilityStore r
  ) =>
  (TeamId -> Sem r Bool) ->
  TeamId ->
  TeamSearchVisibilityView ->
  Sem r ()
setSearchVisibilityInternal availableForTeam tid (TeamSearchVisibilityView searchVisibility) = do
  unlessM (availableForTeam tid) $
    throwS @'TeamSearchVisibilityNotEnabled
  SearchVisibilityData.setSearchVisibility tid searchVisibility

userIsTeamOwner ::
  ( Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (Input (Local ())) r,
    Member TeamStore r
  ) =>
  TeamId ->
  UserId ->
  Sem r ()
userIsTeamOwner tid uid = do
  asking <- qualifyLocal uid
  mem <- getTeamMember asking tid uid
  unless (isTeamOwner mem) $ throwS @'AccessDenied

-- Queues a team for async deletion
queueTeamDeletion ::
  ( Member (ErrorS 'DeleteQueueFull) r,
    Member (Queue DeleteItem) r
  ) =>
  TeamId ->
  UserId ->
  Maybe ConnId ->
  Sem r ()
queueTeamDeletion tid zusr zcon = do
  ok <- E.tryPush (TeamItem tid zusr zcon)
  unless ok $ throwS @'DeleteQueueFull
