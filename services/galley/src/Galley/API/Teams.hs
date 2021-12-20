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

module Galley.API.Teams
  ( createBindingTeamH,
    createNonBindingTeamH,
    updateTeamH,
    updateTeamStatusH,
    getTeamH,
    getTeamInternalH,
    getTeamNameInternalH,
    getBindingTeamIdH,
    getBindingTeamMembersH,
    getManyTeamsH,
    deleteTeamH,
    uncheckedDeleteTeam,
    addTeamMemberH,
    getTeamNotificationsH,
    getTeamConversationRoles,
    getTeamMembersH,
    getTeamMembersCSVH,
    bulkGetTeamMembersH,
    getTeamMemberH,
    deleteTeamMemberH,
    updateTeamMemberH,
    getTeamConversations,
    getTeamConversation,
    deleteTeamConversation,
    getSearchVisibilityH,
    setSearchVisibilityH,
    getSearchVisibilityInternalH,
    setSearchVisibilityInternalH,
    uncheckedAddTeamMemberH,
    uncheckedGetTeamMemberH,
    uncheckedGetTeamMembersH,
    uncheckedDeleteTeamMember,
    userIsTeamOwnerH,
    canUserJoinTeamH,
    internalDeleteBindingTeamWithOneMemberH,
    internalDeleteBindingTeamWithOneMember,
    ensureNotTooLargeForLegalHold,
    ensureNotTooLargeToActivateLegalHold,
  )
where

import Brig.Types.Intra (accountUser)
import Brig.Types.Team (TeamSize (..))
import Control.Lens
import Data.ByteString.Conversion (List, toByteString)
import qualified Data.ByteString.Conversion
import Data.ByteString.Lazy.Builder (lazyByteString)
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
import Data.Qualified
import Data.Range as Range
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.Util as UUID
import Galley.API.Error as Galley
import Galley.API.LegalHold
import qualified Galley.API.Teams.Notifications as APITeamQueue
import qualified Galley.API.Update as API
import Galley.API.Util
import Galley.App
import Galley.Cassandra.Paging
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
import qualified Galley.Effects.Paging as E
import qualified Galley.Effects.Queue as E
import qualified Galley.Effects.SearchVisibilityStore as SearchVisibilityData
import qualified Galley.Effects.SparAccess as Spar
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import qualified Galley.Effects.TeamMemberStore as E
import qualified Galley.Effects.TeamStore as E
import Galley.Effects.WaiRoutes
import qualified Galley.Intra.Journal as Journal
import Galley.Intra.Push
import Galley.Options
import Galley.Types (UserIdList (UserIdList))
import qualified Galley.Types as Conv
import Galley.Types.Conversations.Roles as Roles
import Galley.Types.Teams hiding (newTeam)
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Galley.Types.UserList
import Imports hiding (forkIO)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error, or, result, setStatus)
import Network.Wai.Utilities hiding (Error)
import Polysemy
import Polysemy.Error
import Polysemy.Final
import Polysemy.Input
import Polysemy.Output
import qualified Polysemy.TinyLog as P
import qualified SAML2.WebSSO as SAML
import qualified System.Logger.Class as Log
import qualified Wire.API.Conversation.Role as Public
import Wire.API.ErrorDescription
import Wire.API.Federation.Error
import qualified Wire.API.Notification as Public
import qualified Wire.API.Team as Public
import qualified Wire.API.Team.Conversation as Public
import Wire.API.Team.Export (TeamExportUser (..))
import qualified Wire.API.Team.Feature as Public
import qualified Wire.API.Team.Member as Public
import qualified Wire.API.Team.SearchVisibility as Public
import Wire.API.User (User, UserSSOId (UserScimExternalId), userSCIMExternalId, userSSOId)
import qualified Wire.API.User as Public (UserIdList)
import qualified Wire.API.User as U
import Wire.API.User.Identity (UserSSOId (UserSSOId))
import Wire.API.User.RichInfo (RichInfo)

getTeamH ::
  Members '[Error TeamError, Queue DeleteItem, TeamStore] r =>
  UserId ::: TeamId ::: JSON ->
  Sem r Response
getTeamH (zusr ::: tid ::: _) =
  maybe (throw TeamNotFound) (pure . json) =<< lookupTeam zusr tid

getTeamInternalH ::
  Members '[Error TeamError, TeamStore] r =>
  TeamId ::: JSON ->
  Sem r Response
getTeamInternalH (tid ::: _) =
  fmap json $
    E.getTeam tid >>= note TeamNotFound

getTeamNameInternalH ::
  Members '[Error TeamError, TeamStore] r =>
  TeamId ::: JSON ->
  Sem r Response
getTeamNameInternalH (tid ::: _) =
  fmap json $
    getTeamNameInternal tid >>= note TeamNotFound

getTeamNameInternal :: Member TeamStore r => TeamId -> Sem r (Maybe TeamName)
getTeamNameInternal = fmap (fmap TeamName) . E.getTeamName

getManyTeamsH ::
  (Members '[TeamStore, Queue DeleteItem, ListItems LegacyPaging TeamId] r) =>
  UserId ::: Maybe (Either (Range 1 32 (List TeamId)) TeamId) ::: Range 1 100 Int32 ::: JSON ->
  Sem r Response
getManyTeamsH (zusr ::: range ::: size ::: _) =
  json <$> getManyTeams zusr range size

getManyTeams ::
  (Members '[TeamStore, Queue DeleteItem, ListItems LegacyPaging TeamId] r) =>
  UserId ->
  Maybe (Either (Range 1 32 (List TeamId)) TeamId) ->
  Range 1 100 Int32 ->
  Sem r Public.TeamList
getManyTeams zusr range size =
  withTeamIds zusr range size $ \more ids -> do
    teams <- mapM (lookupTeam zusr) ids
    pure (Public.newTeamList (catMaybes teams) more)

lookupTeam ::
  Members '[TeamStore, Queue DeleteItem] r =>
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
  Members
    '[ BrigAccess,
       Error ActionError,
       Error TeamError,
       GundeckAccess,
       Input UTCTime,
       P.TinyLog,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: JsonRequest Public.NonBindingNewTeam ::: JSON ->
  Sem r Response
createNonBindingTeamH (zusr ::: zcon ::: req ::: _) = do
  newTeam <- fromJsonBody req
  newTeamId <- createNonBindingTeam zusr zcon newTeam
  pure (empty & setStatus status201 . location newTeamId)

createNonBindingTeam ::
  Members
    '[ BrigAccess,
       Error ActionError,
       Error TeamError,
       GundeckAccess,
       Input UTCTime,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ->
  ConnId ->
  Public.NonBindingNewTeam ->
  Sem r TeamId
createNonBindingTeam zusr zcon (Public.NonBindingNewTeam body) = do
  let owner = Public.TeamMember zusr fullPermissions Nothing LH.defUserLegalHoldStatus
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

createBindingTeamH ::
  Members '[GundeckAccess, Input UTCTime, TeamStore, WaiRoutes] r =>
  UserId ::: TeamId ::: JsonRequest BindingNewTeam ::: JSON ->
  Sem r Response
createBindingTeamH (zusr ::: tid ::: req ::: _) = do
  newTeam <- fromJsonBody req
  newTeamId <- createBindingTeam zusr tid newTeam
  pure (empty & setStatus status201 . location newTeamId)

createBindingTeam ::
  Members '[GundeckAccess, Input UTCTime, TeamStore] r =>
  UserId ->
  TeamId ->
  BindingNewTeam ->
  Sem r TeamId
createBindingTeam zusr tid (BindingNewTeam body) = do
  let owner = Public.TeamMember zusr fullPermissions Nothing LH.defUserLegalHoldStatus
  team <-
    E.createTeam (Just tid) zusr (body ^. newTeamName) (body ^. newTeamIcon) (body ^. newTeamIconKey) Binding
  finishCreateTeam team owner [] Nothing
  pure tid

updateTeamStatusH ::
  Members
    '[ BrigAccess,
       Error ActionError,
       Error TeamError,
       Input Opts,
       Input UTCTime,
       P.TinyLog,
       TeamStore,
       WaiRoutes
     ]
    r =>
  TeamId ::: JsonRequest TeamStatusUpdate ::: JSON ->
  Sem r Response
updateTeamStatusH (tid ::: req ::: _) = do
  teamStatusUpdate <- fromJsonBody req
  updateTeamStatus tid teamStatusUpdate
  return empty

updateTeamStatus ::
  Members
    '[ BrigAccess,
       Error ActionError,
       Error TeamError,
       Input Opts,
       Input UTCTime,
       P.TinyLog,
       TeamStore
     ]
    r =>
  TeamId ->
  TeamStatusUpdate ->
  Sem r ()
updateTeamStatus tid (TeamStatusUpdate newStatus cur) = do
  oldStatus <- fmap tdStatus $ E.getTeam tid >>= note TeamNotFound
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
    journal _ _ = throw InvalidTeamStatusUpdate
    validateTransition :: Member (Error ActionError) r => (TeamStatus, TeamStatus) -> Sem r Bool
    validateTransition = \case
      (PendingActive, Active) -> return True
      (Active, Active) -> return False
      (Active, Suspended) -> return True
      (Suspended, Active) -> return True
      (Suspended, Suspended) -> return False
      (_, _) -> throw InvalidTeamStatusUpdate

updateTeamH ::
  Members
    '[ Error ActionError,
       Error NotATeamMember,
       GundeckAccess,
       Input UTCTime,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: TeamId ::: JsonRequest Public.TeamUpdateData ::: JSON ->
  Sem r Response
updateTeamH (zusr ::: zcon ::: tid ::: req ::: _) = do
  updateData <- fromJsonBody req
  updateTeam zusr zcon tid updateData
  pure empty

updateTeam ::
  Members
    '[ Error ActionError,
       Error NotATeamMember,
       GundeckAccess,
       Input UTCTime,
       TeamStore
     ]
    r =>
  UserId ->
  ConnId ->
  TeamId ->
  Public.TeamUpdateData ->
  Sem r ()
updateTeam zusr zcon tid updateData = do
  zusrMembership <- E.getTeamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "Teams.updateTeam")
  void $ permissionCheck SetTeamData zusrMembership
  E.setTeamData tid updateData
  now <- input
  memList <- getTeamMembersForFanout tid
  let e = newEvent TeamUpdate tid now & eventData .~ Just (EdTeamUpdate updateData)
  let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) (memList ^. teamMembers))
  E.push1 $ newPushLocal1 (memList ^. teamMemberListType) zusr (TeamEvent e) r & pushConn .~ Just zcon

deleteTeamH ::
  Members
    '[ BrigAccess,
       Error ActionError,
       Error AuthenticationError,
       Error InternalError,
       Error InvalidInput,
       Error TeamError,
       Error NotATeamMember,
       Queue DeleteItem,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: TeamId ::: OptionalJsonRequest Public.TeamDeleteData ::: JSON ->
  Sem r Response
deleteTeamH (zusr ::: zcon ::: tid ::: req ::: _) = do
  mBody <- fromOptionalJsonBody req
  deleteTeam zusr zcon tid mBody
  pure (empty & setStatus status202)

-- | 'TeamDeleteData' is only required for binding teams
deleteTeam ::
  Members
    '[ BrigAccess,
       Error ActionError,
       Error AuthenticationError,
       Error InternalError,
       Error InvalidInput,
       Error TeamError,
       Error NotATeamMember,
       Queue DeleteItem,
       TeamStore
     ]
    r =>
  UserId ->
  ConnId ->
  TeamId ->
  Maybe Public.TeamDeleteData ->
  Sem r ()
deleteTeam zusr zcon tid mBody = do
  team <- E.getTeam tid >>= note TeamNotFound
  case tdStatus team of
    Deleted -> throw TeamNotFound
    PendingDelete ->
      queueTeamDeletion tid zusr (Just zcon)
    _ -> do
      checkPermissions team
      queueTeamDeletion tid zusr (Just zcon)
  where
    checkPermissions team = do
      void $ permissionCheck DeleteTeam =<< E.getTeamMember tid zusr
      when ((tdTeam team) ^. teamBinding == Binding) $ do
        body <- mBody & note (InvalidPayload "missing request body")
        ensureReAuthorised zusr (body ^. tdAuthPassword)

-- This can be called by stern
internalDeleteBindingTeamWithOneMember ::
  Members
    '[ Error InternalError,
       Error TeamError,
       Queue DeleteItem,
       TeamStore
     ]
    r =>
  TeamId ->
  Sem r ()
internalDeleteBindingTeamWithOneMember tid = do
  team <- E.getTeam tid
  unless ((view teamBinding . tdTeam <$> team) == Just Binding) $
    throw NoBindingTeam
  mems <- E.getTeamMembersWithLimit tid (unsafeRange 2)
  case mems ^. teamMembers of
    (mem : []) -> queueTeamDeletion tid (mem ^. userId) Nothing
    _ -> throw NotAOneMemberTeam

-- This function is "unchecked" because it does not validate that the user has the `DeleteTeam` permission.
uncheckedDeleteTeam ::
  forall r.
  Members
    '[ BrigAccess,
       ExternalAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       SparAccess,
       TeamStore
     ]
    r =>
  Local UserId ->
  Maybe ConnId ->
  TeamId ->
  Sem r ()
uncheckedDeleteTeam lusr zcon tid = do
  team <- E.getTeam tid
  when (isJust team) $ do
    Spar.deleteTeam tid
    now <- input
    convs <-
      filter (not . view managedConversation) <$> E.getTeamConversations tid
    -- Even for LARGE TEAMS, we _DO_ want to fetch all team members here because we
    -- want to generate conversation deletion events for non-team users. This should
    -- be fine as it is done once during the life team of a team and we still do not
    -- fanout this particular event to all team members anyway. And this is anyway
    -- done asynchronously
    membs <- E.getTeamMembers tid
    (ue, be) <- foldrM (createConvDeleteEvents now membs) ([], []) convs
    let e = newEvent TeamDelete tid now
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
      forM_ chunks $ \chunk -> case chunk of
        [] -> return ()
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
      let qconvId = qUntagged $ qualifyAs lusr (c ^. conversationId)
      (bots, convMembs) <- localBotsAndUsers <$> E.getLocalMembers (c ^. conversationId)
      -- Only nonTeamMembers need to get any events, since on team deletion,
      -- all team users are deleted immediately after these events are sent
      -- and will thus never be able to see these events in practice.
      let mm = nonTeamMembers convMembs teamMembs
      let e = Conv.Event Conv.ConvDelete qconvId (qUntagged lusr) now Conv.EdConvDelete
      -- This event always contains all the required recipients
      let p = newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (map recipient mm)
      let ee' = bots `zip` repeat e
      let pp' = maybe pp (\x -> (x & pushConn .~ zcon) : pp) p
      pure (pp', ee' ++ ee)

getTeamConversationRoles ::
  Members '[Error NotATeamMember, TeamStore] r =>
  UserId ->
  TeamId ->
  Sem r Public.ConversationRolesList
getTeamConversationRoles zusr tid = do
  void $ E.getTeamMember tid zusr >>= noteED @NotATeamMember
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ Public.ConversationRolesList wireConvRoles

getTeamMembersH ::
  Members '[Error NotATeamMember, TeamStore] r =>
  UserId ::: TeamId ::: Range 1 Public.HardTruncationLimit Int32 ::: JSON ->
  Sem r Response
getTeamMembersH (zusr ::: tid ::: maxResults ::: _) = do
  (memberList, withPerms) <- getTeamMembers zusr tid maxResults
  pure . json $ teamMemberListJson withPerms memberList

getTeamMembers ::
  Members '[Error NotATeamMember, TeamStore] r =>
  UserId ->
  TeamId ->
  Range 1 Public.HardTruncationLimit Int32 ->
  Sem r (Public.TeamMemberList, Public.TeamMember -> Bool)
getTeamMembers zusr tid maxResults = do
  m <- E.getTeamMember tid zusr >>= noteED @NotATeamMember
  mems <- E.getTeamMembersWithLimit tid maxResults
  let withPerms = (m `canSeePermsOf`)
  pure (mems, withPerms)

outputToStreamingBody :: Member (Final IO) r => Sem (Output LByteString ': r) () -> Sem r StreamingBody
outputToStreamingBody action = withWeavingToFinal @IO $ \state weave _inspect ->
  pure . (<$ state) $ \write flush -> do
    let writeChunk c = embedFinal $ do
          write (lazyByteString c)
          flush
    void . weave . (<$ state) $ runOutputSem writeChunk action

getTeamMembersCSVH ::
  (Members '[BrigAccess, Error ActionError, TeamMemberStore InternalPaging, TeamStore, Final IO] r) =>
  UserId ::: TeamId ::: JSON ->
  Sem r Response
getTeamMembersCSVH (zusr ::: tid ::: _) = do
  E.getTeamMember tid zusr >>= \case
    Nothing -> throw AccessDenied
    Just member -> unless (member `hasPermission` DownloadTeamMembersCsv) $ throw AccessDenied

  -- In case an exception is thrown inside the StreamingBody of responseStream
  -- the response will not contain a correct error message, but rather be an
  -- http error such as 'InvalidChunkHeaders'. The exception however still
  -- reaches the middleware and is being tracked in logging and metrics.
  body <- outputToStreamingBody $ do
    output headerLine
    E.withChunks (\mps -> E.listTeamMembers @InternalPaging tid mps maxBound) $
      \members -> do
        inviters <- lookupInviterHandle members
        users <-
          lookupUser <$> E.lookupActivatedUsers (fmap (view userId) members)
        richInfos <-
          lookupRichInfo <$> E.getRichInfoMultiUser (fmap (view userId) members)
        output @LByteString
          ( encodeDefaultOrderedByNameWith
              defaultEncodeOptions
              (mapMaybe (teamExportUser users inviters richInfos) members)
          )
  pure $
    responseStream
      status200
      [ (hContentType, "text/csv"),
        ("Content-Disposition", "attachment; filename=\"wire_team_members.csv\"")
      ]
      body
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

    teamExportUser ::
      (UserId -> Maybe User) ->
      (UserId -> Maybe Handle.Handle) ->
      (UserId -> Maybe RichInfo) ->
      TeamMember ->
      Maybe TeamExportUser
    teamExportUser users inviters richInfos member = do
      let uid = member ^. userId
      user <- users uid
      pure $
        TeamExportUser
          { tExportDisplayName = U.userDisplayName user,
            tExportHandle = U.userHandle user,
            tExportEmail = U.userIdentity user >>= U.emailIdentity,
            tExportRole = permissionsRole . view permissions $ member,
            tExportCreatedOn = fmap snd . view invitation $ member,
            tExportInvitedBy = inviters . fst =<< member ^. invitation,
            tExportIdpIssuer = userToIdPIssuer user,
            tExportManagedBy = U.userManagedBy user,
            tExportSAMLNamedId = fromMaybe "" (samlNamedId user),
            tExportSCIMExternalId = fromMaybe "" (userSCIMExternalId user),
            tExportSCIMRichInfo = richInfos uid,
            tExportUserId = U.userId user
          }

    lookupInviterHandle :: Member BrigAccess r => [TeamMember] -> Sem r (UserId -> Maybe Handle.Handle)
    lookupInviterHandle members = do
      let inviterIds :: [UserId]
          inviterIds = nub $ catMaybes $ fmap fst . view invitation <$> members

      userList :: [User] <- accountUser <$$> E.getUsers inviterIds

      let userMap :: M.Map UserId Handle.Handle
          userMap = M.fromList . catMaybes $ extract <$> userList
            where
              extract u = (U.userId u,) <$> U.userHandle u

      pure (`M.lookup` userMap)

    userToIdPIssuer :: U.User -> Maybe HttpsUrl
    userToIdPIssuer usr = case (U.userIdentity >=> U.ssoIdentity) usr of
      Just (U.UserSSOId (SAML.UserRef issuer _)) -> either (const Nothing) Just . mkHttpsUrl $ issuer ^. SAML.fromIssuer
      Just _ -> Nothing
      Nothing -> Nothing

    lookupUser :: [U.User] -> (UserId -> Maybe U.User)
    lookupUser users = (`M.lookup` M.fromList (users <&> \user -> (U.userId user, user)))

    lookupRichInfo :: [(UserId, RichInfo)] -> (UserId -> Maybe RichInfo)
    lookupRichInfo pairs = (`M.lookup` M.fromList pairs)

    samlNamedId :: User -> Maybe Text
    samlNamedId =
      userSSOId >=> \case
        (UserSSOId (SAML.UserRef _idp nameId)) -> Just . CI.original . SAML.unsafeShowNameID $ nameId
        (UserScimExternalId _) -> Nothing

bulkGetTeamMembersH ::
  Members
    '[ Error InvalidInput,
       Error NotATeamMember,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: TeamId ::: Range 1 Public.HardTruncationLimit Int32 ::: JsonRequest Public.UserIdList ::: JSON ->
  Sem r Response
bulkGetTeamMembersH (zusr ::: tid ::: maxResults ::: body ::: _) = do
  UserIdList uids <- fromJsonBody body
  (memberList, withPerms) <- bulkGetTeamMembers zusr tid maxResults uids
  pure . json $ teamMemberListJson withPerms memberList

-- | like 'getTeamMembers', but with an explicit list of users we are to return.
bulkGetTeamMembers ::
  Members '[Error InvalidInput, Error NotATeamMember, TeamStore] r =>
  UserId ->
  TeamId ->
  Range 1 HardTruncationLimit Int32 ->
  [UserId] ->
  Sem r (TeamMemberList, TeamMember -> Bool)
bulkGetTeamMembers zusr tid maxResults uids = do
  unless (length uids <= fromIntegral (fromRange maxResults)) $
    throw BulkGetMemberLimitExceeded
  m <- E.getTeamMember tid zusr >>= noteED @NotATeamMember
  mems <- E.selectTeamMembers tid uids
  let withPerms = (m `canSeePermsOf`)
      hasMore = ListComplete
  pure (newTeamMemberList mems hasMore, withPerms)

getTeamMemberH ::
  Members '[Error TeamError, Error NotATeamMember, TeamStore] r =>
  UserId ::: TeamId ::: UserId ::: JSON ->
  Sem r Response
getTeamMemberH (zusr ::: tid ::: uid ::: _) = do
  (member, withPerms) <- getTeamMember zusr tid uid
  pure . json $ teamMemberJson withPerms member

getTeamMember ::
  Members '[Error TeamError, Error NotATeamMember, TeamStore] r =>
  UserId ->
  TeamId ->
  UserId ->
  Sem r (Public.TeamMember, Public.TeamMember -> Bool)
getTeamMember zusr tid uid = do
  m <-
    E.getTeamMember tid zusr
      >>= noteED @NotATeamMember
  let withPerms = (m `canSeePermsOf`)
  member <- E.getTeamMember tid uid >>= note TeamMemberNotFound
  pure (member, withPerms)

internalDeleteBindingTeamWithOneMemberH ::
  Members
    '[ Error InternalError,
       Error TeamError,
       Queue DeleteItem,
       TeamStore
     ]
    r =>
  TeamId ->
  Sem r Response
internalDeleteBindingTeamWithOneMemberH tid = do
  internalDeleteBindingTeamWithOneMember tid
  pure (empty & setStatus status202)

uncheckedGetTeamMemberH ::
  Members '[Error TeamError, TeamStore] r =>
  TeamId ::: UserId ::: JSON ->
  Sem r Response
uncheckedGetTeamMemberH (tid ::: uid ::: _) = do
  json <$> uncheckedGetTeamMember tid uid

uncheckedGetTeamMember ::
  Members '[Error TeamError, TeamStore] r =>
  TeamId ->
  UserId ->
  Sem r TeamMember
uncheckedGetTeamMember tid uid =
  E.getTeamMember tid uid >>= note TeamMemberNotFound

uncheckedGetTeamMembersH ::
  Member TeamStore r =>
  TeamId ::: Range 1 HardTruncationLimit Int32 ::: JSON ->
  Sem r Response
uncheckedGetTeamMembersH (tid ::: maxResults ::: _) = do
  json <$> uncheckedGetTeamMembers tid maxResults

uncheckedGetTeamMembers ::
  Member TeamStore r =>
  TeamId ->
  Range 1 HardTruncationLimit Int32 ->
  Sem r TeamMemberList
uncheckedGetTeamMembers tid maxResults = E.getTeamMembersWithLimit tid maxResults

addTeamMemberH ::
  Members
    '[ BrigAccess,
       GundeckAccess,
       Error ActionError,
       Error LegalHoldError,
       Error TeamError,
       Error NotATeamMember,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       P.TinyLog,
       TeamFeatureStore,
       TeamNotificationStore,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: TeamId ::: JsonRequest Public.NewTeamMember ::: JSON ->
  Sem r Response
addTeamMemberH (zusr ::: zcon ::: tid ::: req ::: _) = do
  nmem <- fromJsonBody req
  addTeamMember zusr zcon tid nmem
  pure empty

addTeamMember ::
  Members
    '[ BrigAccess,
       GundeckAccess,
       Error ActionError,
       Error LegalHoldError,
       Error TeamError,
       Error NotATeamMember,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       TeamFeatureStore,
       TeamNotificationStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ->
  ConnId ->
  TeamId ->
  Public.NewTeamMember ->
  Sem r ()
addTeamMember zusr zcon tid nmem = do
  let uid = nmem ^. ntmNewTeamMember . userId
  P.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "Teams.addTeamMember")
  -- verify permissions
  zusrMembership <-
    E.getTeamMember tid zusr
      >>= permissionCheck AddTeamMember
  let targetPermissions = nmem ^. ntmNewTeamMember . permissions
  targetPermissions `ensureNotElevated` zusrMembership
  ensureNonBindingTeam tid
  ensureUnboundUsers [uid]
  ensureConnectedToLocals zusr [uid]
  (TeamSize sizeBeforeJoin) <- E.getSize tid
  ensureNotTooLargeForLegalHold tid (fromIntegral sizeBeforeJoin + 1)
  memList <- getTeamMembersForFanout tid
  void $ addTeamMemberInternal tid (Just zusr) (Just zcon) nmem memList

-- This function is "unchecked" because there is no need to check for user binding (invite only).
uncheckedAddTeamMemberH ::
  Members
    '[ BrigAccess,
       Error LegalHoldError,
       Error TeamError,
       GundeckAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       P.TinyLog,
       TeamFeatureStore,
       TeamNotificationStore,
       TeamStore,
       WaiRoutes
     ]
    r =>
  TeamId ::: JsonRequest NewTeamMember ::: JSON ->
  Sem r Response
uncheckedAddTeamMemberH (tid ::: req ::: _) = do
  nmem <- fromJsonBody req
  uncheckedAddTeamMember tid nmem
  return empty

uncheckedAddTeamMember ::
  Members
    '[ BrigAccess,
       GundeckAccess,
       Error LegalHoldError,
       Error TeamError,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       MemberStore,
       LegalHoldStore,
       P.TinyLog,
       TeamFeatureStore,
       TeamNotificationStore,
       TeamStore
     ]
    r =>
  TeamId ->
  NewTeamMember ->
  Sem r ()
uncheckedAddTeamMember tid nmem = do
  mems <- getTeamMembersForFanout tid
  (TeamSize sizeBeforeJoin) <- E.getSize tid
  ensureNotTooLargeForLegalHold tid (fromIntegral sizeBeforeJoin + 1)
  (TeamSize sizeBeforeAdd) <- addTeamMemberInternal tid Nothing Nothing nmem mems
  billingUserIds <- Journal.getBillingUserIds tid $ Just $ newTeamMemberList ((nmem ^. ntmNewTeamMember) : mems ^. teamMembers) (mems ^. teamMemberListType)
  Journal.teamUpdate tid (sizeBeforeAdd + 1) billingUserIds

updateTeamMemberH ::
  Members
    '[ BrigAccess,
       Error ActionError,
       Error TeamError,
       Error NotATeamMember,
       Input Opts,
       Input UTCTime,
       GundeckAccess,
       P.TinyLog,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: TeamId ::: JsonRequest Public.NewTeamMember ::: JSON ->
  Sem r Response
updateTeamMemberH (zusr ::: zcon ::: tid ::: req ::: _) = do
  -- the team member to be updated
  targetMember <- view ntmNewTeamMember <$> (fromJsonBody req)
  updateTeamMember zusr zcon tid targetMember
  pure empty

updateTeamMember ::
  forall r.
  Members
    '[ BrigAccess,
       Error ActionError,
       Error TeamError,
       Error NotATeamMember,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       P.TinyLog,
       TeamStore
     ]
    r =>
  UserId ->
  ConnId ->
  TeamId ->
  TeamMember ->
  Sem r ()
updateTeamMember zusr zcon tid targetMember = do
  let targetId = targetMember ^. userId
      targetPermissions = targetMember ^. permissions
  P.debug $
    Log.field "targets" (toByteString targetId)
      . Log.field "action" (Log.val "Teams.updateTeamMember")

  -- get the team and verify permissions
  team <- fmap tdTeam $ E.getTeam tid >>= note TeamNotFound
  user <-
    E.getTeamMember tid zusr
      >>= permissionCheck SetMemberPermissions

  -- user may not elevate permissions
  targetPermissions `ensureNotElevated` user
  previousMember <-
    E.getTeamMember tid targetId >>= note TeamMemberNotFound
  when
    ( downgradesOwner previousMember targetPermissions
        && not (canDowngradeOwner user previousMember)
    )
    $ throw AccessDenied

  -- update target in Cassandra
  E.setTeamMemberPermissions (previousMember ^. permissions) tid targetId targetPermissions

  updatedMembers <- getTeamMembersForFanout tid
  updateJournal team updatedMembers
  updatePeers targetId targetPermissions updatedMembers
  where
    canDowngradeOwner = canDeleteMember

    downgradesOwner :: TeamMember -> Permissions -> Bool
    downgradesOwner previousMember targetPermissions =
      permissionsRole (previousMember ^. permissions) == Just RoleOwner
        && permissionsRole targetPermissions /= Just RoleOwner

    updateJournal :: Team -> TeamMemberList -> Sem r ()
    updateJournal team mems = do
      when (team ^. teamBinding == Binding) $ do
        (TeamSize size) <- E.getSize tid
        billingUserIds <- Journal.getBillingUserIds tid $ Just mems
        Journal.teamUpdate tid size billingUserIds

    updatePeers :: UserId -> Permissions -> TeamMemberList -> Sem r ()
    updatePeers targetId targetPermissions updatedMembers = do
      -- inform members of the team about the change
      -- some (privileged) users will be informed about which change was applied
      let privileged = filter (`canSeePermsOf` targetMember) (updatedMembers ^. teamMembers)
          mkUpdate = EdMemberUpdate targetId
          privilegedUpdate = mkUpdate $ Just targetPermissions
          privilegedRecipients = membersToRecipients Nothing privileged
      now <- input
      let ePriv = newEvent MemberUpdate tid now & eventData ?~ privilegedUpdate
      -- push to all members (user is privileged)
      let pushPriv = newPushLocal (updatedMembers ^. teamMemberListType) zusr (TeamEvent ePriv) $ privilegedRecipients
      for_ pushPriv $ \p -> E.push1 $ p & pushConn .~ Just zcon

deleteTeamMemberH ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error ActionError,
       Error AuthenticationError,
       Error InvalidInput,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       GundeckAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       MemberStore,
       P.TinyLog,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: TeamId ::: UserId ::: OptionalJsonRequest Public.TeamMemberDeleteData ::: JSON ->
  Sem r Response
deleteTeamMemberH (zusr ::: zcon ::: tid ::: remove ::: req ::: _) = do
  lusr <- qualifyLocal zusr
  mBody <- fromOptionalJsonBody req
  deleteTeamMember lusr zcon tid remove mBody >>= \case
    TeamMemberDeleteAccepted -> pure (empty & setStatus status202)
    TeamMemberDeleteCompleted -> pure empty

data TeamMemberDeleteResult
  = TeamMemberDeleteAccepted
  | TeamMemberDeleteCompleted

-- | 'TeamMemberDeleteData' is only required for binding teams
deleteTeamMember ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error ActionError,
       Error AuthenticationError,
       Error InvalidInput,
       Error TeamError,
       Error NotATeamMember,
       ExternalAccess,
       Input Opts,
       Input UTCTime,
       GundeckAccess,
       MemberStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  TeamId ->
  UserId ->
  Maybe Public.TeamMemberDeleteData ->
  Sem r TeamMemberDeleteResult
deleteTeamMember lusr zcon tid remove mBody = do
  P.debug $
    Log.field "targets" (toByteString remove)
      . Log.field "action" (Log.val "Teams.deleteTeamMember")
  zusrMember <- E.getTeamMember tid (tUnqualified lusr)
  targetMember <- E.getTeamMember tid remove
  void $ permissionCheck RemoveTeamMember zusrMember
  do
    dm <- note TeamMemberNotFound zusrMember
    tm <- note TeamMemberNotFound targetMember
    unless (canDeleteMember dm tm) $ throw AccessDenied
  team <- fmap tdTeam $ E.getTeam tid >>= note TeamNotFound
  mems <- getTeamMembersForFanout tid
  if team ^. teamBinding == Binding && isJust targetMember
    then do
      body <- mBody & note (InvalidPayload "missing request body")
      ensureReAuthorised (tUnqualified lusr) (body ^. tmdAuthPassword)
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
  Members
    '[ ConversationStore,
       GundeckAccess,
       ExternalAccess,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
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
      let e = newEvent MemberLeave tid now & eventData ?~ EdMemberLeave remove
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
      let edata = Conv.EdMembersLeave (Conv.QualifiedUserIdList [qUntagged (qualifyAs lusr remove)])
      cc <- E.getTeamConversations tid
      for_ cc $ \c ->
        E.getConversation (c ^. conversationId) >>= \conv ->
          for_ conv $ \dc -> when (remove `isMember` Data.convLocalMembers dc) $ do
            E.deleteMembers (c ^. conversationId) (UserList [remove] [])
            -- If the list was truncated, then the tmids list is incomplete so we simply drop these events
            unless (c ^. managedConversation || mems ^. teamMemberListType == ListTruncated) $
              pushEvent tmids edata now dc
    pushEvent :: Set UserId -> Conv.EventData -> UTCTime -> Data.Conversation -> Sem r ()
    pushEvent exceptTo edata now dc = do
      let qconvId = qUntagged $ qualifyAs lusr (Data.convId dc)
      let (bots, users) = localBotsAndUsers (Data.convLocalMembers dc)
      let x = filter (\m -> not (Conv.lmId m `Set.member` exceptTo)) users
      let y = Conv.Event Conv.MemberLeave qconvId (qUntagged lusr) now edata
      for_ (newPushLocal (mems ^. teamMemberListType) (tUnqualified lusr) (ConvEvent y) (recipient <$> x)) $ \p ->
        E.push1 $ p & pushConn .~ zcon
      E.deliverAsync (bots `zip` repeat y)

getTeamConversations ::
  Members '[Error ActionError, Error NotATeamMember, TeamStore] r =>
  UserId ->
  TeamId ->
  Sem r Public.TeamConversationList
getTeamConversations zusr tid = do
  tm <-
    E.getTeamMember tid zusr
      >>= noteED @NotATeamMember
  unless (tm `hasPermission` GetTeamConversations) $
    throw . OperationDenied . show $ GetTeamConversations
  Public.newTeamConversationList <$> E.getTeamConversations tid

getTeamConversation ::
  Members
    '[ Error ActionError,
       Error ConversationError,
       Error NotATeamMember,
       TeamStore
     ]
    r =>
  UserId ->
  TeamId ->
  ConvId ->
  Sem r Public.TeamConversation
getTeamConversation zusr tid cid = do
  tm <-
    E.getTeamMember tid zusr
      >>= noteED @NotATeamMember
  unless (tm `hasPermission` GetTeamConversations) $
    throw . OperationDenied . show $ GetTeamConversations
  E.getTeamConversation tid cid
    >>= note ConvNotFound

deleteTeamConversation ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InvalidInput,
       Error NotATeamMember,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  TeamId ->
  ConvId ->
  Sem r ()
deleteTeamConversation lusr zcon _tid cid = do
  let lconv = qualifyAs lusr cid
  void $ API.deleteLocalConversation lusr zcon lconv

getSearchVisibilityH ::
  Members
    '[ Error ActionError,
       Error NotATeamMember,
       SearchVisibilityStore,
       TeamStore
     ]
    r =>
  UserId ::: TeamId ::: JSON ->
  Sem r Response
getSearchVisibilityH (uid ::: tid ::: _) = do
  zusrMembership <- E.getTeamMember tid uid
  void $ permissionCheck ViewTeamSearchVisibility zusrMembership
  json <$> getSearchVisibilityInternal tid

setSearchVisibilityH ::
  Members
    '[ Error ActionError,
       Error TeamError,
       Error NotATeamMember,
       Input Opts,
       SearchVisibilityStore,
       TeamStore,
       TeamFeatureStore,
       WaiRoutes
     ]
    r =>
  UserId ::: TeamId ::: JsonRequest Public.TeamSearchVisibilityView ::: JSON ->
  Sem r Response
setSearchVisibilityH (uid ::: tid ::: req ::: _) = do
  zusrMembership <- E.getTeamMember tid uid
  void $ permissionCheck ChangeTeamSearchVisibility zusrMembership
  setSearchVisibilityInternal tid =<< (fromJsonBody req)
  pure noContent

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

ensureUnboundUsers :: Members '[Error TeamError, TeamStore] r => [UserId] -> Sem r ()
ensureUnboundUsers uids = do
  -- We check only 1 team because, by definition, users in binding teams
  -- can only be part of one team.
  teams <- Map.elems <$> E.getUsersTeams uids
  binds <- E.getTeamsBindings teams
  when (any (== Binding) binds) $
    throw UserBindingExists

ensureNonBindingTeam :: Members '[Error TeamError, TeamStore] r => TeamId -> Sem r ()
ensureNonBindingTeam tid = do
  team <- note TeamNotFound =<< E.getTeam tid
  when ((tdTeam team) ^. teamBinding == Binding) $
    throw NoAddToBinding

-- ensure that the permissions are not "greater" than the user's copy permissions
-- this is used to ensure users cannot "elevate" permissions
ensureNotElevated :: Member (Error ActionError) r => Permissions -> TeamMember -> Sem r ()
ensureNotElevated targetPermissions member =
  unless
    ( (targetPermissions ^. self)
        `Set.isSubsetOf` (member ^. permissions . copy)
    )
    $ throw InvalidPermissions

ensureNotTooLarge ::
  Members '[BrigAccess, Error TeamError, Input Opts] r =>
  TeamId ->
  Sem r TeamSize
ensureNotTooLarge tid = do
  o <- input
  (TeamSize size) <- E.getSize tid
  unless (size < fromIntegral (o ^. optSettings . setMaxTeamSize)) $
    throw TooManyTeamMembers
  return $ TeamSize size

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
  Members
    '[ Error LegalHoldError,
       LegalHoldStore,
       TeamStore,
       TeamFeatureStore
     ]
    r =>
  TeamId ->
  Int ->
  Sem r ()
ensureNotTooLargeForLegalHold tid teamSize =
  whenM (isLegalHoldEnabledForTeam tid) $
    unlessM (teamSizeBelowLimit teamSize) $
      throw TooManyTeamMembersOnTeamWithLegalhold

ensureNotTooLargeToActivateLegalHold ::
  Members '[BrigAccess, Error TeamError, TeamStore] r =>
  TeamId ->
  Sem r ()
ensureNotTooLargeToActivateLegalHold tid = do
  (TeamSize teamSize) <- E.getSize tid
  unlessM (teamSizeBelowLimit (fromIntegral teamSize)) $
    throw CannotEnableLegalHoldServiceLargeTeam

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
  Members
    '[ BrigAccess,
       Error TeamError,
       GundeckAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamNotificationStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  TeamId ->
  Maybe UserId ->
  Maybe ConnId ->
  NewTeamMember ->
  TeamMemberList ->
  Sem r TeamSize
addTeamMemberInternal tid origin originConn (view ntmNewTeamMember -> new) memList = do
  P.debug $
    Log.field "targets" (toByteString (new ^. userId))
      . Log.field "action" (Log.val "Teams.addTeamMemberInternal")
  sizeBeforeAdd <- ensureNotTooLarge tid
  E.createTeamMember tid new
  cc <- filter (view managedConversation) <$> E.getTeamConversations tid
  now <- input
  for_ cc $ \c -> do
    lcid <- qualifyLocal (c ^. conversationId)
    luid <- qualifyLocal (new ^. userId)
    E.createMember lcid luid
  let e = newEvent MemberJoin tid now & eventData ?~ EdMemberJoin (new ^. userId)
  E.push1 $
    newPushLocal1 (memList ^. teamMemberListType) (new ^. userId) (TeamEvent e) (recipients origin new) & pushConn .~ originConn
  APITeamQueue.pushTeamEvent tid e
  return sizeBeforeAdd
  where
    recipients (Just o) n =
      list1
        (userRecipient o)
        (membersToRecipients (Just o) (n : memList ^. teamMembers))
    recipients Nothing n =
      list1
        (userRecipient (n ^. userId))
        (membersToRecipients Nothing (memList ^. teamMembers))

-- | See also: 'Gundeck.API.Public.paginateH', but the semantics of this end-point is slightly
-- less warped.  This is a work-around because we cannot send events to all of a large team.
-- See haddocks of module "Galley.API.TeamNotifications" for details.
getTeamNotificationsH ::
  Members
    '[ BrigAccess,
       Error TeamError,
       Error TeamNotificationError,
       TeamNotificationStore
     ]
    r =>
  UserId
    ::: Maybe ByteString {- NotificationId -}
    ::: Range 1 10000 Int32
    ::: JSON ->
  Sem r Response
getTeamNotificationsH (zusr ::: sinceRaw ::: size ::: _) = do
  since <- parseSince
  json @Public.QueuedNotificationList
    <$> APITeamQueue.getTeamNotifications zusr since size
  where
    parseSince :: Member (Error TeamNotificationError) r => Sem r (Maybe Public.NotificationId)
    parseSince = maybe (pure Nothing) (fmap Just . parseUUID) sinceRaw

    parseUUID :: Member (Error TeamNotificationError) r => ByteString -> Sem r Public.NotificationId
    parseUUID raw =
      maybe
        (throw InvalidTeamNotificationId)
        (pure . Id)
        ((UUID.fromASCIIBytes >=> isV1UUID) raw)

    isV1UUID :: UUID.UUID -> Maybe UUID.UUID
    isV1UUID u = if UUID.version u == 1 then Just u else Nothing

finishCreateTeam ::
  Members '[GundeckAccess, Input UTCTime, TeamStore] r =>
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
  let e = newEvent TeamCreate (team ^. teamId) now & eventData ?~ EdTeamCreate team
  let r = membersToRecipients Nothing others
  E.push1 $ newPushLocal1 ListComplete zusr (TeamEvent e) (list1 (userRecipient zusr) r) & pushConn .~ zcon

-- FUTUREWORK: Get rid of CPS
withBindingTeam ::
  Members '[Error TeamError, TeamStore] r =>
  UserId ->
  (TeamId -> Sem r b) ->
  Sem r b
withBindingTeam zusr callback = do
  tid <- E.getOneUserTeam zusr >>= note TeamNotFound
  binding <- E.getTeamBinding tid >>= note TeamNotFound
  case binding of
    Binding -> callback tid
    NonBinding -> throw NotABindingTeamMember

getBindingTeamIdH :: Members '[Error TeamError, TeamStore] r => UserId -> Sem r Response
getBindingTeamIdH = fmap json . getBindingTeamId

getBindingTeamId :: Members '[Error TeamError, TeamStore] r => UserId -> Sem r TeamId
getBindingTeamId zusr = withBindingTeam zusr pure

getBindingTeamMembersH :: Members '[Error TeamError, TeamStore] r => UserId -> Sem r Response
getBindingTeamMembersH = fmap json . getBindingTeamMembers

getBindingTeamMembers ::
  Members
    '[ Error TeamError,
       TeamStore
     ]
    r =>
  UserId ->
  Sem r TeamMemberList
getBindingTeamMembers zusr = withBindingTeam zusr $ \tid ->
  getTeamMembersForFanout tid

canUserJoinTeamH ::
  Members '[BrigAccess, Error LegalHoldError, LegalHoldStore, TeamStore, TeamFeatureStore] r =>
  TeamId ->
  Sem r Response
canUserJoinTeamH tid = canUserJoinTeam tid >> pure empty

-- This could be extended for more checks, for now we test only legalhold
canUserJoinTeam ::
  Members
    '[ BrigAccess,
       Error LegalHoldError,
       LegalHoldStore,
       TeamStore,
       TeamFeatureStore
     ]
    r =>
  TeamId ->
  Sem r ()
canUserJoinTeam tid = do
  lhEnabled <- isLegalHoldEnabledForTeam tid
  when lhEnabled $ do
    (TeamSize sizeBeforeJoin) <- E.getSize tid
    ensureNotTooLargeForLegalHold tid (fromIntegral sizeBeforeJoin + 1)

getTeamSearchVisibilityAvailableInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  TeamId ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSearchVisibility)
getTeamSearchVisibilityAvailableInternal tid = do
  -- TODO: This is just redundant given there is a decent default
  defConfig <- do
    featureTeamSearchVisibility <- view (optSettings . setFeatureFlags . flagTeamSearchVisibility) <$> input
    pure . Public.TeamFeatureStatusNoConfig $ case featureTeamSearchVisibility of
      FeatureTeamSearchVisibilityEnabledByDefault -> Public.TeamFeatureEnabled
      FeatureTeamSearchVisibilityDisabledByDefault -> Public.TeamFeatureDisabled

  fromMaybe defConfig
    <$> TeamFeatures.getFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility tid

-- | Modify and get visibility type for a team (internal, no user permission checks)
getSearchVisibilityInternalH ::
  Member SearchVisibilityStore r =>
  TeamId ::: JSON ->
  Sem r Response
getSearchVisibilityInternalH (tid ::: _) =
  json <$> getSearchVisibilityInternal tid

getSearchVisibilityInternal ::
  Member SearchVisibilityStore r =>
  TeamId ->
  Sem r TeamSearchVisibilityView
getSearchVisibilityInternal =
  fmap TeamSearchVisibilityView
    . SearchVisibilityData.getSearchVisibility

setSearchVisibilityInternalH ::
  Members
    '[ Error TeamError,
       Input Opts,
       SearchVisibilityStore,
       TeamFeatureStore,
       WaiRoutes
     ]
    r =>
  TeamId ::: JsonRequest TeamSearchVisibilityView ::: JSON ->
  Sem r Response
setSearchVisibilityInternalH (tid ::: req ::: _) = do
  setSearchVisibilityInternal tid =<< (fromJsonBody req)
  pure noContent

setSearchVisibilityInternal ::
  Members
    '[ Error TeamError,
       Input Opts,
       SearchVisibilityStore,
       TeamFeatureStore
     ]
    r =>
  TeamId ->
  TeamSearchVisibilityView ->
  Sem r ()
setSearchVisibilityInternal tid (TeamSearchVisibilityView searchVisibility) = do
  status <- getTeamSearchVisibilityAvailableInternal tid
  unless (Public.tfwoStatus status == Public.TeamFeatureEnabled) $
    throw TeamSearchVisibilityNotEnabled
  SearchVisibilityData.setSearchVisibility tid searchVisibility

userIsTeamOwnerH ::
  Members '[Error ActionError, Error TeamError, Error NotATeamMember, TeamStore] r =>
  TeamId ::: UserId ::: JSON ->
  Sem r Response
userIsTeamOwnerH (tid ::: uid ::: _) = do
  userIsTeamOwner tid uid >>= \case
    True -> pure empty
    False -> throw AccessDenied

userIsTeamOwner ::
  Members '[Error TeamError, Error NotATeamMember, TeamStore] r =>
  TeamId ->
  UserId ->
  Sem r Bool
userIsTeamOwner tid uid = do
  let asking = uid
  isTeamOwner . fst <$> getTeamMember asking tid uid

-- Queues a team for async deletion
queueTeamDeletion ::
  Members '[Error InternalError, Queue DeleteItem] r =>
  TeamId ->
  UserId ->
  Maybe ConnId ->
  Sem r ()
queueTeamDeletion tid zusr zcon = do
  ok <- E.tryPush (TeamItem tid zusr zcon)
  unless ok $ throw DeleteQueueFull
