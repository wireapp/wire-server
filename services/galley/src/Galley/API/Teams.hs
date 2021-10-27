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
import Control.Monad.Catch
import Data.ByteString.Conversion hiding (fromList)
import Data.ByteString.Lazy.Builder (lazyByteString)
import qualified Data.CaseInsensitive as CI
import Data.Csv (EncodeOptions (..), Quoting (QuoteAll), encodeDefaultOrderedByNameWith)
import qualified Data.Handle as Handle
import Data.Id
import qualified Data.LegalHold as LH
import qualified Data.List.Extra as List
import Data.List1 (list1)
import qualified Data.Map.Strict as M
import Data.Misc (HttpsUrl, mkHttpsUrl)
import Data.Qualified
import Data.Range as Range
import Data.Set (fromList)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.Util as UUID
import Galley.API.Error as Galley
import Galley.API.LegalHold
import qualified Galley.API.Teams.Notifications as APITeamQueue
import qualified Galley.API.Update as API
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.LegalHold as Data
import qualified Galley.Data.SearchVisibility as SearchVisibilityData
import Galley.Data.Services (BotMember)
import qualified Galley.Data.TeamFeatures as TeamFeatures
import Galley.Effects
import qualified Galley.External as External
import qualified Galley.Intra.Journal as Journal
import Galley.Intra.Push
import qualified Galley.Intra.Spar as Spar
import qualified Galley.Intra.Team as BrigTeam
import Galley.Intra.User
import Galley.Options
import qualified Galley.Options as Opts
import qualified Galley.Queue as Q
import Galley.Types (UserIdList (UserIdList))
import qualified Galley.Types as Conv
import Galley.Types.Conversations.Roles as Roles
import Galley.Types.Teams hiding (newTeam)
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Imports hiding (forkIO)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (or, result, setStatus)
import Network.Wai.Utilities
import qualified SAML2.WebSSO as SAML
import qualified System.Logger.Class as Log
import UnliftIO.Async (mapConcurrently)
import qualified Wire.API.Conversation.Role as Public
import Wire.API.ErrorDescription (ConvNotFound, NotATeamMember, operationDenied)
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

getTeamH :: UserId ::: TeamId ::: JSON -> Galley r Response
getTeamH (zusr ::: tid ::: _) =
  maybe (throwM teamNotFound) (pure . json) =<< lookupTeam zusr tid

getTeamInternalH :: TeamId ::: JSON -> Galley r Response
getTeamInternalH (tid ::: _) =
  maybe (throwM teamNotFound) (pure . json) =<< getTeamInternal tid

getTeamInternal :: TeamId -> Galley r (Maybe TeamData)
getTeamInternal = Data.team

getTeamNameInternalH :: TeamId ::: JSON -> Galley r Response
getTeamNameInternalH (tid ::: _) =
  maybe (throwM teamNotFound) (pure . json) =<< getTeamNameInternal tid

getTeamNameInternal :: TeamId -> Galley r (Maybe TeamName)
getTeamNameInternal = fmap (fmap TeamName) . Data.teamName

getManyTeamsH :: UserId ::: Maybe (Either (Range 1 32 (List TeamId)) TeamId) ::: Range 1 100 Int32 ::: JSON -> Galley r Response
getManyTeamsH (zusr ::: range ::: size ::: _) =
  json <$> getManyTeams zusr range size

getManyTeams :: UserId -> Maybe (Either (Range 1 32 (List TeamId)) TeamId) -> Range 1 100 Int32 -> Galley r Public.TeamList
getManyTeams zusr range size =
  withTeamIds zusr range size $ \more ids -> do
    teams <- mapM (lookupTeam zusr) ids
    pure (Public.newTeamList (catMaybes teams) more)

lookupTeam :: UserId -> TeamId -> Galley r (Maybe Public.Team)
lookupTeam zusr tid = do
  tm <- Data.teamMember tid zusr
  if isJust tm
    then do
      t <- Data.team tid
      when (Just PendingDelete == (tdStatus <$> t)) $ do
        q <- view deleteQueue
        void $ Q.tryPush q (TeamItem tid zusr Nothing)
      pure (tdTeam <$> t)
    else pure Nothing

createNonBindingTeamH ::
  Members '[GundeckAccess, BrigAccess] r =>
  UserId ::: ConnId ::: JsonRequest Public.NonBindingNewTeam ::: JSON ->
  Galley r Response
createNonBindingTeamH (zusr ::: zcon ::: req ::: _) = do
  newTeam <- fromJsonBody req
  newTeamId <- createNonBindingTeam zusr zcon newTeam
  pure (empty & setStatus status201 . location newTeamId)

createNonBindingTeam ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  Public.NonBindingNewTeam ->
  Galley r TeamId
createNonBindingTeam zusr zcon (Public.NonBindingNewTeam body) = do
  let owner = Public.TeamMember zusr fullPermissions Nothing LH.defUserLegalHoldStatus
  let others =
        filter ((zusr /=) . view userId)
          . maybe [] fromRange
          $ body ^. newTeamMembers
  let zothers = map (view userId) others
  ensureUnboundUsers (zusr : zothers)
  ensureConnectedToLocals zusr zothers
  Log.debug $
    Log.field "targets" (toByteString . show $ toByteString <$> zothers)
      . Log.field "action" (Log.val "Teams.createNonBindingTeam")
  team <- Data.createTeam Nothing zusr (body ^. newTeamName) (body ^. newTeamIcon) (body ^. newTeamIconKey) NonBinding
  finishCreateTeam team owner others (Just zcon)
  pure (team ^. teamId)

createBindingTeamH ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ::: TeamId ::: JsonRequest BindingNewTeam ::: JSON ->
  Galley r Response
createBindingTeamH (zusr ::: tid ::: req ::: _) = do
  newTeam <- fromJsonBody req
  newTeamId <- createBindingTeam zusr tid newTeam
  pure (empty & setStatus status201 . location newTeamId)

createBindingTeam ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ->
  TeamId ->
  BindingNewTeam ->
  Galley r TeamId
createBindingTeam zusr tid (BindingNewTeam body) = do
  let owner = Public.TeamMember zusr fullPermissions Nothing LH.defUserLegalHoldStatus
  team <- Data.createTeam (Just tid) zusr (body ^. newTeamName) (body ^. newTeamIcon) (body ^. newTeamIconKey) Binding
  finishCreateTeam team owner [] Nothing
  pure tid

updateTeamStatusH :: Member BrigAccess r => TeamId ::: JsonRequest TeamStatusUpdate ::: JSON -> Galley r Response
updateTeamStatusH (tid ::: req ::: _) = do
  teamStatusUpdate <- fromJsonBody req
  updateTeamStatus tid teamStatusUpdate
  return empty

updateTeamStatus :: Member BrigAccess r => TeamId -> TeamStatusUpdate -> Galley r ()
updateTeamStatus tid (TeamStatusUpdate newStatus cur) = do
  oldStatus <- tdStatus <$> (Data.team tid >>= ifNothing teamNotFound)
  valid <- validateTransition (oldStatus, newStatus)
  when valid $ do
    journal newStatus cur
    Data.updateTeamStatus tid newStatus
  where
    journal Suspended _ = Journal.teamSuspend tid
    journal Active c = do
      teamCreationTime <- Data.teamCreationTime tid
      -- When teams are created, they are activated immediately. In this situation, Brig will
      -- most likely report team size as 0 due to ES taking some time to index the team creator.
      -- This is also very difficult to test, so is not tested.
      (TeamSize possiblyStaleSize) <- BrigTeam.getSize tid
      let size =
            if possiblyStaleSize == 0
              then 1
              else possiblyStaleSize
      Journal.teamActivate tid size c teamCreationTime
    journal _ _ = throwM invalidTeamStatusUpdate
    validateTransition :: (TeamStatus, TeamStatus) -> Galley r Bool
    validateTransition = \case
      (PendingActive, Active) -> return True
      (Active, Active) -> return False
      (Active, Suspended) -> return True
      (Suspended, Active) -> return True
      (Suspended, Suspended) -> return False
      (_, _) -> throwM invalidTeamStatusUpdate

updateTeamH ::
  Member GundeckAccess r =>
  UserId ::: ConnId ::: TeamId ::: JsonRequest Public.TeamUpdateData ::: JSON ->
  Galley r Response
updateTeamH (zusr ::: zcon ::: tid ::: req ::: _) = do
  updateData <- fromJsonBody req
  updateTeam zusr zcon tid updateData
  pure empty

updateTeam ::
  Member GundeckAccess r =>
  UserId ->
  ConnId ->
  TeamId ->
  Public.TeamUpdateData ->
  Galley r ()
updateTeam zusr zcon tid updateData = do
  zusrMembership <- Data.teamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "Teams.updateTeam")
  void $ permissionCheck SetTeamData zusrMembership
  Data.updateTeam tid updateData
  now <- liftIO getCurrentTime
  memList <- Data.teamMembersForFanout tid
  let e = newEvent TeamUpdate tid now & eventData .~ Just (EdTeamUpdate updateData)
  let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) (memList ^. teamMembers))
  push1 $ newPushLocal1 (memList ^. teamMemberListType) zusr (TeamEvent e) r & pushConn .~ Just zcon

deleteTeamH ::
  Member BrigAccess r =>
  UserId ::: ConnId ::: TeamId ::: OptionalJsonRequest Public.TeamDeleteData ::: JSON ->
  Galley r Response
deleteTeamH (zusr ::: zcon ::: tid ::: req ::: _) = do
  mBody <- fromOptionalJsonBody req
  deleteTeam zusr zcon tid mBody
  pure (empty & setStatus status202)

-- | 'TeamDeleteData' is only required for binding teams
deleteTeam ::
  Member BrigAccess r =>
  UserId ->
  ConnId ->
  TeamId ->
  Maybe Public.TeamDeleteData ->
  Galley r ()
deleteTeam zusr zcon tid mBody = do
  team <- Data.team tid >>= ifNothing teamNotFound
  case tdStatus team of
    Deleted ->
      throwM teamNotFound
    PendingDelete ->
      queueTeamDeletion tid zusr (Just zcon)
    _ -> do
      checkPermissions team
      queueTeamDeletion tid zusr (Just zcon)
  where
    checkPermissions team = do
      void $ permissionCheck DeleteTeam =<< Data.teamMember tid zusr
      when ((tdTeam team) ^. teamBinding == Binding) $ do
        body <- mBody & ifNothing (invalidPayload "missing request body")
        ensureReAuthorised zusr (body ^. tdAuthPassword)

-- This can be called by stern
internalDeleteBindingTeamWithOneMember :: TeamId -> Galley r ()
internalDeleteBindingTeamWithOneMember tid = do
  team <- Data.team tid
  unless ((view teamBinding . tdTeam <$> team) == Just Binding) $
    throwM noBindingTeam
  mems <- Data.teamMembersWithLimit tid (unsafeRange 2)
  case mems ^. teamMembers of
    (mem : []) -> queueTeamDeletion tid (mem ^. userId) Nothing
    _ -> throwM notAOneMemberTeam

-- This function is "unchecked" because it does not validate that the user has the `DeleteTeam` permission.
uncheckedDeleteTeam ::
  forall r.
  Members '[BrigAccess, ExternalAccess, GundeckAccess, SparAccess] r =>
  UserId ->
  Maybe ConnId ->
  TeamId ->
  Galley r ()
uncheckedDeleteTeam zusr zcon tid = do
  team <- Data.team tid
  when (isJust team) $ do
    Spar.deleteTeam tid
    now <- liftIO getCurrentTime
    convs <- filter (not . view managedConversation) <$> Data.teamConversations tid
    -- Even for LARGE TEAMS, we _DO_ want to fetch all team members here because we
    -- want to generate conversation deletion events for non-team users. This should
    -- be fine as it is done once during the life team of a team and we still do not
    -- fanout this particular event to all team members anyway. And this is anyway
    -- done asynchronously
    membs <- Data.teamMembersCollectedWithPagination tid
    (ue, be) <- foldrM (createConvDeleteEvents now membs) ([], []) convs
    let e = newEvent TeamDelete tid now
    pushDeleteEvents membs e ue
    External.deliverAsync be
    -- TODO: we don't delete bots here, but we should do that, since
    -- every bot user can only be in a single conversation. Just
    -- deleting conversations from the database is not enough.
    when ((view teamBinding . tdTeam <$> team) == Just Binding) $ do
      mapM_ (deleteUser . view userId) membs
      Journal.teamDelete tid
    Data.unsetTeamLegalholdWhitelisted tid
    Data.deleteTeam tid
  where
    pushDeleteEvents :: [TeamMember] -> Event -> [Push] -> Galley r ()
    pushDeleteEvents membs e ue = do
      o <- view $ options . optSettings
      let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) membs)
      -- To avoid DoS on gundeck, send team deletion events in chunks
      let chunkSize = fromMaybe defConcurrentDeletionEvents (o ^. setConcurrentDeletionEvents)
      let chunks = List.chunksOf chunkSize (toList r)
      forM_ chunks $ \chunk -> case chunk of
        [] -> return ()
        -- push TeamDelete events. Note that despite having a complete list, we are guaranteed in the
        -- push module to never fan this out to more than the limit
        x : xs -> push1 (newPushLocal1 ListComplete zusr (TeamEvent e) (list1 x xs) & pushConn .~ zcon)
      -- To avoid DoS on gundeck, send conversation deletion events slowly
      let delay = 1000 * (fromMaybe defDeleteConvThrottleMillis (o ^. setDeleteConvThrottleMillis))
      forM_ ue $ \event -> do
        -- push ConversationDelete events
        push1 event
        threadDelay delay
    createConvDeleteEvents ::
      UTCTime ->
      [TeamMember] ->
      TeamConversation ->
      ([Push], [(BotMember, Conv.Event)]) ->
      Galley r ([Push], [(BotMember, Conv.Event)])
    createConvDeleteEvents now teamMembs c (pp, ee) = do
      localDomain <- viewFederationDomain
      let qconvId = Qualified (c ^. conversationId) localDomain
          qorig = Qualified zusr localDomain
      (bots, convMembs) <- localBotsAndUsers <$> Data.members (c ^. conversationId)
      -- Only nonTeamMembers need to get any events, since on team deletion,
      -- all team users are deleted immediately after these events are sent
      -- and will thus never be able to see these events in practice.
      let mm = nonTeamMembers convMembs teamMembs
      let e = Conv.Event Conv.ConvDelete qconvId qorig now Conv.EdConvDelete
      -- This event always contains all the required recipients
      let p = newPushLocal ListComplete zusr (ConvEvent e) (map recipient mm)
      let ee' = bots `zip` repeat e
      let pp' = maybe pp (\x -> (x & pushConn .~ zcon) : pp) p
      pure (pp', ee' ++ ee)

getTeamConversationRoles :: UserId -> TeamId -> Galley r Public.ConversationRolesList
getTeamConversationRoles zusr tid = do
  mem <- Data.teamMember tid zusr
  case mem of
    Nothing -> throwErrorDescriptionType @NotATeamMember
    Just _ -> do
      -- NOTE: If/when custom roles are added, these roles should
      --       be merged with the team roles (if they exist)
      pure $ Public.ConversationRolesList wireConvRoles

getTeamMembersH :: UserId ::: TeamId ::: Range 1 Public.HardTruncationLimit Int32 ::: JSON -> Galley r Response
getTeamMembersH (zusr ::: tid ::: maxResults ::: _) = do
  (memberList, withPerms) <- getTeamMembers zusr tid maxResults
  pure . json $ teamMemberListJson withPerms memberList

getTeamMembers :: UserId -> TeamId -> Range 1 Public.HardTruncationLimit Int32 -> Galley r (Public.TeamMemberList, Public.TeamMember -> Bool)
getTeamMembers zusr tid maxResults = do
  Data.teamMember tid zusr >>= \case
    Nothing -> throwErrorDescriptionType @NotATeamMember
    Just m -> do
      mems <- Data.teamMembersWithLimit tid maxResults
      let withPerms = (m `canSeePermsOf`)
      pure (mems, withPerms)

getTeamMembersCSVH ::
  Member BrigAccess r =>
  UserId ::: TeamId ::: JSON ->
  Galley r Response
getTeamMembersCSVH (zusr ::: tid ::: _) = do
  Data.teamMember tid zusr >>= \case
    Nothing -> throwM accessDenied
    Just member -> unless (member `hasPermission` DownloadTeamMembersCsv) $ throwM accessDenied

  env <- ask
  -- In case an exception is thrown inside the StreamingBody of responseStream
  -- the response will not contain a correct error message, but rather be an
  -- http error such as 'InvalidChunkHeaders'. The exception however still
  -- reaches the middleware and is being tracked in logging and metrics.
  pure $
    responseStream
      status200
      [ (hContentType, "text/csv"),
        ("Content-Disposition", "attachment; filename=\"wire_team_members.csv\"")
      ]
      $ \write flush -> do
        let writeString = write . lazyByteString
        writeString headerLine
        flush
        evalGalley env $ do
          Data.withTeamMembersWithChunks tid $ \members -> do
            inviters <- lookupInviterHandle members
            users <- lookupUser <$> lookupActivatedUsers (fmap (view userId) members)
            richInfos <- lookupRichInfo <$> getRichInfoMultiUser (fmap (view userId) members)
            liftIO $ do
              writeString
                ( encodeDefaultOrderedByNameWith
                    defaultEncodeOptions
                    (mapMaybe (teamExportUser users inviters richInfos) members)
                )
              flush
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

    lookupInviterHandle :: Member BrigAccess r => [TeamMember] -> Galley r (UserId -> Maybe Handle.Handle)
    lookupInviterHandle members = do
      let inviterIds :: [UserId]
          inviterIds = nub $ catMaybes $ fmap fst . view invitation <$> members

      userList :: [User] <- accountUser <$$> getUsers inviterIds

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

bulkGetTeamMembersH :: UserId ::: TeamId ::: Range 1 Public.HardTruncationLimit Int32 ::: JsonRequest Public.UserIdList ::: JSON -> Galley r Response
bulkGetTeamMembersH (zusr ::: tid ::: maxResults ::: body ::: _) = do
  UserIdList uids <- fromJsonBody body
  (memberList, withPerms) <- bulkGetTeamMembers zusr tid maxResults uids
  pure . json $ teamMemberListJson withPerms memberList

-- | like 'getTeamMembers', but with an explicit list of users we are to return.
bulkGetTeamMembers :: UserId -> TeamId -> Range 1 HardTruncationLimit Int32 -> [UserId] -> Galley r (TeamMemberList, TeamMember -> Bool)
bulkGetTeamMembers zusr tid maxResults uids = do
  unless (length uids <= fromIntegral (fromRange maxResults)) $
    throwM bulkGetMemberLimitExceeded
  Data.teamMember tid zusr >>= \case
    Nothing -> throwErrorDescriptionType @NotATeamMember
    Just m -> do
      mems <- Data.teamMembersLimited tid uids
      let withPerms = (m `canSeePermsOf`)
          hasMore = ListComplete
      pure (newTeamMemberList mems hasMore, withPerms)

getTeamMemberH :: UserId ::: TeamId ::: UserId ::: JSON -> Galley r Response
getTeamMemberH (zusr ::: tid ::: uid ::: _) = do
  (member, withPerms) <- getTeamMember zusr tid uid
  pure . json $ teamMemberJson withPerms member

getTeamMember :: UserId -> TeamId -> UserId -> Galley r (Public.TeamMember, Public.TeamMember -> Bool)
getTeamMember zusr tid uid = do
  zusrMembership <- Data.teamMember tid zusr
  case zusrMembership of
    Nothing -> throwErrorDescriptionType @NotATeamMember
    Just m -> do
      let withPerms = (m `canSeePermsOf`)
      Data.teamMember tid uid >>= \case
        Nothing -> throwM teamMemberNotFound
        Just member -> pure (member, withPerms)

internalDeleteBindingTeamWithOneMemberH :: TeamId -> Galley r Response
internalDeleteBindingTeamWithOneMemberH tid = do
  internalDeleteBindingTeamWithOneMember tid
  pure (empty & setStatus status202)

uncheckedGetTeamMemberH :: TeamId ::: UserId ::: JSON -> Galley r Response
uncheckedGetTeamMemberH (tid ::: uid ::: _) = do
  json <$> uncheckedGetTeamMember tid uid

uncheckedGetTeamMember :: TeamId -> UserId -> Galley r TeamMember
uncheckedGetTeamMember tid uid = do
  Data.teamMember tid uid >>= ifNothing teamMemberNotFound

uncheckedGetTeamMembersH :: TeamId ::: Range 1 HardTruncationLimit Int32 ::: JSON -> Galley r Response
uncheckedGetTeamMembersH (tid ::: maxResults ::: _) = do
  json <$> uncheckedGetTeamMembers tid maxResults

uncheckedGetTeamMembers ::
  TeamId ->
  Range 1 HardTruncationLimit Int32 ->
  Galley r TeamMemberList
uncheckedGetTeamMembers tid maxResults = Data.teamMembersWithLimit tid maxResults

addTeamMemberH ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: TeamId ::: JsonRequest Public.NewTeamMember ::: JSON ->
  Galley r Response
addTeamMemberH (zusr ::: zcon ::: tid ::: req ::: _) = do
  nmem <- fromJsonBody req
  addTeamMember zusr zcon tid nmem
  pure empty

addTeamMember ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  TeamId ->
  Public.NewTeamMember ->
  Galley r ()
addTeamMember zusr zcon tid nmem = do
  let uid = nmem ^. ntmNewTeamMember . userId
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "Teams.addTeamMember")
  -- verify permissions
  zusrMembership <-
    Data.teamMember tid zusr
      >>= permissionCheck AddTeamMember
  let targetPermissions = nmem ^. ntmNewTeamMember . permissions
  targetPermissions `ensureNotElevated` zusrMembership
  ensureNonBindingTeam tid
  ensureUnboundUsers [uid]
  ensureConnectedToLocals zusr [uid]
  (TeamSize sizeBeforeJoin) <- BrigTeam.getSize tid
  ensureNotTooLargeForLegalHold tid (fromIntegral sizeBeforeJoin + 1)
  memList <- Data.teamMembersForFanout tid
  void $ addTeamMemberInternal tid (Just zusr) (Just zcon) nmem memList

-- This function is "unchecked" because there is no need to check for user binding (invite only).
uncheckedAddTeamMemberH ::
  Members '[BrigAccess, GundeckAccess] r =>
  TeamId ::: JsonRequest NewTeamMember ::: JSON ->
  Galley r Response
uncheckedAddTeamMemberH (tid ::: req ::: _) = do
  nmem <- fromJsonBody req
  uncheckedAddTeamMember tid nmem
  return empty

uncheckedAddTeamMember ::
  Members '[BrigAccess, GundeckAccess] r =>
  TeamId ->
  NewTeamMember ->
  Galley r ()
uncheckedAddTeamMember tid nmem = do
  mems <- Data.teamMembersForFanout tid
  (TeamSize sizeBeforeJoin) <- BrigTeam.getSize tid
  ensureNotTooLargeForLegalHold tid (fromIntegral sizeBeforeJoin + 1)
  (TeamSize sizeBeforeAdd) <- addTeamMemberInternal tid Nothing Nothing nmem mems
  billingUserIds <- Journal.getBillingUserIds tid $ Just $ newTeamMemberList ((nmem ^. ntmNewTeamMember) : mems ^. teamMembers) (mems ^. teamMemberListType)
  Journal.teamUpdate tid (sizeBeforeAdd + 1) billingUserIds

updateTeamMemberH ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: TeamId ::: JsonRequest Public.NewTeamMember ::: JSON ->
  Galley r Response
updateTeamMemberH (zusr ::: zcon ::: tid ::: req ::: _) = do
  -- the team member to be updated
  targetMember <- view ntmNewTeamMember <$> fromJsonBody req
  updateTeamMember zusr zcon tid targetMember
  pure empty

updateTeamMember ::
  forall r.
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  TeamId ->
  TeamMember ->
  Galley r ()
updateTeamMember zusr zcon tid targetMember = do
  let targetId = targetMember ^. userId
      targetPermissions = targetMember ^. permissions
  Log.debug $
    Log.field "targets" (toByteString targetId)
      . Log.field "action" (Log.val "Teams.updateTeamMember")

  -- get the team and verify permissions
  team <- tdTeam <$> (Data.team tid >>= ifNothing teamNotFound)
  user <-
    Data.teamMember tid zusr
      >>= permissionCheck SetMemberPermissions

  -- user may not elevate permissions
  targetPermissions `ensureNotElevated` user
  previousMember <-
    Data.teamMember tid targetId >>= \case
      Nothing ->
        -- target user must be in same team
        throwM teamMemberNotFound
      Just previousMember -> pure previousMember
  when
    ( downgradesOwner previousMember targetPermissions
        && not (canDowngradeOwner user previousMember)
    )
    $ throwM accessDenied

  -- update target in Cassandra
  Data.updateTeamMember (previousMember ^. permissions) tid targetId targetPermissions

  updatedMembers <- Data.teamMembersForFanout tid
  updateJournal team updatedMembers
  updatePeers targetId targetPermissions updatedMembers
  where
    canDowngradeOwner = canDeleteMember

    downgradesOwner :: TeamMember -> Permissions -> Bool
    downgradesOwner previousMember targetPermissions =
      permissionsRole (previousMember ^. permissions) == Just RoleOwner
        && permissionsRole targetPermissions /= Just RoleOwner

    updateJournal :: Team -> TeamMemberList -> Galley r ()
    updateJournal team mems = do
      when (team ^. teamBinding == Binding) $ do
        (TeamSize size) <- BrigTeam.getSize tid
        billingUserIds <- Journal.getBillingUserIds tid $ Just mems
        Journal.teamUpdate tid size billingUserIds

    updatePeers :: UserId -> Permissions -> TeamMemberList -> Galley r ()
    updatePeers targetId targetPermissions updatedMembers = do
      -- inform members of the team about the change
      -- some (privileged) users will be informed about which change was applied
      let privileged = filter (`canSeePermsOf` targetMember) (updatedMembers ^. teamMembers)
          mkUpdate = EdMemberUpdate targetId
          privilegedUpdate = mkUpdate $ Just targetPermissions
          privilegedRecipients = membersToRecipients Nothing privileged
      now <- liftIO getCurrentTime
      let ePriv = newEvent MemberUpdate tid now & eventData ?~ privilegedUpdate
      -- push to all members (user is privileged)
      let pushPriv = newPushLocal (updatedMembers ^. teamMemberListType) zusr (TeamEvent ePriv) $ privilegedRecipients
      for_ pushPriv $ \p -> push1 $ p & pushConn .~ Just zcon

deleteTeamMemberH ::
  Members '[BrigAccess, ExternalAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: TeamId ::: UserId ::: OptionalJsonRequest Public.TeamMemberDeleteData ::: JSON ->
  Galley r Response
deleteTeamMemberH (zusr ::: zcon ::: tid ::: remove ::: req ::: _) = do
  mBody <- fromOptionalJsonBody req
  deleteTeamMember zusr zcon tid remove mBody >>= \case
    TeamMemberDeleteAccepted -> pure (empty & setStatus status202)
    TeamMemberDeleteCompleted -> pure empty

data TeamMemberDeleteResult
  = TeamMemberDeleteAccepted
  | TeamMemberDeleteCompleted

-- | 'TeamMemberDeleteData' is only required for binding teams
deleteTeamMember ::
  Members '[BrigAccess, ExternalAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  TeamId ->
  UserId ->
  Maybe Public.TeamMemberDeleteData ->
  Galley r TeamMemberDeleteResult
deleteTeamMember zusr zcon tid remove mBody = do
  Log.debug $
    Log.field "targets" (toByteString remove)
      . Log.field "action" (Log.val "Teams.deleteTeamMember")
  zusrMember <- Data.teamMember tid zusr
  targetMember <- Data.teamMember tid remove
  void $ permissionCheck RemoveTeamMember zusrMember
  do
    dm <- maybe (throwM teamMemberNotFound) pure zusrMember
    tm <- maybe (throwM teamMemberNotFound) pure targetMember
    unless (canDeleteMember dm tm) $ throwM accessDenied
  team <- tdTeam <$> (Data.team tid >>= ifNothing teamNotFound)
  mems <- Data.teamMembersForFanout tid
  if team ^. teamBinding == Binding && isJust targetMember
    then do
      body <- mBody & ifNothing (invalidPayload "missing request body")
      ensureReAuthorised zusr (body ^. tmdAuthPassword)
      (TeamSize sizeBeforeDelete) <- BrigTeam.getSize tid
      -- TeamSize is 'Natural' and subtracting from  0 is an error
      -- TeamSize could be reported as 0 if team members are added and removed very quickly,
      -- which happens in tests
      let sizeAfterDelete =
            if sizeBeforeDelete == 0
              then 0
              else sizeBeforeDelete - 1
      deleteUser remove
      billingUsers <- Journal.getBillingUserIds tid (Just mems)
      Journal.teamUpdate tid sizeAfterDelete $ filter (/= remove) billingUsers
      pure TeamMemberDeleteAccepted
    else do
      uncheckedDeleteTeamMember zusr (Just zcon) tid remove mems
      pure TeamMemberDeleteCompleted

-- This function is "unchecked" because it does not validate that the user has the `RemoveTeamMember` permission.
uncheckedDeleteTeamMember ::
  forall r.
  Members '[BrigAccess, GundeckAccess, ExternalAccess] r =>
  UserId ->
  Maybe ConnId ->
  TeamId ->
  UserId ->
  TeamMemberList ->
  Galley r ()
uncheckedDeleteTeamMember zusr zcon tid remove mems = do
  now <- liftIO getCurrentTime
  pushMemberLeaveEvent now
  Data.removeTeamMember tid remove
  removeFromConvsAndPushConvLeaveEvent now
  where
    -- notify all team members.
    pushMemberLeaveEvent :: UTCTime -> Galley r ()
    pushMemberLeaveEvent now = do
      let e = newEvent MemberLeave tid now & eventData ?~ EdMemberLeave remove
      let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) (mems ^. teamMembers))
      push1 $ newPushLocal1 (mems ^. teamMemberListType) zusr (TeamEvent e) r & pushConn .~ zcon
    -- notify all conversation members not in this team.
    removeFromConvsAndPushConvLeaveEvent :: UTCTime -> Galley r ()
    removeFromConvsAndPushConvLeaveEvent now = do
      -- This may not make sense if that list has been truncated. In such cases, we still want to
      -- remove the user from conversations but never send out any events. We assume that clients
      -- handle nicely these missing events, regardless of whether they are in the same team or not
      localDomain <- viewFederationDomain
      let tmids = Set.fromList $ map (view userId) (mems ^. teamMembers)
      let edata = Conv.EdMembersLeave (Conv.QualifiedUserIdList [Qualified remove localDomain])
      cc <- Data.teamConversations tid
      for_ cc $ \c ->
        Data.conversation (c ^. conversationId) >>= \conv ->
          for_ conv $ \dc -> when (remove `isMember` Data.convLocalMembers dc) $ do
            Data.removeMember remove (c ^. conversationId)
            -- If the list was truncated, then the tmids list is incomplete so we simply drop these events
            unless (c ^. managedConversation || mems ^. teamMemberListType == ListTruncated) $
              pushEvent tmids edata now dc
    pushEvent :: Set UserId -> Conv.EventData -> UTCTime -> Data.Conversation -> Galley r ()
    pushEvent exceptTo edata now dc = do
      localDomain <- viewFederationDomain
      let qconvId = Qualified (Data.convId dc) localDomain
          qusr = Qualified zusr localDomain
      let (bots, users) = localBotsAndUsers (Data.convLocalMembers dc)
      let x = filter (\m -> not (Conv.lmId m `Set.member` exceptTo)) users
      let y = Conv.Event Conv.MemberLeave qconvId qusr now edata
      for_ (newPushLocal (mems ^. teamMemberListType) zusr (ConvEvent y) (recipient <$> x)) $ \p ->
        push1 $ p & pushConn .~ zcon
      External.deliverAsync (bots `zip` repeat y)

getTeamConversations :: UserId -> TeamId -> Galley r Public.TeamConversationList
getTeamConversations zusr tid = do
  tm <- Data.teamMember tid zusr >>= ifNothing (errorDescriptionTypeToWai @NotATeamMember)
  unless (tm `hasPermission` GetTeamConversations) $
    throwErrorDescription (operationDenied GetTeamConversations)
  Public.newTeamConversationList <$> Data.teamConversations tid

getTeamConversation :: UserId -> TeamId -> ConvId -> Galley r Public.TeamConversation
getTeamConversation zusr tid cid = do
  tm <- Data.teamMember tid zusr >>= ifNothing (errorDescriptionTypeToWai @NotATeamMember)
  unless (tm `hasPermission` GetTeamConversations) $
    throwErrorDescription (operationDenied GetTeamConversations)
  Data.teamConversation tid cid >>= maybe (throwErrorDescriptionType @ConvNotFound) pure

deleteTeamConversation ::
  Members '[BotAccess, BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ->
  ConnId ->
  TeamId ->
  ConvId ->
  Galley r ()
deleteTeamConversation zusr zcon _tid cid = do
  lusr <- qualifyLocal zusr
  lconv <- qualifyLocal cid
  void $ API.deleteLocalConversation lusr zcon lconv

getSearchVisibilityH :: UserId ::: TeamId ::: JSON -> Galley r Response
getSearchVisibilityH (uid ::: tid ::: _) = do
  zusrMembership <- Data.teamMember tid uid
  void $ permissionCheck ViewTeamSearchVisibility zusrMembership
  json <$> getSearchVisibilityInternal tid

setSearchVisibilityH :: UserId ::: TeamId ::: JsonRequest Public.TeamSearchVisibilityView ::: JSON -> Galley r Response
setSearchVisibilityH (uid ::: tid ::: req ::: _) = do
  zusrMembership <- Data.teamMember tid uid
  void $ permissionCheck ChangeTeamSearchVisibility zusrMembership
  setSearchVisibilityInternal tid =<< fromJsonBody req
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
withTeamIds ::
  UserId ->
  Maybe (Either (Range 1 32 (List TeamId)) TeamId) ->
  Range 1 100 Int32 ->
  (Bool -> [TeamId] -> Galley r a) ->
  Galley r a
withTeamIds usr range size k = case range of
  Nothing -> do
    r <- Data.teamIdsFrom usr Nothing (rcast size)
    k (Data.resultSetType r == Data.ResultSetTruncated) (Data.resultSetResult r)
  Just (Right c) -> do
    r <- Data.teamIdsFrom usr (Just c) (rcast size)
    k (Data.resultSetType r == Data.ResultSetTruncated) (Data.resultSetResult r)
  Just (Left cc) -> do
    ids <- Data.teamIdsOf usr cc
    k False ids
{-# INLINE withTeamIds #-}

ensureUnboundUsers :: [UserId] -> Galley r ()
ensureUnboundUsers uids = do
  -- We check only 1 team because, by definition, users in binding teams
  -- can only be part of one team.
  ts <- liftGalley0 $ mapConcurrently Data.oneUserTeam uids
  let teams = toList $ fromList (catMaybes ts)
  binds <- liftGalley0 $ mapConcurrently Data.teamBinding teams
  when (any ((==) (Just Binding)) binds) $
    throwM userBindingExists

ensureNonBindingTeam :: TeamId -> Galley r ()
ensureNonBindingTeam tid = do
  team <- Data.team tid >>= ifNothing teamNotFound
  when ((tdTeam team) ^. teamBinding == Binding) $
    throwM noAddToBinding

-- ensure that the permissions are not "greater" than the user's copy permissions
-- this is used to ensure users cannot "elevate" permissions
ensureNotElevated :: Permissions -> TeamMember -> Galley r ()
ensureNotElevated targetPermissions member =
  unless
    ( (targetPermissions ^. self)
        `Set.isSubsetOf` (member ^. permissions . copy)
    )
    $ throwM invalidPermissions

ensureNotTooLarge :: Member BrigAccess r => TeamId -> Galley r TeamSize
ensureNotTooLarge tid = do
  o <- view options
  (TeamSize size) <- BrigTeam.getSize tid
  unless (size < fromIntegral (o ^. optSettings . setMaxTeamSize)) $
    throwM tooManyTeamMembers
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
ensureNotTooLargeForLegalHold :: Member BrigAccess r => TeamId -> Int -> Galley r ()
ensureNotTooLargeForLegalHold tid teamSize = do
  whenM (isLegalHoldEnabledForTeam tid) $ do
    unlessM (teamSizeBelowLimit teamSize) $ do
      throwM tooManyTeamMembersOnTeamWithLegalhold

ensureNotTooLargeToActivateLegalHold :: Member BrigAccess r => TeamId -> Galley r ()
ensureNotTooLargeToActivateLegalHold tid = do
  (TeamSize teamSize) <- BrigTeam.getSize tid
  unlessM (teamSizeBelowLimit (fromIntegral teamSize)) $ do
    throwM cannotEnableLegalHoldServiceLargeTeam

teamSizeBelowLimit :: Int -> Galley r Bool
teamSizeBelowLimit teamSize = do
  limit <- fromIntegral . fromRange <$> fanoutLimit
  let withinLimit = teamSize <= limit
  view (options . Opts.optSettings . Opts.setFeatureFlags . flagLegalHold) >>= \case
    FeatureLegalHoldDisabledPermanently -> pure withinLimit
    FeatureLegalHoldDisabledByDefault -> pure withinLimit
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      -- unlimited, see docs of 'ensureNotTooLargeForLegalHold'
      pure True

addTeamMemberInternal ::
  Members '[BrigAccess, GundeckAccess] r =>
  TeamId ->
  Maybe UserId ->
  Maybe ConnId ->
  NewTeamMember ->
  TeamMemberList ->
  Galley r TeamSize
addTeamMemberInternal tid origin originConn (view ntmNewTeamMember -> new) memList = do
  Log.debug $
    Log.field "targets" (toByteString (new ^. userId))
      . Log.field "action" (Log.val "Teams.addTeamMemberInternal")
  sizeBeforeAdd <- ensureNotTooLarge tid
  Data.addTeamMember tid new
  cc <- filter (view managedConversation) <$> Data.teamConversations tid
  now <- liftIO getCurrentTime
  for_ cc $ \c -> do
    lcid <- qualifyLocal (c ^. conversationId)
    luid <- qualifyLocal (new ^. userId)
    Data.addMember lcid luid
  let e = newEvent MemberJoin tid now & eventData ?~ EdMemberJoin (new ^. userId)
  push1 $ newPushLocal1 (memList ^. teamMemberListType) (new ^. userId) (TeamEvent e) (recipients origin new) & pushConn .~ originConn
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
  Member BrigAccess r =>
  UserId
    ::: Maybe ByteString {- NotificationId -}
    ::: Range 1 10000 Int32
    ::: JSON ->
  Galley r Response
getTeamNotificationsH (zusr ::: sinceRaw ::: size ::: _) = do
  since <- parseSince
  json @Public.QueuedNotificationList
    <$> APITeamQueue.getTeamNotifications zusr since size
  where
    parseSince :: Galley r (Maybe Public.NotificationId)
    parseSince = maybe (pure Nothing) (fmap Just . parseUUID) sinceRaw

    parseUUID :: ByteString -> Galley r Public.NotificationId
    parseUUID raw =
      maybe
        (throwM invalidTeamNotificationId)
        (pure . Id)
        ((UUID.fromASCIIBytes >=> isV1UUID) raw)

    isV1UUID :: UUID.UUID -> Maybe UUID.UUID
    isV1UUID u = if UUID.version u == 1 then Just u else Nothing

finishCreateTeam ::
  Member GundeckAccess r =>
  Team ->
  TeamMember ->
  [TeamMember] ->
  Maybe ConnId ->
  Galley r ()
finishCreateTeam team owner others zcon = do
  let zusr = owner ^. userId
  for_ (owner : others) $
    Data.addTeamMember (team ^. teamId)
  now <- liftIO getCurrentTime
  let e = newEvent TeamCreate (team ^. teamId) now & eventData ?~ EdTeamCreate team
  let r = membersToRecipients Nothing others
  push1 $ newPushLocal1 ListComplete zusr (TeamEvent e) (list1 (userRecipient zusr) r) & pushConn .~ zcon

withBindingTeam :: UserId -> (TeamId -> Galley r b) -> Galley r b
withBindingTeam zusr callback = do
  tid <- Data.oneUserTeam zusr >>= ifNothing teamNotFound
  binding <- Data.teamBinding tid >>= ifNothing teamNotFound
  case binding of
    Binding -> callback tid
    NonBinding -> throwM nonBindingTeam

getBindingTeamIdH :: UserId -> Galley r Response
getBindingTeamIdH = fmap json . getBindingTeamId

getBindingTeamId :: UserId -> Galley r TeamId
getBindingTeamId zusr = withBindingTeam zusr pure

getBindingTeamMembersH :: UserId -> Galley r Response
getBindingTeamMembersH = fmap json . getBindingTeamMembers

getBindingTeamMembers :: UserId -> Galley r TeamMemberList
getBindingTeamMembers zusr = withBindingTeam zusr $ \tid ->
  Data.teamMembersForFanout tid

canUserJoinTeamH :: Member BrigAccess r => TeamId -> Galley r Response
canUserJoinTeamH tid = canUserJoinTeam tid >> pure empty

-- This could be extended for more checks, for now we test only legalhold
canUserJoinTeam :: Member BrigAccess r => TeamId -> Galley r ()
canUserJoinTeam tid = do
  lhEnabled <- isLegalHoldEnabledForTeam tid
  when lhEnabled $ do
    (TeamSize sizeBeforeJoin) <- BrigTeam.getSize tid
    ensureNotTooLargeForLegalHold tid (fromIntegral sizeBeforeJoin + 1)

getTeamSearchVisibilityAvailableInternal :: TeamId -> Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility)
getTeamSearchVisibilityAvailableInternal tid = do
  -- TODO: This is just redundant given there is a decent default
  defConfig <- do
    featureTeamSearchVisibility <- view (options . optSettings . setFeatureFlags . flagTeamSearchVisibility)
    pure . Public.TeamFeatureStatusNoConfig $ case featureTeamSearchVisibility of
      FeatureTeamSearchVisibilityEnabledByDefault -> Public.TeamFeatureEnabled
      FeatureTeamSearchVisibilityDisabledByDefault -> Public.TeamFeatureDisabled

  fromMaybe defConfig
    <$> TeamFeatures.getFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility tid

-- | Modify and get visibility type for a team (internal, no user permission checks)
getSearchVisibilityInternalH :: TeamId ::: JSON -> Galley r Response
getSearchVisibilityInternalH (tid ::: _) =
  json <$> getSearchVisibilityInternal tid

getSearchVisibilityInternal :: TeamId -> Galley r TeamSearchVisibilityView
getSearchVisibilityInternal = fmap TeamSearchVisibilityView . SearchVisibilityData.getSearchVisibility

setSearchVisibilityInternalH :: TeamId ::: JsonRequest TeamSearchVisibilityView ::: JSON -> Galley r Response
setSearchVisibilityInternalH (tid ::: req ::: _) = do
  setSearchVisibilityInternal tid =<< fromJsonBody req
  pure noContent

setSearchVisibilityInternal :: TeamId -> TeamSearchVisibilityView -> Galley r ()
setSearchVisibilityInternal tid (TeamSearchVisibilityView searchVisibility) = do
  status <- getTeamSearchVisibilityAvailableInternal tid
  unless (Public.tfwoStatus status == Public.TeamFeatureEnabled) $
    throwM teamSearchVisibilityNotEnabled
  SearchVisibilityData.setSearchVisibility tid searchVisibility

userIsTeamOwnerH :: TeamId ::: UserId ::: JSON -> Galley r Response
userIsTeamOwnerH (tid ::: uid ::: _) = do
  userIsTeamOwner tid uid >>= \case
    True -> pure empty
    False -> throwM accessDenied

userIsTeamOwner :: TeamId -> UserId -> Galley r Bool
userIsTeamOwner tid uid = do
  let asking = uid
  isTeamOwner . fst <$> getTeamMember asking tid uid

-- Queues a team for async deletion
queueTeamDeletion :: TeamId -> UserId -> Maybe ConnId -> Galley r ()
queueTeamDeletion tid zusr zcon = do
  q <- view deleteQueue
  ok <- Q.tryPush q (TeamItem tid zusr zcon)
  if ok
    then pure ()
    else throwM deleteQueueFull
