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
    getTeamMembersH,
    bulkGetTeamMembersH,
    getTeamMemberH,
    deleteTeamMemberH,
    updateTeamMemberH,
    getTeamConversationsH,
    getTeamConversationH,
    getTeamConversationRolesH,
    deleteTeamConversationH,
    getSSOStatusH,
    getSSOStatusInternalH,
    setSSOStatusInternalH,
    getLegalholdStatusH,
    getLegalholdStatusInternalH,
    setLegalholdStatusInternalH,
    uncheckedAddTeamMemberH,
    uncheckedGetTeamMemberH,
    uncheckedGetTeamMembersH,
    uncheckedDeleteTeamMember,
    withBindingTeam,
    userIsTeamOwnerH,
    canUserJoinTeamH,
    internalDeleteBindingTeamWithOneMemberH,
    internalDeleteBindingTeamWithOneMember,
  )
where

import Brig.Types.Team (TeamSize (..))
import Brig.Types.Team.LegalHold (LegalHoldStatus (..), LegalHoldTeamConfig (..))
import Control.Lens
import Control.Monad.Catch
import Data.ByteString.Conversion hiding (fromList)
import Data.Id
import qualified Data.List.Extra as List
import Data.List1 (list1)
import Data.Range as Range
import Data.Set (fromList)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Galley.API.Error as Galley
import Galley.API.LegalHold
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.LegalHold as LegalHoldData
import qualified Galley.Data.SSO as SSOData
import Galley.Data.Services (BotMember)
import qualified Galley.Data.Types as Data
import qualified Galley.External as External
import qualified Galley.Intra.Journal as Journal
import Galley.Intra.Push
import qualified Galley.Intra.Spar as Spar
import qualified Galley.Intra.Team as BrigTeam
import Galley.Intra.User
import Galley.Options
import qualified Galley.Queue as Q
import Galley.Types (UserIdList (UserIdList))
import qualified Galley.Types as Conv
import Galley.Types.Conversations.Roles as Roles
import Galley.Types.Teams hiding (newTeam)
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SSO
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (or, result, setStatus)
import Network.Wai.Utilities
import qualified System.Logger.Class as Log
import UnliftIO (mapConcurrently)

getTeamH :: UserId ::: TeamId ::: JSON -> Galley Response
getTeamH (zusr ::: tid ::: _) =
  maybe (throwM teamNotFound) (pure . json) =<< lookupTeam zusr tid

getTeamInternalH :: TeamId ::: JSON -> Galley Response
getTeamInternalH (tid ::: _) =
  maybe (throwM teamNotFound) (pure . json) =<< getTeamInternal tid

getTeamInternal :: TeamId -> Galley (Maybe TeamData)
getTeamInternal = Data.team

getTeamNameInternalH :: TeamId ::: JSON -> Galley Response
getTeamNameInternalH (tid ::: _) =
  maybe (throwM teamNotFound) (pure . json) =<< getTeamNameInternal tid

getTeamNameInternal :: TeamId -> Galley (Maybe TeamName)
getTeamNameInternal = fmap (fmap TeamName) . Data.teamName

getManyTeamsH :: UserId ::: Maybe (Either (Range 1 32 (List TeamId)) TeamId) ::: Range 1 100 Int32 ::: JSON -> Galley Response
getManyTeamsH (zusr ::: range ::: size ::: _) =
  json <$> getManyTeams zusr range size

getManyTeams :: UserId -> Maybe (Either (Range 1 32 (List TeamId)) TeamId) -> Range 1 100 Int32 -> Galley TeamList
getManyTeams zusr range size =
  withTeamIds zusr range size $ \more ids -> do
    teams <- mapM (lookupTeam zusr) ids
    pure (newTeamList (catMaybes teams) more)

lookupTeam :: UserId -> TeamId -> Galley (Maybe Team)
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

createNonBindingTeamH :: UserId ::: ConnId ::: JsonRequest NonBindingNewTeam ::: JSON -> Galley Response
createNonBindingTeamH (zusr ::: zcon ::: req ::: _) = do
  newTeam <- fromJsonBody req
  newTeamId <- createNonBindingTeam zusr zcon newTeam
  pure (empty & setStatus status201 . location newTeamId)

createNonBindingTeam :: UserId -> ConnId -> NonBindingNewTeam -> Galley TeamId
createNonBindingTeam zusr zcon (NonBindingNewTeam body) = do
  let owner = newTeamMember zusr fullPermissions Nothing
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

createBindingTeamH :: UserId ::: TeamId ::: JsonRequest BindingNewTeam ::: JSON -> Galley Response
createBindingTeamH (zusr ::: tid ::: req ::: _) = do
  newTeam <- fromJsonBody req
  newTeamId <- createBindingTeam zusr tid newTeam
  pure (empty & setStatus status201 . location newTeamId)

createBindingTeam :: UserId -> TeamId -> BindingNewTeam -> Galley TeamId
createBindingTeam zusr tid (BindingNewTeam body) = do
  let owner = newTeamMember zusr fullPermissions Nothing
  team <- Data.createTeam (Just tid) zusr (body ^. newTeamName) (body ^. newTeamIcon) (body ^. newTeamIconKey) Binding
  finishCreateTeam team owner [] Nothing
  pure tid

updateTeamStatusH :: TeamId ::: JsonRequest TeamStatusUpdate ::: JSON -> Galley Response
updateTeamStatusH (tid ::: req ::: _) = do
  teamStatusUpdate <- fromJsonBody req
  updateTeamStatus tid teamStatusUpdate
  return empty

updateTeamStatus :: TeamId -> TeamStatusUpdate -> Galley ()
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
    validateTransition :: (TeamStatus, TeamStatus) -> Galley Bool
    validateTransition = \case
      (PendingActive, Active) -> return True
      (Active, Active) -> return False
      (Active, Suspended) -> return True
      (Suspended, Active) -> return True
      (Suspended, Suspended) -> return False
      (_, _) -> throwM invalidTeamStatusUpdate

updateTeamH :: UserId ::: ConnId ::: TeamId ::: JsonRequest TeamUpdateData ::: JSON -> Galley Response
updateTeamH (zusr ::: zcon ::: tid ::: req ::: _) = do
  updateData <- fromJsonBody req
  updateTeam zusr zcon tid updateData
  pure empty

updateTeam :: UserId -> ConnId -> TeamId -> TeamUpdateData -> Galley ()
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
  push1 $ newPush1 (memList ^. teamMemberListType) zusr (TeamEvent e) r & pushConn .~ Just zcon

deleteTeamH :: UserId ::: ConnId ::: TeamId ::: OptionalJsonRequest TeamDeleteData ::: JSON -> Galley Response
deleteTeamH (zusr ::: zcon ::: tid ::: req ::: _) = do
  mBody <- fromOptionalJsonBody req
  deleteTeam zusr zcon tid mBody
  pure (empty & setStatus status202)

-- | 'TeamDeleteData' is only required for binding teams
deleteTeam :: UserId -> ConnId -> TeamId -> Maybe TeamDeleteData -> Galley ()
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
internalDeleteBindingTeamWithOneMember :: TeamId -> Galley ()
internalDeleteBindingTeamWithOneMember tid = do
  team <- Data.team tid
  unless ((view teamBinding . tdTeam <$> team) == Just Binding) $
    throwM noBindingTeam
  mems <- Data.teamMembersWithLimit tid (unsafeRange 2)
  case mems ^. teamMembers of
    (mem : []) -> queueTeamDeletion tid (mem ^. userId) Nothing
    _ -> throwM notAOneMemberTeam

-- This function is "unchecked" because it does not validate that the user has the `DeleteTeam` permission.
uncheckedDeleteTeam :: UserId -> Maybe ConnId -> TeamId -> Galley ()
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
    void . forkIO $ void $ External.deliver be
    -- TODO: we don't delete bots here, but we should do that, since
    -- every bot user can only be in a single conversation. Just
    -- deleting conversations from the database is not enough.
    when ((view teamBinding . tdTeam <$> team) == Just Binding) $ do
      mapM_ (deleteUser . view userId) membs
      Journal.teamDelete tid
    Data.deleteTeam tid
  where
    pushDeleteEvents :: [TeamMember] -> Event -> [Push] -> Galley ()
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
        x : xs -> push1 (newPush1 ListComplete zusr (TeamEvent e) (list1 x xs) & pushConn .~ zcon)
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
      Galley ([Push], [(BotMember, Conv.Event)])
    createConvDeleteEvents now teamMembs c (pp, ee) = do
      (bots, convMembs) <- botsAndUsers <$> Data.members (c ^. conversationId)
      -- Only nonTeamMembers need to get any events, since on team deletion,
      -- all team users are deleted immediately after these events are sent
      -- and will thus never be able to see these events in practice.
      let mm = nonTeamMembers convMembs teamMembs
      let e = Conv.Event Conv.ConvDelete (c ^. conversationId) zusr now Nothing
      -- This event always contains all the required recipients
      let p = newPush ListComplete zusr (ConvEvent e) (map recipient mm)
      let ee' = bots `zip` repeat e
      let pp' = maybe pp (\x -> (x & pushConn .~ zcon) : pp) p
      pure (pp', ee' ++ ee)

getTeamConversationRolesH :: UserId ::: TeamId ::: JSON -> Galley Response
getTeamConversationRolesH (zusr ::: tid ::: _) = do
  json <$> getTeamConversationRoles zusr tid

getTeamConversationRoles :: UserId -> TeamId -> Galley ConversationRolesList
getTeamConversationRoles zusr tid = do
  mem <- Data.teamMember tid zusr
  case mem of
    Nothing -> throwM notATeamMember
    Just _ -> do
      -- NOTE: If/when custom roles are added, these roles should
      --       be merged with the team roles (if they exist)
      pure $ ConversationRolesList wireConvRoles

getTeamMembersH :: UserId ::: TeamId ::: Range 1 HardTruncationLimit Int32 ::: JSON -> Galley Response
getTeamMembersH (zusr ::: tid ::: maxResults ::: _) = do
  (memberList, withPerms) <- getTeamMembers zusr tid maxResults
  pure . json $ teamMemberListJson withPerms memberList

getTeamMembers :: UserId -> TeamId -> Range 1 HardTruncationLimit Int32 -> Galley (TeamMemberList, TeamMember -> Bool)
getTeamMembers zusr tid maxResults = do
  Data.teamMember tid zusr >>= \case
    Nothing -> throwM notATeamMember
    Just m -> do
      mems <- Data.teamMembersWithLimit tid maxResults
      let withPerms = (m `canSeePermsOf`)
      pure (mems, withPerms)

bulkGetTeamMembersH :: UserId ::: TeamId ::: Range 1 HardTruncationLimit Int32 ::: JsonRequest UserIdList ::: JSON -> Galley Response
bulkGetTeamMembersH (zusr ::: tid ::: maxResults ::: body ::: _) = do
  UserIdList uids <- fromJsonBody body
  (memberList, withPerms) <- bulkGetTeamMembers zusr tid maxResults uids
  pure . json $ teamMemberListJson withPerms memberList

-- | like 'getTeamMembers', but with an explicit list of users we are to return.
bulkGetTeamMembers :: UserId -> TeamId -> Range 1 HardTruncationLimit Int32 -> [UserId] -> Galley (TeamMemberList, TeamMember -> Bool)
bulkGetTeamMembers zusr tid maxResults uids = do
  unless (length uids <= fromIntegral (fromRange maxResults)) $
    throwM bulkGetMemberLimitExceeded
  Data.teamMember tid zusr >>= \case
    Nothing -> throwM notATeamMember
    Just m -> do
      mems <- Data.teamMembersLimited tid uids
      let withPerms = (m `canSeePermsOf`)
          hasMore = ListComplete
      pure (newTeamMemberList mems hasMore, withPerms)

getTeamMemberH :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getTeamMemberH (zusr ::: tid ::: uid ::: _) = do
  (member, withPerms) <- getTeamMember zusr tid uid
  pure . json $ teamMemberJson withPerms member

getTeamMember :: UserId -> TeamId -> UserId -> Galley (TeamMember, TeamMember -> Bool)
getTeamMember zusr tid uid = do
  zusrMembership <- Data.teamMember tid zusr
  case zusrMembership of
    Nothing -> throwM notATeamMember
    Just m -> do
      let withPerms = (m `canSeePermsOf`)
      Data.teamMember tid uid >>= \case
        Nothing -> throwM teamMemberNotFound
        Just member -> pure (member, withPerms)

internalDeleteBindingTeamWithOneMemberH :: TeamId -> Galley Response
internalDeleteBindingTeamWithOneMemberH tid = do
  internalDeleteBindingTeamWithOneMember tid
  pure (empty & setStatus status202)

uncheckedGetTeamMemberH :: TeamId ::: UserId ::: JSON -> Galley Response
uncheckedGetTeamMemberH (tid ::: uid ::: _) = do
  json <$> uncheckedGetTeamMember tid uid

uncheckedGetTeamMember :: TeamId -> UserId -> Galley TeamMember
uncheckedGetTeamMember tid uid = do
  Data.teamMember tid uid >>= ifNothing teamMemberNotFound

uncheckedGetTeamMembersH :: TeamId ::: Range 1 HardTruncationLimit Int32 ::: JSON -> Galley Response
uncheckedGetTeamMembersH (tid ::: maxResults ::: _) = do
  json <$> uncheckedGetTeamMembers tid maxResults

uncheckedGetTeamMembers :: TeamId -> Range 1 HardTruncationLimit Int32 -> Galley TeamMemberList
uncheckedGetTeamMembers tid maxResults = Data.teamMembersWithLimit tid maxResults

addTeamMemberH :: UserId ::: ConnId ::: TeamId ::: JsonRequest NewTeamMember ::: JSON -> Galley Response
addTeamMemberH (zusr ::: zcon ::: tid ::: req ::: _) = do
  nmem <- fromJsonBody req
  addTeamMember zusr zcon tid nmem
  pure empty

addTeamMember :: UserId -> ConnId -> TeamId -> NewTeamMember -> Galley ()
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
  memList <- Data.teamMembersForFanout tid
  -- FUTUREWORK: We cannot enable legalhold on large teams right now
  ensureNotTooLargeForLegalHold tid memList
  void $ addTeamMemberInternal tid (Just zusr) (Just zcon) nmem memList

-- This function is "unchecked" because there is no need to check for user binding (invite only).
uncheckedAddTeamMemberH :: TeamId ::: JsonRequest NewTeamMember ::: JSON -> Galley Response
uncheckedAddTeamMemberH (tid ::: req ::: _) = do
  nmem <- fromJsonBody req
  uncheckedAddTeamMember tid nmem
  return empty

uncheckedAddTeamMember :: TeamId -> NewTeamMember -> Galley ()
uncheckedAddTeamMember tid nmem = do
  mems <- Data.teamMembersForFanout tid
  -- FUTUREWORK: We cannot enable legalhold on large teams right now
  ensureNotTooLargeForLegalHold tid mems
  (TeamSize sizeBeforeAdd) <- addTeamMemberInternal tid Nothing Nothing nmem mems

  Journal.teamUpdate tid (sizeBeforeAdd + 1) $ Just $ newTeamMemberList ((nmem ^. ntmNewTeamMember) : mems ^. teamMembers) (mems ^. teamMemberListType)

updateTeamMemberH :: UserId ::: ConnId ::: TeamId ::: JsonRequest NewTeamMember ::: JSON -> Galley Response
updateTeamMemberH (zusr ::: zcon ::: tid ::: req ::: _) = do
  -- the team member to be updated
  targetMember <- view ntmNewTeamMember <$> fromJsonBody req
  updateTeamMember zusr zcon tid targetMember
  pure empty

updateTeamMember :: UserId -> ConnId -> TeamId -> TeamMember -> Galley ()
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
  previousMember <- Data.teamMember tid targetId >>= \case
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
  Data.updateTeamMember tid targetId targetPermissions

  updatedMembers <- Data.teamMembersForFanout tid
  updateJournal team updatedMembers
  updatePeers targetId targetPermissions updatedMembers
  where
    --
    canDowngradeOwner = canDeleteMember
    --
    downgradesOwner :: TeamMember -> Permissions -> Bool
    downgradesOwner previousMember targetPermissions =
      permissionsRole (previousMember ^. permissions) == Just RoleOwner
        && permissionsRole targetPermissions /= Just RoleOwner
    --
    updateJournal :: Team -> TeamMemberList -> Galley ()
    updateJournal team mems = do
      when (team ^. teamBinding == Binding) $ do
        (TeamSize size) <- BrigTeam.getSize tid
        Journal.teamUpdate tid size (Just mems)
    --
    updatePeers :: UserId -> Permissions -> TeamMemberList -> Galley ()
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
      let pushPriv = newPush (updatedMembers ^. teamMemberListType) zusr (TeamEvent ePriv) $ privilegedRecipients
      for_ pushPriv $ \p -> push1 $ p & pushConn .~ Just zcon

deleteTeamMemberH :: UserId ::: ConnId ::: TeamId ::: UserId ::: OptionalJsonRequest TeamMemberDeleteData ::: JSON -> Galley Response
deleteTeamMemberH (zusr ::: zcon ::: tid ::: remove ::: req ::: _) = do
  mBody <- fromOptionalJsonBody req
  deleteTeamMember zusr zcon tid remove mBody >>= \case
    TeamMemberDeleteAccepted -> pure (empty & setStatus status202)
    TeamMemberDeleteCompleted -> pure empty

data TeamMemberDeleteResult
  = TeamMemberDeleteAccepted
  | TeamMemberDeleteCompleted

-- | 'TeamMemberDeleteData' is only required for binding teams
deleteTeamMember :: UserId -> ConnId -> TeamId -> UserId -> Maybe TeamMemberDeleteData -> Galley TeamMemberDeleteResult
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
      let memsWithoutDeleted = newTeamMemberList (filter (\u -> u ^. userId /= remove) (mems ^. teamMembers)) (mems ^. teamMemberListType)
      Journal.teamUpdate tid sizeAfterDelete (Just memsWithoutDeleted)
      pure TeamMemberDeleteAccepted
    else do
      uncheckedDeleteTeamMember zusr (Just zcon) tid remove mems
      pure TeamMemberDeleteCompleted

-- This function is "unchecked" because it does not validate that the user has the `RemoveTeamMember` permission.
uncheckedDeleteTeamMember :: UserId -> Maybe ConnId -> TeamId -> UserId -> TeamMemberList -> Galley ()
uncheckedDeleteTeamMember zusr zcon tid remove mems = do
  now <- liftIO getCurrentTime
  pushMemberLeaveEvent now
  Data.removeTeamMember tid remove
  removeFromConvsAndPushConvLeaveEvent now
  where
    -- notify all team members.
    pushMemberLeaveEvent :: UTCTime -> Galley ()
    pushMemberLeaveEvent now = do
      let e = newEvent MemberLeave tid now & eventData .~ Just (EdMemberLeave remove)
      let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) (mems ^. teamMembers))
      push1 $ newPush1 (mems ^. teamMemberListType) zusr (TeamEvent e) r & pushConn .~ zcon
    -- notify all conversation members not in this team.
    removeFromConvsAndPushConvLeaveEvent :: UTCTime -> Galley ()
    removeFromConvsAndPushConvLeaveEvent now = do
      -- This may not make sense if that list has been truncated. In such cases, we still want to
      -- remove the user from conversations but never send out any events. We assume that clients
      -- handle nicely these missing events, regardless of whether they are in the same team or not
      let tmids = Set.fromList $ map (view userId) (mems ^. teamMembers)
      let edata = Conv.EdMembersLeave (Conv.UserIdList [remove])
      cc <- Data.teamConversations tid
      for_ cc $ \c -> Data.conversation (c ^. conversationId) >>= \conv ->
        for_ conv $ \dc -> when (makeIdOpaque remove `isMember` Data.convMembers dc) $ do
          Data.removeMember remove (c ^. conversationId)
          -- If the list was truncated, then the tmids list is incomplete so we simply drop these events
          unless (c ^. managedConversation || mems ^. teamMemberListType == ListTruncated) $
            pushEvent tmids edata now dc
    pushEvent :: Set UserId -> Conv.EventData -> UTCTime -> Data.Conversation -> Galley ()
    pushEvent exceptTo edata now dc = do
      let (bots, users) = botsAndUsers (Data.convMembers dc)
      let x = filter (\m -> not (Conv.memId m `Set.member` exceptTo)) users
      let y = Conv.Event Conv.MemberLeave (Data.convId dc) zusr now (Just edata)
      for_ (newPush (mems ^. teamMemberListType) zusr (ConvEvent y) (recipient <$> x)) $ \p ->
        push1 $ p & pushConn .~ zcon
      void . forkIO $ void $ External.deliver (bots `zip` repeat y)

getTeamConversationsH :: UserId ::: TeamId ::: JSON -> Galley Response
getTeamConversationsH (zusr ::: tid ::: _) = do
  json <$> getTeamConversations zusr tid

getTeamConversations :: UserId -> TeamId -> Galley TeamConversationList
getTeamConversations zusr tid = do
  tm <- Data.teamMember tid zusr >>= ifNothing notATeamMember
  unless (tm `hasPermission` GetTeamConversations) $
    throwM (operationDenied GetTeamConversations)
  newTeamConversationList <$> Data.teamConversations tid

getTeamConversationH :: UserId ::: TeamId ::: ConvId ::: JSON -> Galley Response
getTeamConversationH (zusr ::: tid ::: cid ::: _) = do
  json <$> getTeamConversation zusr tid cid

getTeamConversation :: UserId -> TeamId -> ConvId -> Galley TeamConversation
getTeamConversation zusr tid cid = do
  tm <- Data.teamMember tid zusr >>= ifNothing notATeamMember
  unless (tm `hasPermission` GetTeamConversations) $
    throwM (operationDenied GetTeamConversations)
  Data.teamConversation tid cid >>= maybe (throwM convNotFound) pure

deleteTeamConversationH :: UserId ::: ConnId ::: TeamId ::: ConvId ::: JSON -> Galley Response
deleteTeamConversationH (zusr ::: zcon ::: tid ::: cid ::: _) = do
  deleteTeamConversation zusr zcon tid cid
  pure empty

deleteTeamConversation :: UserId -> ConnId -> TeamId -> ConvId -> Galley ()
deleteTeamConversation zusr zcon tid cid = do
  (bots, cmems) <- botsAndUsers <$> Data.members cid
  ensureActionAllowed Roles.DeleteConversation =<< getSelfMember zusr cmems
  flip Data.deleteCode Data.ReusableCode =<< Data.mkKey cid
  now <- liftIO getCurrentTime
  let ce = Conv.Event Conv.ConvDelete cid zusr now Nothing
  let recps = fmap recipient cmems
  let convPush = newPush ListComplete zusr (ConvEvent ce) recps <&> pushConn .~ Just zcon
  pushSome $ maybeToList convPush
  void . forkIO $ void $ External.deliver (bots `zip` repeat ce)
  -- TODO: we don't delete bots here, but we should do that, since every
  -- bot user can only be in a single conversation
  Data.removeTeamConv tid cid

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
  (Bool -> [TeamId] -> Galley a) ->
  Galley a
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

ensureUnboundUsers :: [UserId] -> Galley ()
ensureUnboundUsers uids = do
  e <- ask
  -- We check only 1 team because, by definition, users in binding teams
  -- can only be part of one team.
  ts <- liftIO $ mapConcurrently (evalGalley e . Data.oneUserTeam) uids
  let teams = toList $ fromList (catMaybes ts)
  binds <- liftIO $ mapConcurrently (evalGalley e . Data.teamBinding) teams
  when (any ((==) (Just Binding)) binds) $
    throwM userBindingExists

ensureNonBindingTeam :: TeamId -> Galley ()
ensureNonBindingTeam tid = do
  team <- Data.team tid >>= ifNothing teamNotFound
  when ((tdTeam team) ^. teamBinding == Binding) $
    throwM noAddToBinding

-- ensure that the permissions are not "greater" than the user's copy permissions
-- this is used to ensure users cannot "elevate" permissions
ensureNotElevated :: Permissions -> TeamMember -> Galley ()
ensureNotElevated targetPermissions member =
  unless
    ( (targetPermissions ^. self)
        `Set.isSubsetOf` (member ^. permissions . copy)
    )
    $ throwM invalidPermissions

ensureNotTooLarge :: TeamId -> Galley TeamSize
ensureNotTooLarge tid = do
  o <- view options
  (TeamSize size) <- BrigTeam.getSize tid
  unless (size < fromIntegral (o ^. optSettings . setMaxTeamSize)) $
    throwM tooManyTeamMembers
  return $ TeamSize size

-- FUTUREWORK: Large teams cannot have legalhold enabled, needs rethinking
--             due to the expensive operation of removing settings
ensureNotTooLargeForLegalHold :: TeamId -> TeamMemberList -> Galley ()
ensureNotTooLargeForLegalHold tid mems = do
  limit <- fromIntegral . fromRange <$> fanoutLimit
  when (length (mems ^. teamMembers) >= limit) $ do
    lhEnabled <- isLegalHoldEnabled tid
    when lhEnabled $
      throwM tooManyTeamMembersOnTeamWithLegalhold

addTeamMemberInternal :: TeamId -> Maybe UserId -> Maybe ConnId -> NewTeamMember -> TeamMemberList -> Galley TeamSize
addTeamMemberInternal tid origin originConn newMem memList = do
  let new = newMem ^. ntmNewTeamMember
  Log.debug $
    Log.field "targets" (toByteString (new ^. userId))
      . Log.field "action" (Log.val "Teams.addTeamMemberInternal")
  sizeBeforeAdd <- ensureNotTooLarge tid
  Data.addTeamMember tid new
  cc <- filter (view managedConversation) <$> Data.teamConversations tid
  now <- liftIO getCurrentTime
  for_ cc $ \c ->
    Data.addMember now (c ^. conversationId) (new ^. userId)
  let e = newEvent MemberJoin tid now & eventData ?~ EdMemberJoin (new ^. userId)
  push1 $ newPush1 (memList ^. teamMemberListType) (new ^. userId) (TeamEvent e) (recipients origin new) & pushConn .~ originConn
  return sizeBeforeAdd
  where
    recipients (Just o) n = list1 (userRecipient o) (membersToRecipients (Just o) (n : memList ^. teamMembers))
    recipients Nothing n = list1 (userRecipient (n ^. userId)) (membersToRecipients Nothing (memList ^. teamMembers))

finishCreateTeam :: Team -> TeamMember -> [TeamMember] -> Maybe ConnId -> Galley ()
finishCreateTeam team owner others zcon = do
  let zusr = owner ^. userId
  for_ (owner : others) $
    Data.addTeamMember (team ^. teamId)
  now <- liftIO getCurrentTime
  let e = newEvent TeamCreate (team ^. teamId) now & eventData ?~ EdTeamCreate team
  let r = membersToRecipients Nothing others
  push1 $ newPush1 ListComplete zusr (TeamEvent e) (list1 (userRecipient zusr) r) & pushConn .~ zcon

withBindingTeam :: UserId -> (TeamId -> Galley b) -> Galley b
withBindingTeam zusr callback = do
  tid <- Data.oneUserTeam zusr >>= ifNothing teamNotFound
  binding <- Data.teamBinding tid >>= ifNothing teamNotFound
  case binding of
    Binding -> callback tid
    NonBinding -> throwM nonBindingTeam

getBindingTeamIdH :: UserId -> Galley Response
getBindingTeamIdH = fmap json . getBindingTeamId

getBindingTeamId :: UserId -> Galley TeamId
getBindingTeamId zusr = withBindingTeam zusr pure

getBindingTeamMembersH :: UserId -> Galley Response
getBindingTeamMembersH = fmap json . getBindingTeamMembers

getBindingTeamMembers :: UserId -> Galley TeamMemberList
getBindingTeamMembers zusr = withBindingTeam zusr $ \tid ->
  Data.teamMembersForFanout tid

canUserJoinTeamH :: TeamId -> Galley Response
canUserJoinTeamH tid = canUserJoinTeam tid >> pure empty

-- This could be extended for more checks, for now we test only legalhold
canUserJoinTeam :: TeamId -> Galley ()
canUserJoinTeam tid = do
  lhEnabled <- isLegalHoldEnabled tid
  when (lhEnabled) $
    checkTeamSize
  where
    checkTeamSize = do
      (TeamSize size) <- BrigTeam.getSize tid
      limit <- fromIntegral . fromRange <$> fanoutLimit
      -- Teams larger than fanout limit cannot use legalhold
      when (size >= limit) $ do
        throwM tooManyTeamMembersOnTeamWithLegalhold

-- Public endpoints for feature checks

getSSOStatusH :: UserId ::: TeamId ::: JSON -> Galley Response
getSSOStatusH (uid ::: tid ::: _) = do
  json <$> getSSOStatus uid tid

getSSOStatus :: UserId -> TeamId -> Galley SSOTeamConfig
getSSOStatus uid tid = do
  zusrMembership <- Data.teamMember tid uid
  void $ permissionCheck ViewSSOTeamSettings zusrMembership
  getSSOStatusInternal tid

getLegalholdStatusH :: UserId ::: TeamId ::: JSON -> Galley Response
getLegalholdStatusH (uid ::: tid ::: _) = do
  json <$> getLegalholdStatus uid tid

getLegalholdStatus :: UserId -> TeamId -> Galley LegalHoldTeamConfig
getLegalholdStatus uid tid = do
  zusrMembership <- Data.teamMember tid uid
  void $ permissionCheck ViewLegalHoldTeamSettings zusrMembership
  getLegalholdStatusInternal tid

-- Enable / Disable team features
-- These endpoints are internal only and  meant to be called
-- only from authorized personnel (e.g., from a backoffice tool)

-- | Get SSO status for a team.
getSSOStatusInternalH :: TeamId ::: JSON -> Galley Response
getSSOStatusInternalH (tid ::: _) = do
  json <$> getSSOStatusInternal tid

getSSOStatusInternal :: TeamId -> Galley SSOTeamConfig
getSSOStatusInternal tid = do
  defConfig <- do
    featureSSO <- view (options . optSettings . setFeatureFlags . flagSSO)
    pure . SSOTeamConfig $ case featureSSO of
      FeatureSSOEnabledByDefault -> SSOEnabled
      FeatureSSODisabledByDefault -> SSODisabled
  ssoTeamConfig <- SSOData.getSSOTeamConfig tid
  pure . fromMaybe defConfig $ ssoTeamConfig

-- | Enable or disable SSO for a team.
setSSOStatusInternalH :: TeamId ::: JsonRequest SSOTeamConfig ::: JSON -> Galley Response
setSSOStatusInternalH (tid ::: req ::: _) = do
  ssoTeamConfig <- fromJsonBody req
  setSSOStatusInternal tid ssoTeamConfig
  pure noContent

setSSOStatusInternal :: TeamId -> SSOTeamConfig -> Galley ()
setSSOStatusInternal tid ssoTeamConfig = do
  case ssoTeamConfigStatus ssoTeamConfig of
    SSODisabled -> throwM disableSsoNotImplemented
    SSOEnabled -> pure () -- this one is easy to implement :)
  SSOData.setSSOTeamConfig tid ssoTeamConfig

-- | Get legal hold status for a team.
getLegalholdStatusInternalH :: TeamId ::: JSON -> Galley Response
getLegalholdStatusInternalH (tid ::: _) = do
  json <$> getLegalholdStatusInternal tid

getLegalholdStatusInternal :: TeamId -> Galley LegalHoldTeamConfig
getLegalholdStatusInternal tid = do
  featureLegalHold <- view (options . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledByDefault -> do
      legalHoldTeamConfig <- LegalHoldData.getLegalHoldTeamConfig tid
      pure (fromMaybe disabledConfig legalHoldTeamConfig)
    FeatureLegalHoldDisabledPermanently -> do
      pure disabledConfig
  where
    disabledConfig = LegalHoldTeamConfig LegalHoldDisabled

-- | Enable or disable legal hold for a team.
setLegalholdStatusInternalH :: TeamId ::: JsonRequest LegalHoldTeamConfig ::: JSON -> Galley Response
setLegalholdStatusInternalH (tid ::: req ::: _) = do
  legalHoldTeamConfig <- fromJsonBody req
  setLegalholdStatusInternal tid legalHoldTeamConfig
  pure noContent

setLegalholdStatusInternal :: TeamId -> LegalHoldTeamConfig -> Galley ()
setLegalholdStatusInternal tid legalHoldTeamConfig = do
  do
    featureLegalHold <- view (options . optSettings . setFeatureFlags . flagLegalHold)
    case featureLegalHold of
      FeatureLegalHoldDisabledByDefault -> do
        pure ()
      FeatureLegalHoldDisabledPermanently -> do
        throwM legalHoldFeatureFlagNotEnabled
  case legalHoldTeamConfigStatus legalHoldTeamConfig of
    LegalHoldDisabled -> removeSettings' tid
    -- FUTUREWORK: We cannot enable legalhold on large teams right now
    LegalHoldEnabled -> checkTeamSize
  LegalHoldData.setLegalHoldTeamConfig tid legalHoldTeamConfig
  where
    checkTeamSize = do
      (TeamSize size) <- BrigTeam.getSize tid
      limit <- fromIntegral . fromRange <$> fanoutLimit
      when (size > limit) $ do
        throwM cannotEnableLegalHoldServiceLargeTeam

userIsTeamOwnerH :: TeamId ::: UserId ::: JSON -> Galley Response
userIsTeamOwnerH (tid ::: uid ::: _) = do
  userIsTeamOwner tid uid >>= \case
    True -> pure empty
    False -> throwM accessDenied

userIsTeamOwner :: TeamId -> UserId -> Galley Bool
userIsTeamOwner tid uid = do
  let asking = uid
  isTeamOwner . fst <$> getTeamMember asking tid uid

-- Queues a team for async deletion
queueTeamDeletion :: TeamId -> UserId -> Maybe ConnId -> Galley ()
queueTeamDeletion tid zusr zcon = do
  q <- view deleteQueue
  ok <- Q.tryPush q (TeamItem tid zusr zcon)
  if ok
    then pure ()
    else throwM deleteQueueFull
