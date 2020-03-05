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
    uncheckedRemoveTeamMember,
    withBindingTeam,
  )
where

import Brig.Types.Team.LegalHold (LegalHoldStatus (..), LegalHoldTeamConfig (..))
import Cassandra (hasMore, result)
import Control.Lens hiding (from, to)
import Control.Monad.Catch
import Data.ByteString.Conversion hiding (fromList)
import Data.Id
import qualified Data.List.Extra as List
import Data.List1 (list1)
import Data.Range
import Data.Set (fromList)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Galley.API.Error
import Galley.API.LegalHold
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.LegalHold as LegalHoldData
import qualified Galley.Data.SSO as SSOData
import Galley.Data.Services (BotMember)
import Galley.Data.Types
import qualified Galley.External as External
import qualified Galley.Intra.Journal as Journal
import Galley.Intra.Push
import qualified Galley.Intra.Spar as Spar
import Galley.Intra.User
import Galley.Options
import qualified Galley.Queue as Q
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
  ensureConnected zusr zothers
  Log.debug $
    Log.field "targets" (toByteString . show $ toByteString <$> zothers)
      . Log.field "action" (Log.val "Teams.createNonBindingTeam")
  team <- Data.createTeam Nothing zusr (body ^. newTeamName) (body ^. newTeamIcon) (body ^. newTeamIconKey) NonBinding
  finishCreateTeam team owner others (Just zcon)

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

updateTeamStatusH :: TeamId ::: JsonRequest TeamStatusUpdate ::: JSON -> Galley Response
updateTeamStatusH (tid ::: req ::: _) = do
  teamStatusUpdate <- fromJsonBody req
  updateTeamStatus tid teamStatusUpdate
  return empty

updateTeamStatus :: TeamId -> TeamStatusUpdate -> Galley ()
updateTeamStatus tid (TeamStatusUpdate newStatus cur) = do
  oldStatus <- tdStatus <$> (Data.team tid >>= ifNothing teamNotFound)
  valid <- validateTransition oldStatus newStatus
  when valid $ do
    journal newStatus cur
    Data.updateTeamStatus tid newStatus
  where
    journal Suspended _ = Journal.teamSuspend tid
    journal Active c = Data.teamMembersUnsafeForLargeTeams tid >>= \mems ->
      Journal.teamActivate tid mems c =<< Data.teamCreationTime tid
    journal _ _ = throwM invalidTeamStatusUpdate
    validateTransition from to = case (from, to) of
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
  permissionCheckSimple SetTeamData zusrMembership
  Data.updateTeam tid updateData
  now <- liftIO getCurrentTime
  membs <- Data.teamMembersUnsafeForLargeTeams tid
  let e = newEvent TeamUpdate tid now & eventData .~ Just (EdTeamUpdate updateData)
  let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) membs)
  push1 $ newPush1 zusr (TeamEvent e) r & pushConn .~ Just zcon

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
      queueDelete
    _ -> do
      checkPermissions team
      queueDelete
  where
    checkPermissions team = do
      permissionCheckSimple DeleteTeam =<< Data.teamMember tid zusr
      when ((tdTeam team) ^. teamBinding == Binding) $ do
        body <- mBody & ifNothing (invalidPayload "missing request body")
        ensureReAuthorised zusr (body ^. tdAuthPassword)
    queueDelete = do
      q <- view deleteQueue
      ok <- Q.tryPush q (TeamItem tid zusr (Just zcon))
      if ok
        then pure ()
        else throwM deleteQueueFull

-- This function is "unchecked" because it does not validate that the user has the `DeleteTeam` permission.
uncheckedDeleteTeam :: UserId -> Maybe ConnId -> TeamId -> Galley ()
uncheckedDeleteTeam zusr zcon tid = do
  team <- Data.team tid
  when (isJust team) $ do
    Spar.deleteTeam tid
    now <- liftIO getCurrentTime
    convs <- filter (not . view managedConversation) <$> Data.teamConversations tid
    membs <- Data.teamMembersUnsafeForLargeTeams tid
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
        -- push TeamDelete events
        x : xs -> push1 (newPush1 zusr (TeamEvent e) (list1 x xs) & pushConn .~ zcon)
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
      let p = newPush zusr (ConvEvent e) (map recipient mm)
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
    Nothing -> throwM noTeamMember
    Just _ -> do
      -- NOTE: If/when custom roles are added, these roles should
      --       be merged with the team roles (if they exist)
      pure $ ConversationRolesList wireConvRoles

getTeamMembersH :: UserId ::: TeamId ::: Range 1 2000 Int32 ::: JSON -> Galley Response
getTeamMembersH (zusr ::: tid ::: maxResults ::: _) = do
  (memberList, withPerms) <- getTeamMembers zusr tid maxResults
  pure . json $ teamMemberListJson withPerms memberList

getTeamMembers :: UserId -> TeamId -> Range 1 2000 Int32 -> Galley (TeamMemberList, TeamMember -> Bool)
getTeamMembers zusr tid maxResults = do
  (mems, hasMore) <- Data.teamMembers tid maxResults
  Data.teamMember tid zusr >>= \case
    Nothing -> throwM noTeamMember
    Just m -> do
      let withPerms = (m `canSeePermsOf`)
      pure (newTeamMemberList mems hasMore, withPerms)

getTeamMemberH :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getTeamMemberH (zusr ::: tid ::: uid ::: _) = do
  (member, withPerms) <- getTeamMember zusr tid uid
  pure . json $ teamMemberJson withPerms member

getTeamMember :: UserId -> TeamId -> UserId -> Galley (TeamMember, TeamMember -> Bool)
getTeamMember zusr tid uid = do
  zusrMembership <- Data.teamMember tid zusr
  case zusrMembership of
    Nothing -> throwM noTeamMember
    Just m -> do
      let withPerms = (m `canSeePermsOf`)
      Data.teamMember tid uid >>= \case
        Nothing -> throwM teamMemberNotFound
        Just member -> pure (member, withPerms)

uncheckedGetTeamMemberH :: TeamId ::: UserId ::: JSON -> Galley Response
uncheckedGetTeamMemberH (tid ::: uid ::: _) = do
  json <$> uncheckedGetTeamMember tid uid

uncheckedGetTeamMember :: TeamId -> UserId -> Galley TeamMember
uncheckedGetTeamMember tid uid = do
  Data.teamMember tid uid >>= ifNothing teamMemberNotFound

uncheckedGetTeamMembersH :: TeamId ::: Range 1 2000 Int32 ::: JSON -> Galley Response
uncheckedGetTeamMembersH (tid ::: maxResults ::: _) = do
  json <$> uncheckedGetTeamMembers tid maxResults

uncheckedGetTeamMembers :: TeamId -> Range 1 2000 Int32 -> Galley TeamMemberList
uncheckedGetTeamMembers tid maxResults  = do
  (mems, hasMore) <- Data.teamMembers tid maxResults
  return $ newTeamMemberList mems hasMore

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
  zusrMembership <- Data.teamMember tid zusr >>= \case
    Nothing -> throwM noTeamMember
    Just u -> pure u
  -- verify permissions
  permissionCheckSimple AddTeamMember (Just zusrMembership)
  let targetPermissions = nmem ^. ntmNewTeamMember . permissions
  targetPermissions `ensureNotElevated` zusrMembership
  ensureNonBindingTeam tid
  ensureUnboundUsers [uid]
  ensureConnected zusr [uid]
  mems <- Data.teamMembersUnsafeForLargeTeams tid
  addTeamMemberInternal tid (Just zusr) (Just zcon) nmem mems

-- This function is "unchecked" because there is no need to check for user binding (invite only).
uncheckedAddTeamMemberH :: TeamId ::: JsonRequest NewTeamMember ::: JSON -> Galley Response
uncheckedAddTeamMemberH (tid ::: req ::: _) = do
  nmem <- fromJsonBody req
  uncheckedAddTeamMember tid nmem
  return empty

uncheckedAddTeamMember :: TeamId -> NewTeamMember -> Galley ()
uncheckedAddTeamMember tid nmem = do
  mems <- Data.teamMembersUnsafeForLargeTeams tid
  addTeamMemberInternal tid Nothing Nothing nmem mems
  Journal.teamUpdate tid (nmem ^. ntmNewTeamMember : mems)

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
  user <- Data.teamMember tid zusr >>= \case
    Nothing -> throwM noTeamMember
    Just u -> pure u
  permissionCheckSimple SetMemberPermissions (Just user)
  -- user may not elevate permissions
  targetPermissions `ensureNotElevated` user
  -- target user must be in same team
  Data.teamMember tid targetId >>= \case
    Nothing -> throwM teamMemberNotFound
    _ -> pure ()
  -- cannot demote only owner (effectively removing the last owner)
  members <- Data.teamMembersUnsafeForLargeTeams tid
  okToDelete <- canBeDeleted members targetId tid
  when (not okToDelete && targetPermissions /= fullPermissions) $
    throwM noOtherOwner
  -- update target in Cassandra
  Data.updateTeamMember tid targetId targetPermissions
  let otherMembers = filter (\u -> u ^. userId /= targetId) members
      updatedMembers = targetMember : otherMembers
  -- note the change in the journal
  when (team ^. teamBinding == Binding) $ Journal.teamUpdate tid updatedMembers
  -- inform members of the team about the change
  -- some (privileged) users will be informed about which change was applied
  let privileged = filter (`canSeePermsOf` targetMember) updatedMembers
      mkUpdate = EdMemberUpdate targetId
      privilegedUpdate = mkUpdate $ Just targetPermissions
      privilegedRecipients = membersToRecipients Nothing privileged
  now <- liftIO getCurrentTime
  let ePriv = newEvent MemberUpdate tid now & eventData ?~ privilegedUpdate
  -- push to all members (user is privileged)
  let pushPriv = newPush zusr (TeamEvent ePriv) $ privilegedRecipients
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
  zusrMembership <- Data.teamMember tid zusr
  permissionCheckSimple RemoveTeamMember zusrMembership
  okToDelete <- canBeDeleted [] remove tid
  unless okToDelete $ throwM noOtherOwner
  team <- tdTeam <$> (Data.team tid >>= ifNothing teamNotFound)
  removeMembership <- Data.teamMember tid remove
  mems <- Data.teamMembersUnsafeForLargeTeams tid
  if team ^. teamBinding == Binding && isJust removeMembership
    then do
      body <- mBody & ifNothing (invalidPayload "missing request body")
      ensureReAuthorised zusr (body ^. tmdAuthPassword)
      deleteUser remove
      Journal.teamUpdate tid (filter (\u -> u ^. userId /= remove) mems)
      pure TeamMemberDeleteAccepted
    else do
      uncheckedRemoveTeamMember zusr (Just zcon) tid remove mems
      pure TeamMemberDeleteCompleted

-- This function is "unchecked" because it does not validate that the user has the `RemoveTeamMember` permission.
uncheckedRemoveTeamMember :: UserId -> Maybe ConnId -> TeamId -> UserId -> [TeamMember] -> Galley ()
uncheckedRemoveTeamMember zusr zcon tid remove mems = do
  now <- liftIO getCurrentTime
  let e = newEvent MemberLeave tid now & eventData .~ Just (EdMemberLeave remove)
  let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) mems)
  push1 $ newPush1 zusr (TeamEvent e) r & pushConn .~ zcon
  Data.removeTeamMember tid remove
  let tmids = Set.fromList $ map (view userId) mems
  let edata = Conv.EdMembersLeave (Conv.UserIdList [remove])
  cc <- Data.teamConversations tid
  for_ cc $ \c -> Data.conversation (c ^. conversationId) >>= \conv ->
    for_ conv $ \dc -> when (remove `isMember` Data.convMembers dc) $ do
      Data.removeMember remove (c ^. conversationId)
      unless (c ^. managedConversation) $
        pushEvent tmids edata now dc
  where
    pushEvent tmids edata now dc = do
      let (bots, users) = botsAndUsers (Data.convMembers dc)
      let x = filter (\m -> not (Conv.memId m `Set.member` tmids)) users
      let y = Conv.Event Conv.MemberLeave (Data.convId dc) zusr now (Just edata)
      for_ (newPush zusr (ConvEvent y) (recipient <$> x)) $ \p ->
        push1 $ p & pushConn .~ zcon
      void . forkIO $ void $ External.deliver (bots `zip` repeat y)

getTeamConversationsH :: UserId ::: TeamId ::: JSON -> Galley Response
getTeamConversationsH (zusr ::: tid ::: _) = do
  json <$> getTeamConversations zusr tid

getTeamConversations :: UserId -> TeamId -> Galley TeamConversationList
getTeamConversations zusr tid = do
  tm <- Data.teamMember tid zusr >>= ifNothing noTeamMember
  unless (tm `hasPermission` GetTeamConversations) $
    throwM (operationDenied GetTeamConversations)
  newTeamConversationList <$> Data.teamConversations tid

getTeamConversationH :: UserId ::: TeamId ::: ConvId ::: JSON -> Galley Response
getTeamConversationH (zusr ::: tid ::: cid ::: _) = do
  json <$> getTeamConversation zusr tid cid

getTeamConversation :: UserId -> TeamId -> ConvId -> Galley TeamConversation
getTeamConversation zusr tid cid = do
  tm <- Data.teamMember tid zusr >>= ifNothing noTeamMember
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
  flip Data.deleteCode ReusableCode =<< mkKey cid
  now <- liftIO getCurrentTime
  let ce = Conv.Event Conv.ConvDelete cid zusr now Nothing
  let recps = fmap recipient cmems
  let convPush = newPush zusr (ConvEvent ce) recps <&> pushConn .~ Just zcon
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
    Data.ResultSet r <- Data.teamIdsFrom usr Nothing (rcast size)
    k (hasMore r) (result r)
  Just (Right c) -> do
    Data.ResultSet r <- Data.teamIdsFrom usr (Just c) (rcast size)
    k (hasMore r) (result r)
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

addTeamMemberInternal :: TeamId -> Maybe UserId -> Maybe ConnId -> NewTeamMember -> [TeamMember] -> Galley ()
addTeamMemberInternal tid origin originConn newMem mems = do
  let new = newMem ^. ntmNewTeamMember
  Log.debug $
    Log.field "targets" (toByteString (new ^. userId))
      . Log.field "action" (Log.val "Teams.addTeamMemberInternal")
  o <- view options
  unless (length mems < fromIntegral (o ^. optSettings . setMaxTeamSize)) $
    throwM tooManyTeamMembers
  Data.addTeamMember tid new
  cc <- filter (view managedConversation) <$> Data.teamConversations tid
  now <- liftIO getCurrentTime
  for_ cc $ \c ->
    Data.addMember now (c ^. conversationId) (new ^. userId)
  let e = newEvent MemberJoin tid now & eventData .~ Just (EdMemberJoin (new ^. userId))
  push1 $ newPush1 (new ^. userId) (TeamEvent e) (r origin new) & pushConn .~ originConn
  where
    r (Just o) n = list1 (userRecipient o) (membersToRecipients (Just o) (n : mems))
    r Nothing n = list1 (userRecipient (n ^. userId)) (membersToRecipients Nothing (n : mems))

finishCreateTeam :: Team -> TeamMember -> [TeamMember] -> Maybe ConnId -> Galley TeamId
finishCreateTeam team owner others zcon = do
  let zusr = owner ^. userId
  for_ (owner : others) $
    Data.addTeamMember (team ^. teamId)
  now <- liftIO getCurrentTime
  let e = newEvent TeamCreate (team ^. teamId) now & eventData .~ Just (EdTeamCreate team)
  let r = membersToRecipients Nothing others
  push1 $ newPush1 zusr (TeamEvent e) (list1 (userRecipient zusr) r) & pushConn .~ zcon
  pure (team ^. teamId)

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
getBindingTeamMembers zusr = withBindingTeam zusr $ \tid -> do
  members <- Data.teamMembersUnsafeForLargeTeams tid
  pure $ newTeamMemberList members False

-- Public endpoints for feature checks

getSSOStatusH :: UserId ::: TeamId ::: JSON -> Galley Response
getSSOStatusH (uid ::: tid ::: _) = do
  json <$> getSSOStatus uid tid

getSSOStatus :: UserId -> TeamId -> Galley SSOTeamConfig
getSSOStatus uid tid = do
  zusrMembership <- Data.teamMember tid uid
  permissionCheckSimple ViewSSOTeamSettings zusrMembership
  getSSOStatusInternal tid

getLegalholdStatusH :: UserId ::: TeamId ::: JSON -> Galley Response
getLegalholdStatusH (uid ::: tid ::: _) = do
  json <$> getLegalholdStatus uid tid

getLegalholdStatus :: UserId -> TeamId -> Galley LegalHoldTeamConfig
getLegalholdStatus uid tid = do
  zusrMembership <- Data.teamMember tid uid
  permissionCheckSimple ViewLegalHoldTeamSettings zusrMembership
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
    LegalHoldDisabled -> removeSettings' tid Nothing
    LegalHoldEnabled -> pure ()
  LegalHoldData.setLegalHoldTeamConfig tid legalHoldTeamConfig
