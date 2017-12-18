{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.API.Teams
    ( createBindingTeam
    , createNonBindingTeam
    , updateTeam
    , updateTeamStatus
    , getTeam
    , getTeamInternal
    , getTeamNameInternal
    , getBindingTeamId
    , getBindingTeamMembers
    , getManyTeams
    , deleteTeam
    , uncheckedDeleteTeam
    , addTeamMember
    , getTeamMembers
    , getTeamMember
    , deleteTeamMember
    , getTeamConversations
    , getTeamConversation
    , deleteTeamConversation
    , updateTeamMember
    , uncheckedAddTeamMember
    , uncheckedGetTeamMember
    , uncheckedGetTeamMembers
    , uncheckedRemoveTeamMember
    , withBindingTeam
    ) where

import Cassandra (result, hasMore)
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens hiding (from, to)
import Control.Monad (unless, when, void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Conversion hiding (fromList)
import Data.Foldable (for_, foldrM)
import Data.Int
import Data.Id
import Data.List1 (list1)
import Data.List (partition)
import Data.Maybe (catMaybes, isJust)
import Data.Range
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (mapM)
import Data.Set (fromList, toList)
import Galley.App
import Galley.API.Error
import Galley.API.Util
import Galley.Intra.Push
import Galley.Intra.User
import Galley.Options
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result, or)
import Network.Wai.Utilities
import Prelude hiding (head, mapM)

import qualified Data.Set as Set
import qualified Galley.Data as Data
import qualified Galley.Queue as Q
import qualified Galley.Types as Conv
import qualified Galley.Types.Teams as Teams
import qualified Galley.Intra.Journal as Journal

getTeam :: UserId ::: TeamId ::: JSON -> Galley Response
getTeam (zusr::: tid ::: _) =
    maybe (throwM teamNotFound) (pure . json) =<< lookupTeam zusr tid

getTeamInternal :: TeamId ::: JSON -> Galley Response
getTeamInternal (tid ::: _) =
    maybe (throwM teamNotFound) (pure . json) =<< Data.team tid

getTeamNameInternal :: TeamId ::: JSON -> Galley Response
getTeamNameInternal (tid ::: _) =
    maybe (throwM teamNotFound) (pure . json . TeamName) =<< Data.teamName tid

getManyTeams :: UserId ::: Maybe (Either (Range 1 32 (List TeamId)) TeamId) ::: Range 1 100 Int32 ::: JSON -> Galley Response
getManyTeams (zusr ::: range ::: size ::: _) =
    withTeamIds zusr range size $ \more ids -> do
        teams <- mapM (lookupTeam zusr) ids
        pure (json $ newTeamList (catMaybes teams) more)

lookupTeam :: UserId -> TeamId -> Galley (Maybe Team)
lookupTeam zusr tid = do
    tm <- Data.teamMember tid zusr
    if isJust tm then do
        t <- Data.team tid
        when (Just PendingDelete == (tdStatus <$> t)) $ do
            q <- view deleteQueue
            void $ Q.tryPush q (TeamItem tid zusr Nothing)
        pure (tdTeam <$> t)
    else
        pure Nothing

createNonBindingTeam :: UserId ::: ConnId ::: Request ::: JSON ::: JSON -> Galley Response
createNonBindingTeam (zusr::: zcon ::: req ::: _) = do
    NonBindingNewTeam body <- fromBody req invalidPayload
    let owner  = newTeamMember zusr fullPermissions
    let others = filter ((zusr /=) . view userId)
               . maybe [] fromRange
               $ body^.newTeamMembers
    let zothers = map (view userId) others
    ensureUnboundUsers (zusr : zothers)
    ensureConnected zusr zothers
    team <- Data.createTeam Nothing zusr (body^.newTeamName) (body^.newTeamIcon) (body^.newTeamIconKey) NonBinding
    finishCreateTeam team owner others (Just zcon)

createBindingTeam :: UserId ::: TeamId ::: Request ::: JSON ::: JSON -> Galley Response
createBindingTeam (zusr ::: tid ::: req ::: _) = do
    BindingNewTeam body <- fromBody req invalidPayload
    let owner  = newTeamMember zusr fullPermissions
    team <- Data.createTeam (Just tid) zusr (body^.newTeamName) (body^.newTeamIcon) (body^.newTeamIconKey) Binding
    finishCreateTeam team owner [] Nothing

updateTeamStatus :: TeamId ::: Request ::: JSON ::: JSON -> Galley Response
updateTeamStatus (tid ::: req ::: _) = do
    TeamStatusUpdate to <- fromBody req invalidPayload
    team <- Data.team tid >>= ifNothing teamNotFound
    valid <- validateTransition (tdStatus team) to
    when valid $ do
      journal to
      Data.updateTeamStatus tid to
    return empty
  where
    journal Suspended = Journal.teamSuspend tid
    journal Active    = Data.teamMembers tid >>= \mems ->
                        Journal.teamActivate tid mems =<< Data.teamCreationTime tid
    journal _         = throwM invalidTeamStatusUpdate

    validateTransition from to = case (from, to) of
        ( PendingActive, Active    ) -> return True
        ( Active       , Active    ) -> return False
        ( Active       , Suspended ) -> return True
        ( Suspended    , Active    ) -> return True
        ( Suspended    , Suspended ) -> return False
        ( _            , _         ) -> throwM invalidTeamStatusUpdate

updateTeam :: UserId ::: ConnId ::: TeamId ::: Request ::: JSON ::: JSON -> Galley Response
updateTeam (zusr::: zcon ::: tid ::: req ::: _) = do
    body <- fromBody req invalidPayload
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr SetTeamData membs
    Data.updateTeam tid body
    now <- liftIO getCurrentTime
    let e = newEvent TeamUpdate tid now & eventData .~ Just (EdTeamUpdate body)
    let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) membs)
    push1 $ newPush1 zusr (TeamEvent e) r & pushConn .~ Just zcon
    pure empty

deleteTeam :: UserId ::: ConnId ::: TeamId ::: Request ::: Maybe JSON ::: JSON -> Galley Response
deleteTeam (zusr::: zcon ::: tid ::: req ::: _ ::: _) = do
    team <- Data.team tid >>= ifNothing teamNotFound
    case tdStatus team of
        Deleted -> throwM teamNotFound
        PendingDelete -> queueDelete
        _ -> do
            void $ permissionCheck zusr DeleteTeam =<< Data.teamMembers tid
            when ((tdTeam team)^.teamBinding == Binding) $ do
                body <- fromBody req invalidPayload
                ensureReAuthorised zusr (body^.tdAuthPassword)
            queueDelete
  where
    queueDelete = do
        q  <- view deleteQueue
        ok <- Q.tryPush q (TeamItem tid zusr (Just zcon))
        if ok then
            pure (empty & setStatus status202)
        else
            throwM deleteQueueFull

-- This function is "unchecked" because it does not validate that the user has the `DeleteTeam` permission.
uncheckedDeleteTeam :: UserId -> Maybe ConnId -> TeamId -> Galley ()
uncheckedDeleteTeam zusr zcon tid = do
    team <- Data.team tid
    when (isJust team) $ do
        membs  <- Data.teamMembers tid
        now    <- liftIO getCurrentTime
        convs  <- filter (not . view managedConversation) <$> Data.teamConversations tid
        events <- foldrM (pushEvents now membs) [] convs
        let e = newEvent TeamDelete tid now
        let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) membs)
        pushSome ((newPush1 zusr (TeamEvent e) r & pushConn .~ zcon) : events)
        when ((view teamBinding . tdTeam <$> team) == Just Binding) $ do
            mapM_ (deleteUser . view userId) membs
            Journal.teamDelete tid
        Data.deleteTeam tid
  where
    pushEvents now membs c pp = do
        mm <- flip nonTeamMembers membs <$> Data.members (c^.conversationId)
        let e = Conv.Event Conv.ConvDelete (c^.conversationId) zusr now Nothing
        let p = newPush zusr (ConvEvent e) (map recipient mm)
        pure (maybe pp (\x -> (x & pushConn .~ zcon) : pp) p)

getTeamMembers :: UserId ::: TeamId ::: JSON -> Galley Response
getTeamMembers (zusr::: tid ::: _) = do
    mems <- Data.teamMembers tid
    case findTeamMember zusr mems of
        Nothing -> throwM noTeamMember
        Just  m -> do
            let withPerm = m `hasPermission` GetMemberPermissions
            pure (json $ teamMemberListJson withPerm (newTeamMemberList mems))

getTeamMember :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getTeamMember (zusr::: tid ::: uid ::: _) = do
    mems <- Data.teamMembers tid
    case findTeamMember zusr mems of
        Nothing -> throwM noTeamMember
        Just  m -> do
            let withPerm = m `hasPermission` GetMemberPermissions
            let member   = findTeamMember uid mems
            maybe (throwM teamMemberNotFound) (pure . json . teamMemberJson withPerm) member

uncheckedGetTeamMember :: TeamId ::: UserId ::: JSON -> Galley Response
uncheckedGetTeamMember (tid ::: uid ::: _) = do
    mem <- Data.teamMember tid uid >>= ifNothing teamMemberNotFound
    return . json $ teamMemberJson True mem

uncheckedGetTeamMembers :: TeamId ::: JSON -> Galley Response
uncheckedGetTeamMembers (tid ::: _) = do
    mems <- Data.teamMembers tid
    return . json $ teamMemberListJson True (newTeamMemberList mems)

addTeamMember :: UserId ::: ConnId ::: TeamId ::: Request ::: JSON ::: JSON -> Galley Response
addTeamMember (zusr::: zcon ::: tid ::: req ::: _) = do
    nmem <- fromBody req invalidPayload
    mems <- Data.teamMembers tid

    -- verify permissions
    tmem <- permissionCheck zusr AddTeamMember mems
    let targetPermissions = nmem^.ntmNewTeamMember.permissions
    targetPermissions `ensureNotElevated` tmem

    ensureNonBindingTeam tid
    ensureUnboundUsers [nmem^.ntmNewTeamMember.userId]
    ensureConnected zusr [nmem^.ntmNewTeamMember.userId]
    addTeamMemberInternal tid (Just zusr) (Just zcon) nmem mems

-- This function is "unchecked" because there is no need to check for user binding (invite only).
uncheckedAddTeamMember :: TeamId ::: Request ::: JSON ::: JSON -> Galley Response
uncheckedAddTeamMember (tid ::: req ::: _) = do
    nmem <- fromBody req invalidPayload
    mems <- Data.teamMembers tid
    rsp <- addTeamMemberInternal tid Nothing Nothing nmem mems
    Journal.teamUpdate tid (nmem^.ntmNewTeamMember : mems)
    return rsp

updateTeamMember :: UserId ::: ConnId ::: TeamId ::: Request ::: JSON ::: JSON
                 -> Galley Response
updateTeamMember (zusr::: zcon ::: tid ::: req ::: _) = do
    -- the team member to be updated
    targetMember <- view ntmNewTeamMember <$> fromBody req invalidPayload
    let targetId          = targetMember^.userId
        targetPermissions = targetMember^.permissions

    -- get the team and verify permissions
    team    <- tdTeam <$> (Data.team tid >>= ifNothing teamNotFound)
    members <- Data.teamMembers tid
    user    <- permissionCheck zusr SetMemberPermissions members

    -- user may not elevate permissions
    targetPermissions `ensureNotElevated` user

    -- target user must be in same team
    unless (isTeamMember targetId members) $
      throwM teamMemberNotFound

    -- cannot demote only owner (effectively removing the last owner)
    when (targetId `isOnlyOwner` members
          && targetPermissions /= fullPermissions) $
        throwM noOtherOwner

    -- update target in Cassandra
    Data.updateTeamMember tid targetId targetPermissions

    let otherMembers = filter (\u -> u^.userId /= targetId) members
        updatedMembers = targetMember : otherMembers

    -- note the change in the journal
    when (team^.teamBinding == Binding) $ Journal.teamUpdate tid updatedMembers

    -- inform members of the team about the change
    -- some (privileged) users will be informed about which change was applied
    let privilege = flip hasPermission GetMemberPermissions
        (privileged, unprivileged) = partition privilege updatedMembers
        mkUpdate               = EdMemberUpdate targetId
        privilegedUpdate       = mkUpdate $ Just targetPermissions
        unPrivilegedUpdate     = mkUpdate Nothing
        privilegedRecipients   = membersToRecipients Nothing privileged
        unPrivilegedRecipients = membersToRecipients Nothing unprivileged

    now <- liftIO getCurrentTime
    let ePriv  = newEvent MemberUpdate tid now & eventData ?~ privilegedUpdate
        eUPriv = newEvent MemberUpdate tid now & eventData ?~ unPrivilegedUpdate

    -- push to all members (user is privileged)
    let pushPriv   = newPush zusr (TeamEvent ePriv) $ privilegedRecipients
        pushUnPriv = newPush zusr (TeamEvent eUPriv) $ unPrivilegedRecipients
    for_ pushPriv   $ \p -> push1 $ p & pushConn .~ Just zcon
    for_ pushUnPriv $ \p -> push1 $ p & pushConn .~ Just zcon
    pure empty

deleteTeamMember :: UserId ::: ConnId ::: TeamId ::: UserId ::: Request ::: Maybe JSON ::: JSON -> Galley Response
deleteTeamMember (zusr::: zcon ::: tid ::: remove ::: req ::: _ ::: _) = do
    mems <- Data.teamMembers tid
    void $ permissionCheck zusr RemoveTeamMember mems
    when (remove `isOnlyOwner` mems) $
        throwM noOtherOwner
    team <- tdTeam <$> (Data.team tid >>= ifNothing teamNotFound)
    if team^.teamBinding == Binding && isTeamMember remove mems then do
        body <- fromBody req invalidPayload
        ensureReAuthorised zusr (body^.tmdAuthPassword)
        deleteUser remove
        Journal.teamUpdate tid (filter (\u -> u^.userId /= remove) mems)
        pure (empty & setStatus status202)
    else do
        uncheckedRemoveTeamMember zusr (Just zcon) tid remove mems
        pure empty

-- This function is "unchecked" because it does not validate that the user has the `RemoveTeamMember` permission.
uncheckedRemoveTeamMember :: UserId -> Maybe ConnId -> TeamId -> UserId -> [TeamMember] -> Galley ()
uncheckedRemoveTeamMember zusr zcon tid remove mems = do
    now <- liftIO getCurrentTime
    let e = newEvent MemberLeave tid now & eventData .~ Just (EdMemberLeave remove)
    let r = list1 (userRecipient zusr) (membersToRecipients (Just zusr) mems)
    push1 $ newPush1 zusr (TeamEvent e) r & pushConn .~ zcon
    Data.removeTeamMember tid remove
    let tmids = Set.fromList $ map (view userId) mems
    let edata = Conv.EdMembers (Conv.Members [remove])
    cc <- Data.teamConversations tid
    for_ cc $ \c -> Data.conversation (c^.conversationId) >>= \conv ->
        for_ conv $ \dc -> when (remove `isMember` Data.convMembers dc) $ do
            Data.removeMember remove (c^.conversationId)
            unless (c^.managedConversation) $
                pushEvent tmids edata now dc
  where
    pushEvent tmids edata now dc = do
        let x = filter (\m -> not (Conv.memId m `Set.member` tmids)) (Data.convMembers dc)
        let y = Conv.Event Conv.MemberLeave (Data.convId dc) zusr now (Just edata)
        for_ (newPush zusr (ConvEvent y) (recipient <$> x)) $ \p ->
            push1 $ p & pushConn .~ zcon

getTeamConversations :: UserId ::: TeamId ::: JSON -> Galley Response
getTeamConversations (zusr::: tid ::: _) = do
    tm <- Data.teamMember tid zusr >>= ifNothing noTeamMember
    unless (tm `hasPermission` GetTeamConversations) $
        throwM (operationDenied GetTeamConversations)
    json . newTeamConversationList <$> Data.teamConversations tid

getTeamConversation :: UserId ::: TeamId ::: ConvId ::: JSON -> Galley Response
getTeamConversation (zusr::: tid ::: cid ::: _) = do
    tm <- Data.teamMember tid zusr >>= ifNothing noTeamMember
    unless (tm `hasPermission` GetTeamConversations) $
        throwM (operationDenied GetTeamConversations)
    Data.teamConversation tid cid >>= maybe (throwM convNotFound) (pure . json)

deleteTeamConversation :: UserId ::: ConnId ::: TeamId ::: ConvId ::: JSON -> Galley Response
deleteTeamConversation (zusr::: zcon ::: tid ::: cid ::: _) = do
    tmems <- Data.teamMembers tid
    void $ permissionCheck zusr DeleteConversation tmems
    cmems <- Data.members cid
    now <- liftIO getCurrentTime
    let te = newEvent Teams.ConvDelete tid now & eventData .~ Just (Teams.EdConvDelete cid)
    let ce = Conv.Event Conv.ConvDelete cid zusr now Nothing
    let tr = list1 (userRecipient zusr) (membersToRecipients (Just zusr) tmems)
    let p  = newPush1 zusr (TeamEvent te) tr & pushConn .~ Just zcon
    case map recipient (nonTeamMembers cmems tmems) of
        []     -> push1 p
        (m:mm) -> pushSome [p, newPush1 zusr (ConvEvent ce) (list1 m mm) & pushConn .~ Just zcon]
    Data.removeTeamConv tid cid
    pure empty

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
withTeamIds :: UserId
            -> Maybe (Either (Range 1 32 (List TeamId)) TeamId)
            -> Range 1 100 Int32
            -> (Bool -> [TeamId] -> Galley Response)
            -> Galley Response
withTeamIds usr range size k = case range of
    Nothing        -> do
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
    e  <- ask
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
    when ((tdTeam team)^.teamBinding == Binding) $
        throwM noAddToBinding

-- ensure that the permissions are not "greater" than the user's copy permissions
-- this is used to ensure users cannot "elevate" permissions
ensureNotElevated :: Permissions -> TeamMember -> Galley ()
ensureNotElevated targetPermissions member =
  unless ((targetPermissions^.self)
           `Set.isSubsetOf` (member^.permissions.copy)) $
    throwM invalidPermissions

addTeamMemberInternal :: TeamId -> Maybe UserId -> Maybe ConnId -> NewTeamMember -> [TeamMember] -> Galley Response
addTeamMemberInternal tid origin originConn newMem mems = do
    o <- view options
    unless (length mems < o^.optSettings.setMaxTeamSize) $
        throwM tooManyTeamMembers
    let new = newMem^.ntmNewTeamMember
    Data.addTeamMember tid new
    cc <- filter (view managedConversation) <$> Data.teamConversations tid
    for_ cc $ \c ->
        Data.addMember (c^.conversationId) (new^.userId)
    now <- liftIO getCurrentTime
    let e = newEvent MemberJoin tid now & eventData .~ Just (EdMemberJoin (new^.userId))
    push1 $ newPush1 (new^.userId) (TeamEvent e) (r origin new) & pushConn .~ originConn
    pure empty
  where
    r (Just o) n = list1 (userRecipient o)           (membersToRecipients (Just o) (n : mems))
    r Nothing  n = list1 (userRecipient (n^.userId)) (membersToRecipients Nothing  (n : mems))

finishCreateTeam :: Team -> TeamMember -> [TeamMember] -> Maybe ConnId -> Galley Response
finishCreateTeam team owner others zcon = do
    let zusr = owner^.userId
    for_ (owner : others) $
        Data.addTeamMember (team^.teamId)
    now <- liftIO getCurrentTime
    let e = newEvent TeamCreate (team^.teamId) now & eventData .~ Just (EdTeamCreate team)
    let r = membersToRecipients Nothing others
    push1 $ newPush1 zusr (TeamEvent e) (list1 (userRecipient zusr) r) & pushConn .~ zcon
    pure (empty & setStatus status201 . location (team^.teamId))

withBindingTeam :: UserId -> (TeamId -> Galley b) -> Galley b
withBindingTeam zusr callback = do
    tid <- Data.oneUserTeam zusr >>= ifNothing teamNotFound
    binding <- Data.teamBinding tid >>= ifNothing teamNotFound
    case binding of
        Binding -> callback tid
        NonBinding -> throwM nonBindingTeam

getBindingTeamId :: UserId -> Galley Response
getBindingTeamId zusr = withBindingTeam zusr $ pure . json

getBindingTeamMembers :: UserId -> Galley Response
getBindingTeamMembers zusr = withBindingTeam zusr $ \tid -> do
    members <- Data.teamMembers tid
    pure $ json $ teamMemberListJson True (newTeamMemberList members)
