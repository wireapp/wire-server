module Galley.API.Util where

import Imports
import Brig.Types (Relation (..))
import Brig.Types.Intra (ReAuthUser (..))
import Control.Lens (view, (.~))
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Id
import Data.Misc (PlainTextPassword (..))
import Data.Time
import Galley.App
import Galley.API.Error
import Galley.Data.Services (BotMember, newBotMember)
import Galley.Intra.Push
import Galley.Intra.User
import Galley.Types
import Galley.Types.Conversations.Roles
import Galley.Types.Teams
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error)
import Network.Wai.Utilities
import UnliftIO (concurrently)

import qualified Data.Set          as Set
import qualified Data.Text.Lazy    as LT
import qualified Galley.Data       as Data
import qualified Galley.Data.Types as DataTypes

type JSON = Media "application" "json"

ensureAccessRole :: AccessRole -> [UserId] -> Maybe [TeamMember] -> Galley ()
ensureAccessRole role users mbTms = case role of
    PrivateAccessRole -> throwM convAccessDenied
    TeamAccessRole -> case mbTms of
        Nothing -> throwM internalError
        Just tms ->
            unless (null $ notTeamMember users tms) $
                throwM noTeamMember
    ActivatedAccessRole -> do
        activated <- lookupActivatedUsers users
        when (length activated /= length users) $ throwM convAccessDenied
    NonActivatedAccessRole -> return ()

-- | Check that the given user is either part of the same team(s) as the other
-- users OR that there is a connection.
--
-- Team members are always considered connected, so we only check 'ensureConnected'
-- for non-team-members of the _given_ user
ensureConnectedOrSameTeam :: UserId -> [UserId] -> Galley ()
ensureConnectedOrSameTeam _ []   = pure ()
ensureConnectedOrSameTeam u uids = do
    uTeams <- Data.userTeams u
    -- We collect all the relevant uids from same teams as the origin user
    sameTeamUids <- forM uTeams $ \team ->
        fmap (view userId) <$> Data.teamMembersLimited team uids
    -- Do not check connections for users that are on the same team
    ensureConnected u (uids \\ join sameTeamUids)

-- | Check that the user is connected to everybody else.
--
-- The connection has to be bidirectional (e.g. if A connects to B and later
-- B blocks A, the status of A-to-B is still 'Accepted' but it doesn't mean
-- that they are connected).
ensureConnected :: UserId -> [UserId] -> Galley ()
ensureConnected _ []   = pure ()
ensureConnected u uids = do
    (connsFrom, connsTo) <-
        getConnections [u] uids (Just Accepted) `concurrently`
        getConnections uids [u] (Just Accepted)
    unless (length connsFrom == length uids && length connsTo == length uids) $
        throwM notConnected

ensureReAuthorised :: UserId -> Maybe PlainTextPassword -> Galley ()
ensureReAuthorised u secret = do
    reAuthed <- reAuthUser u (ReAuthUser secret)
    unless reAuthed $
        throwM reAuthFailed

-- | Given a member in a conversation, check if the given action
-- is permitted.
-- If not, throw 'Member'; if the user is found and does not have the given permission, throw
-- 'operationDenied'.  Otherwise, return the found user.
ensureActionAllowed :: Action -> Member -> Galley ()
ensureActionAllowed action mem = case isActionAllowed action (memConvRoleName mem) of
    Just True  -> return ()
    Just False -> throwM (actionDenied action)
    Nothing    -> throwM (badRequest "Custom roles not supported")
               -- ^ Actually, this will "never" happen due to the
               --   fact that there can be no custom roles at the moment

-- | Ensure that the set of actions provided are not "greater" than the user's
--   own. This is used to ensure users cannot "elevate" allowed actions
--   This function needs to be review when custom roles are introduced since only
--   custom roles can cause `roleNameToActions` to return a Nothing
ensureConvRoleNotElevated :: Member -> RoleName -> Galley ()
ensureConvRoleNotElevated origMember targetRole = do
    case (roleNameToActions targetRole, roleNameToActions (memConvRoleName origMember)) of
         (Just targetActions, Just memberActions) ->
            unless (Set.isSubsetOf targetActions memberActions) $
                throwM invalidActions
         (_                 , _                 ) ->
            throwM (badRequest "Custom roles not supported")
            -- ^ Actually, this will "never" happen due to the
            --   fact that there can be no custom roles at the moment

bindingTeamMembers :: TeamId -> Galley [TeamMember]
bindingTeamMembers tid = do
    binding <- Data.teamBinding tid >>= ifNothing teamNotFound
    case binding of
        Binding -> Data.teamMembers tid
        NonBinding -> throwM nonBindingTeam

-- | Pick a team member with a given user id from some team members.  If the filter comes up empty,
-- throw 'noTeamMember'; if the user is found and does not have the given permission, throw
-- 'operationDenied'.  Otherwise, return the found user.
permissionCheck :: (Foldable m, IsPerm perm, Show perm) => UserId -> perm -> m TeamMember -> Galley TeamMember
permissionCheck u p t =
    case find ((u ==) . view userId) t of
        Just m -> do
            unless (m `hasPermission` p) $
                throwM (operationDenied p)
            pure m
        Nothing -> throwM noTeamMember

assertOnTeam :: UserId -> TeamId -> Galley ()
assertOnTeam uid tid = do
    members <- Data.teamMembers tid
    let isOnTeam = isJust $ find ((uid ==) . view userId) members
    unless isOnTeam (throwM noTeamMember)

-- | If the conversation is in a team, throw iff zusr is a team member and does not have named
-- permission.  If the conversation is not in a team, do nothing (no error).
permissionCheckTeamConv :: UserId -> ConvId -> Perm -> Galley ()
permissionCheckTeamConv zusr cnv perm = Data.conversation cnv >>= \case
    Just cnv' -> case Data.convTeam cnv' of
        Just tid -> void $ permissionCheck zusr perm =<< Data.teamMembers tid
        Nothing -> pure ()
    Nothing -> throwM convNotFound

-- | Try to accept a 1-1 conversation, promoting connect conversations as appropriate.
acceptOne2One :: UserId -> Data.Conversation -> Maybe ConnId -> Galley Data.Conversation
acceptOne2One usr conv conn = case Data.convType conv of
    One2OneConv ->
        if usr `isMember` mems then
            return conv
        else do
            now <- liftIO getCurrentTime
            mm  <- snd <$> Data.addMember now cid usr
            return $ conv { Data.convMembers = mems <> toList mm }
    ConnectConv -> case mems of
        [_,_] | usr `isMember` mems -> promote
        [_,_] -> throwM convNotFound
        _     -> do
            when (length mems > 2) $
                throwM badConvState
            now <- liftIO getCurrentTime
            (e, mm) <- Data.addMember now cid usr
            conv'   <- if isJust (find ((usr /=) . memId) mems) then promote else pure conv
            let mems' = mems <> toList mm
            for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> mems')) $ \p ->
                push1 $ p & pushConn  .~ conn & pushRoute .~ RouteDirect
            return $ conv' { Data.convMembers = mems' }
    _ -> throwM $ invalidOp "accept: invalid conversation type"
  where
    cid   = Data.convId conv
    mems  = Data.convMembers conv

    promote = do
        Data.acceptConnect cid
        return $ conv { Data.convType = One2OneConv }

    badConvState = Error status500 "bad-state"
                 $ "Connect conversation with more than 2 members: "
                <> LT.pack (show cid)

isBot :: Member -> Bool
isBot = isJust . memService

isMember :: Foldable m => UserId -> m Member -> Bool
isMember u = isJust . find ((u ==) . memId)

findMember :: Data.Conversation -> UserId -> Maybe Member
findMember c u = find ((u ==) . memId) (Data.convMembers c)

botsAndUsers :: Foldable t => t Member -> ([BotMember], [Member])
botsAndUsers = foldr fn ([], [])
  where
    fn m ~(bb, mm) = case newBotMember m of
        Nothing -> (bb, m:mm)
        Just  b -> (b:bb, mm)

location :: ToByteString a => a -> Response -> Response
location = addHeader hLocation . toByteString'

nonTeamMembers :: [Member] -> [TeamMember] -> [Member]
nonTeamMembers cm tm = filter (not . flip isTeamMember tm . memId) cm

convMembsAndTeamMembs :: [Member] -> [TeamMember] -> [Recipient]
convMembsAndTeamMembs convMembs teamMembs
    = fmap userRecipient . setnub $ map memId convMembs <> map (view userId) teamMembs
  where
    setnub = Set.toList . Set.fromList

membersToRecipients :: Maybe UserId -> [TeamMember] -> [Recipient]
membersToRecipients Nothing  = map (userRecipient . view userId)
membersToRecipients (Just u) = map userRecipient . filter (/= u) . map (view userId)

-- Note that we use 2 nearly identical functions but slightly different
-- semantics; when using `getSelfMember`, if that user is _not_ part of
-- the conversation, we don't want to disclose that such a conversation
-- with that id exists.
getSelfMember :: Foldable t => UserId -> t Member -> Galley Member
getSelfMember = getMember convNotFound

getOtherMember :: Foldable t => UserId -> t Member -> Galley Member
getOtherMember = getMember convMemberNotFound

getMember :: Foldable t => Error -> UserId -> t Member -> Galley Member
getMember ex u ms = do
    let member = find ((u ==) . memId) ms
    case member of
        Just m  -> return m
        Nothing -> throwM ex

getConversationAndCheckMembership :: UserId -> ConvId -> Galley Data.Conversation
getConversationAndCheckMembership = getConversationAndCheckMembershipWithError convAccessDenied

getConversationAndCheckMembershipWithError :: Error -> UserId -> ConvId -> Galley Data.Conversation
getConversationAndCheckMembershipWithError ex zusr cnv = do
    c <- Data.conversation cnv >>= ifNothing convNotFound
    when (DataTypes.isConvDeleted c) $ do
        Data.deleteConversation cnv
        throwM convNotFound
    unless (zusr `isMember` Data.convMembers c) $
        throwM ex
    return c
