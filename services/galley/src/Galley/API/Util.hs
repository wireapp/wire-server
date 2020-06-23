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

module Galley.API.Util where

import Brig.Types (Relation (..))
import Brig.Types.Intra (ReAuthUser (..))
import Control.Lens (view, (.~), (^.))
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Domain (Domain)
import Data.Id as Id
import Data.IdMapping (MappedOrLocalId (Local, Mapped), partitionMappedOrLocalIds)
import Data.List.NonEmpty (nonEmpty)
import Data.Misc (PlainTextPassword (..))
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
import Data.Time
import Galley.API.Error
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.Services (BotMember, newBotMember)
import qualified Galley.Data.Types as DataTypes
import Galley.Intra.Push
import Galley.Intra.User
import Galley.Options (optSettings, setEnableFederationWithDomain)
import Galley.Types
import Galley.Types.Conversations.Roles
import Galley.Types.Teams
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error)
import Network.Wai.Utilities
import qualified System.Logger.Class as Log
import UnliftIO (concurrently)

type JSON = Media "application" "json"

ensureAccessRole :: AccessRole -> [(UserId, Maybe TeamMember)] -> Galley ()
ensureAccessRole role users = case role of
  PrivateAccessRole -> throwM convAccessDenied
  TeamAccessRole ->
    when (any (isNothing . snd) users) $
      throwM notATeamMember
  ActivatedAccessRole -> do
    activated <- lookupActivatedUsers $ map fst users
    when (length activated /= length users) $
      throwM convAccessDenied
  NonActivatedAccessRole -> return ()

-- | Check that the given user is either part of the same team(s) as the other
-- users OR that there is a connection.
--
-- Team members are always considered connected, so we only check 'ensureConnected'
-- for non-team-members of the _given_ user
ensureConnectedOrSameTeam :: UserId -> [UserId] -> Galley ()
ensureConnectedOrSameTeam _ [] = pure ()
ensureConnectedOrSameTeam u uids = do
  uTeams <- Data.userTeams u
  -- We collect all the relevant uids from same teams as the origin user
  sameTeamUids <- forM uTeams $ \team ->
    fmap (view userId) <$> Data.teamMembersLimited team uids
  -- Do not check connections for users that are on the same team
  -- FUTUREWORK(federation, #1262): handle remote users (can't be part of the same team, just check connections)
  ensureConnected u (Local <$> uids \\ join sameTeamUids)

-- | Check that the user is connected to everybody else.
--
-- The connection has to be bidirectional (e.g. if A connects to B and later
-- B blocks A, the status of A-to-B is still 'Accepted' but it doesn't mean
-- that they are connected).
ensureConnected :: UserId -> [MappedOrLocalId Id.U] -> Galley ()
ensureConnected _ [] = pure ()
ensureConnected u mappedOrLocalUserIds = do
  let (localUserIds, remoteUserIds) = partitionMappedOrLocalIds mappedOrLocalUserIds
  -- FUTUREWORK(federation, #1262): check remote connections
  for_ (nonEmpty remoteUserIds) $
    throwM . federationNotImplemented
  ensureConnectedToLocals u localUserIds

ensureConnectedToLocals :: UserId -> [UserId] -> Galley ()
ensureConnectedToLocals _ [] = pure ()
ensureConnectedToLocals u uids = do
  (connsFrom, connsTo) <-
    getConnections [u] uids (Just Accepted)
      `concurrently` getConnections uids [u] (Just Accepted)
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
ensureActionAllowed :: Action -> InternalMember a -> Galley ()
ensureActionAllowed action mem = case isActionAllowed action (memConvRoleName mem) of
  Just True -> return ()
  Just False -> throwM (actionDenied action)
  Nothing -> throwM (badRequest "Custom roles not supported")

-- Actually, this will "never" happen due to the
-- fact that there can be no custom roles at the moment

-- | Ensure that the set of actions provided are not "greater" than the user's
--   own. This is used to ensure users cannot "elevate" allowed actions
--   This function needs to be review when custom roles are introduced since only
--   custom roles can cause `roleNameToActions` to return a Nothing
ensureConvRoleNotElevated :: InternalMember a -> RoleName -> Galley ()
ensureConvRoleNotElevated origMember targetRole = do
  case (roleNameToActions targetRole, roleNameToActions (memConvRoleName origMember)) of
    (Just targetActions, Just memberActions) ->
      unless (Set.isSubsetOf targetActions memberActions) $
        throwM invalidActions
    (_, _) ->
      throwM (badRequest "Custom roles not supported")

-- | If a team memeber is not given throw 'notATeamMember'; if the given team
-- member does not have the given permission, throw 'operationDenied'.
-- Otherwise, return unit.
permissionCheck :: (IsPerm perm, Show perm) => perm -> Maybe TeamMember -> Galley TeamMember
permissionCheck p = \case
  Just m -> do
    if m `hasPermission` p
      then pure m
      else throwM (operationDenied p)
  Nothing -> throwM notATeamMember

assertOnTeam :: UserId -> TeamId -> Galley ()
assertOnTeam uid tid = do
  Data.teamMember tid uid >>= \case
    Nothing -> throwM notATeamMember
    Just _ -> return ()

-- | If the conversation is in a team, throw iff zusr is a team member and does not have named
-- permission.  If the conversation is not in a team, do nothing (no error).
permissionCheckTeamConv :: UserId -> ConvId -> Perm -> Galley ()
permissionCheckTeamConv zusr cnv perm =
  Data.conversation cnv >>= \case
    Just cnv' -> case Data.convTeam cnv' of
      Just tid -> void $ permissionCheck perm =<< Data.teamMember tid zusr
      Nothing -> pure ()
    Nothing -> throwM convNotFound

-- | Try to accept a 1-1 conversation, promoting connect conversations as appropriate.
acceptOne2One :: UserId -> Data.Conversation -> Maybe ConnId -> Galley Data.Conversation
acceptOne2One usr conv conn = case Data.convType conv of
  One2OneConv ->
    if Local usr `isMember` mems
      then return conv
      else do
        now <- liftIO getCurrentTime
        mm <- snd <$> Data.addMember now cid usr
        return $ conv {Data.convMembers = mems <> toList mm}
  ConnectConv -> case mems of
    [_, _] | Local usr `isMember` mems -> promote
    [_, _] -> throwM convNotFound
    _ -> do
      when (length mems > 2) $
        throwM badConvState
      now <- liftIO getCurrentTime
      (e, mm) <- Data.addMember now cid usr
      conv' <- if isJust (find ((Local usr /=) . memId) mems) then promote else pure conv
      let mems' = mems <> toList mm
      for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> mems')) $ \p ->
        push1 $ p & pushConn .~ conn & pushRoute .~ RouteDirect
      return $ conv' {Data.convMembers = mems'}
  _ -> throwM $ invalidOp "accept: invalid conversation type"
  where
    cid = Data.convId conv
    mems = Data.convMembers conv
    promote = do
      Data.acceptConnect cid
      return $ conv {Data.convType = One2OneConv}
    badConvState =
      Error status500 "bad-state" $
        "Connect conversation with more than 2 members: "
          <> LT.pack (show cid)

isBot :: InternalMember a -> Bool
isBot = isJust . memService

isMember :: (Eq a, Foldable m) => a -> m (InternalMember a) -> Bool
isMember u = isJust . find ((u ==) . memId)

findMember :: Data.Conversation -> MappedOrLocalId Id.U -> Maybe Member
findMember c u = find ((u ==) . memId) (Data.convMembers c)

botsAndUsers :: (Log.MonadLogger m, Traversable t) => t Member -> m ([BotMember], [Member])
botsAndUsers = fmap fold . traverse botOrUser
  where
    botOrUser m = case memService m of
      Just _ -> do
        -- we drop invalid bots here, which shouldn't happen
        bot <- mkBotMember m
        pure (toList bot, [])
      Nothing ->
        pure ([], [m])
    mkBotMember :: Log.MonadLogger m => Member -> m (Maybe BotMember)
    mkBotMember m = case memId m of
      Mapped _ -> do
        Log.warn $ Log.msg @Text "Bot member with qualified user ID found, ignoring it."
        pure Nothing -- remote members can't be bots for now
      Local localMemId ->
        pure $ newBotMember (m {memId = localMemId} :: LocalMember)

location :: ToByteString a => a -> Response -> Response
location = addHeader hLocation . toByteString'

nonTeamMembers :: [Member] -> [TeamMember] -> [Member]
nonTeamMembers cm tm = filter (not . isMemberOfTeam . memId) cm
  where
    isMemberOfTeam = \case
      Local uid -> isTeamMember uid tm
      Mapped _ -> False -- teams and their members are always on the same backend

convMembsAndTeamMembs :: [Member] -> [TeamMember] -> [Recipient]
convMembsAndTeamMembs convMembs teamMembs =
  fmap userRecipient . setnub $ map memId convMembs <> map (Local . view userId) teamMembs
  where
    setnub = Set.toList . Set.fromList

membersToRecipients :: Maybe UserId -> [TeamMember] -> [Recipient]
membersToRecipients Nothing = map (userRecipient . Local . view userId)
membersToRecipients (Just u) = map (userRecipient . Local) . filter (/= u) . map (view userId)

-- | Note that we use 2 nearly identical functions but slightly different
-- semantics; when using `getSelfMember`, if that user is _not_ part of
-- the conversation, we don't want to disclose that such a conversation
-- with that id exists.
getSelfMember :: Foldable t => UserId -> t Member -> Galley LocalMember
getSelfMember = getMember convNotFound

getOtherMember :: Foldable t => UserId -> t Member -> Galley LocalMember
getOtherMember = getMember convMemberNotFound

-- | Since we search by local user ID, we know that the member must be local.
getMember :: Foldable t => Error -> UserId -> t Member -> Galley LocalMember
getMember ex u ms = do
  let member = find ((Local u ==) . memId) ms
  case member of
    Just m -> return (m {memId = u})
    Nothing -> throwM ex

getConversationAndCheckMembership :: UserId -> MappedOrLocalId Id.C -> Galley Data.Conversation
getConversationAndCheckMembership = getConversationAndCheckMembershipWithError convAccessDenied

getConversationAndCheckMembershipWithError :: Error -> UserId -> MappedOrLocalId Id.C -> Galley Data.Conversation
getConversationAndCheckMembershipWithError ex zusr = \case
  Mapped idMapping ->
    throwM . federationNotImplemented $ pure idMapping
  Local convId -> do
    -- should we merge resolving to qualified ID and looking up the conversation?
    c <- Data.conversation convId >>= ifNothing convNotFound
    when (DataTypes.isConvDeleted c) $ do
      Data.deleteConversation convId
      throwM convNotFound
    unless (Local zusr `isMember` Data.convMembers c) $
      throwM ex
    return c

-- | Deletion requires a permission check, but also a 'Role' comparison:
-- Owners can only be deleted by another owner (and not themselves).
--
-- FUTUREWORK: do not do this with 'Role', but introduce permissions "can delete owner", "can
-- delete admin", etc.
canDeleteMember :: TeamMember -> TeamMember -> Bool
canDeleteMember deleter deletee
  | getRole deletee == RoleOwner =
    getRole deleter == RoleOwner -- owners can only be deleted by another owner
      && (deleter ^. userId /= deletee ^. userId) -- owner cannot delete itself
  | otherwise =
    True
  where
    -- (team members having no role is an internal error, but we don't want to deal with that
    -- here, so we pick a reasonable default.)
    getRole mem = fromMaybe RoleMember $ permissionsRole $ mem ^. permissions

--------------------------------------------------------------------------------
-- Federation

viewFederationDomain :: Galley (Maybe Domain)
viewFederationDomain = view (options . optSettings . setEnableFederationWithDomain)

isFederationEnabled :: Galley Bool
isFederationEnabled = isJust <$> viewFederationDomain
