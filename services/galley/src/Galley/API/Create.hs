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

-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.
module Galley.API.Create
  ( createGroupConversation,
    createSelfConversation,
    createConnectConversation,
  )
where

import Control.Lens hiding ((??))
import Data.Id
import Data.List1 (list1)
import Data.Misc (FutureWork (FutureWork))
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Time
import qualified Data.UUID.Tagged as U
import Galley.API.Error
import Galley.API.Mapping
import Galley.API.Util
import qualified Galley.Data.Conversation as Data
import Galley.Data.Conversation.Types
import Galley.Effects
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.GundeckAccess as E
import qualified Galley.Effects.MemberStore as E
import qualified Galley.Effects.TeamStore as E
import Galley.Intra.Push
import Galley.Options
import Galley.Types.Conversations.Members
import Galley.Types.Teams (ListType (..), Perm (..), notTeamMember)
import Galley.Types.UserList
import Galley.Validation
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import Wire.API.Conversation hiding (Conversation, Member)
import qualified Wire.API.Conversation as Public
import Wire.API.ErrorDescription
import Wire.API.Event.Conversation hiding (Conversation)
import Wire.API.Federation.Error
import Wire.API.Routes.Public.Galley (ConversationResponse)
import Wire.API.Routes.Public.Util
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))

----------------------------------------------------------------------------
-- Group conversations

-- | The public-facing endpoint for creating group conversations.
--
-- See Note [managed conversations].
createGroupConversation ::
  Members
    '[ ConversationStore,
       BrigAccess,
       Error ActionError,
       Error ConversationError,
       Error InternalError,
       Error InvalidInput,
       Error LegalHoldError,
       Error NotATeamMember,
       FederatorAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  NewConv ->
  Sem r ConversationResponse
createGroupConversation lusr conn body =
  let create = case newConvTeam body of
        Nothing -> createGroupConversation' ()
        Just tinfo -> createGroupConversation' tinfo
   in create lusr conn body

ensureNoLegalholdConflicts ::
  Members '[Error LegalHoldError, Input Opts, LegalHoldStore, TeamStore] r =>
  [Remote UserId] ->
  [UserId] ->
  Sem r ()
ensureNoLegalholdConflicts remotes locals = do
  let FutureWork _remotes = FutureWork @'LegalholdPlusFederationNotImplemented remotes
  whenM (anyLegalholdActivated locals) $
    unlessM (allLegalholdConsentGiven locals) $
      throw MissingLegalholdConsent

class CreateTeamGroupConv a where
  type CreateTeamGroupConvEffects a :: [Effect]
  checkCreateConvPermissions ::
    Members (CreateTeamGroupConvEffects a) r =>
    Local UserId ->
    NewConv ->
    a ->
    Sem r ()

instance CreateTeamGroupConv () where
  type
    CreateTeamGroupConvEffects () =
      '[ BrigAccess,
         Error ActionError
       ]
  checkCreateConvPermissions lusr newConv () =
    ensureConnected lusr (newConvMembers lusr newConv)

instance CreateTeamGroupConv ConvTeamInfo where
  type
    CreateTeamGroupConvEffects ConvTeamInfo =
      '[ BrigAccess,
         Error ActionError,
         Error ConversationError,
         Error NotATeamMember,
         TeamStore
       ]
  checkCreateConvPermissions lusr body tinfo = do
    let allUsers = newConvMembers lusr body
        convTeam = cnvTeamId tinfo
    zusrMembership <- E.getTeamMember convTeam (tUnqualified lusr)
    void $ permissionCheck CreateConversation zusrMembership
    convLocalMemberships <- mapM (E.getTeamMember convTeam) (ulLocals allUsers)
    ensureAccessRole (accessRoles body) (zip (ulLocals allUsers) convLocalMemberships)
    -- In teams we don't have 1:1 conversations, only regular conversations. We want
    -- users without the 'AddRemoveConvMember' permission to still be able to create
    -- regular conversations, therefore we check for 'AddRemoveConvMember' only if
    -- there are going to be more than two users in the conversation.
    -- FUTUREWORK: We keep this permission around because not doing so will break backwards
    -- compatibility in the sense that the team role 'partners' would be able to create group
    -- conversations (which they should not be able to).
    -- Not sure at the moment how to best solve this but it is unlikely
    -- we can ever get rid of the team permission model anyway - the only thing I can
    -- think of is that 'partners' can create convs but not be admins...
    when (length allUsers > 1) $ do
      void $ permissionCheck DoNotUseDeprecatedAddRemoveConvMember zusrMembership
      -- Team members are always considered to be connected, so we only check
      -- 'ensureConnected' for non-team-members.

      ensureConnectedToLocals (tUnqualified lusr) (notTeamMember (ulLocals allUsers) (catMaybes convLocalMemberships))
      ensureConnectedToRemotes lusr (ulRemotes allUsers)

-- | A helper for creating a group conversation, used by the endpoint handlers
-- above. It works both for team and non-team convrsation. Only supports
-- unmanaged conversations.
createGroupConversation' ::
  CreateTeamGroupConv a =>
  Members (CreateTeamGroupConvEffects a) r =>
  Members
    '[ ConversationStore,
       Error InternalError,
       Error InvalidInput,
       Error LegalHoldError,
       FederatorAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       P.TinyLog,
       TeamStore
     ]
    r =>
  a ->
  Local UserId ->
  ConnId ->
  Public.NewConv ->
  Sem r ConversationResponse
createGroupConversation' tinfo lusr zcon body = do
  name <- rangeCheckedMaybe (newConvName body)
  checkCreateConvPermissions lusr body tinfo
  let allUsers = newConvMembers lusr body
  o <- input
  checkedUsers <- checkedConvSize o allUsers
  ensureNoLegalholdConflicts (ulRemotes allUsers) (ulLocals allUsers)
  conv <-
    E.createConversation
      NewConversation
        { ncType = RegularConv,
          ncCreator = tUnqualified lusr,
          ncAccess = access body,
          ncAccessRoles = accessRoles body,
          ncName = name,
          ncTeam = fmap cnvTeamId (newConvTeam body),
          ncMessageTimer = newConvMessageTimer body,
          ncReceiptMode = newConvReceiptMode body,
          ncUsers = checkedUsers,
          ncRole = newConvUsersRole body
        }
  now <- input
  -- NOTE: We only send (conversation) events to members of the conversation
  notifyCreatedConversation (Just now) lusr (Just zcon) conv
  conversationCreated lusr conv

----------------------------------------------------------------------------
-- Other kinds of conversations

createSelfConversation ::
  forall r.
  Members '[ConversationStore, Error InternalError, P.TinyLog] r =>
  Local UserId ->
  Sem r ConversationResponse
createSelfConversation lusr = do
  c <- E.getConversation (Id . toUUID . tUnqualified $ lusr)
  maybe create (conversationExisted lusr) c
  where
    create :: Sem r ConversationResponse
    create = do
      c <- E.createSelfConversation lusr Nothing
      conversationCreated lusr c

createConnectConversation ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InternalError,
       Error InvalidInput,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  Maybe ConnId ->
  Connect ->
  Sem r ConversationResponse
createConnectConversation lusr conn j = do
  lrecipient <- ensureLocal lusr (cRecipient j)
  (x, y) <- toUUIDs (tUnqualified lusr) (tUnqualified lrecipient)
  n <- rangeCheckedMaybe (cName j)
  conv <- E.getConversation (Data.localOne2OneConvId x y)
  maybe (create x y n) (update n) conv
  where
    create x y n = do
      c <- E.createConnectConversation x y n
      now <- input
      let lcid = qualifyAs lusr (Data.convId c)
          e = Event ConvConnect (qUntagged lcid) (qUntagged lusr) now (EdConnect j)
      notifyCreatedConversation Nothing lusr conn c
      for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> Data.convLocalMembers c)) $ \p ->
        E.push1 $
          p
            & pushRoute .~ RouteDirect
            & pushConn .~ conn
      conversationCreated lusr c
    update n conv = do
      let mems = Data.convLocalMembers conv
       in conversationExisted lusr
            =<< if
                | (tUnqualified lusr) `isMember` mems ->
                  -- we already were in the conversation, maybe also other
                  connect n conv
                | otherwise -> do
                  let lcid = qualifyAs lusr (Data.convId conv)
                  mm <- E.createMember lcid lusr
                  let conv' =
                        conv
                          { Data.convLocalMembers = Data.convLocalMembers conv <> toList mm
                          }
                  if null mems
                    then do
                      -- the conversation was empty
                      connect n conv'
                    else do
                      -- we were not in the conversation, but someone else
                      conv'' <- acceptOne2One lusr conv' conn
                      if Data.convType conv'' == ConnectConv
                        then connect n conv''
                        else return conv''
    connect n conv
      | Data.convType conv == ConnectConv = do
        let lcnv = qualifyAs lusr (Data.convId conv)
        n' <- case n of
          Just x -> do
            E.setConversationName (Data.convId conv) x
            return . Just $ fromRange x
          Nothing -> return $ Data.convName conv
        t <- input
        let e = Event ConvConnect (qUntagged lcnv) (qUntagged lusr) t (EdConnect j)
        for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> Data.convLocalMembers conv)) $ \p ->
          E.push1 $
            p
              & pushRoute .~ RouteDirect
              & pushConn .~ conn
        return $ conv {Data.convName = n'}
      | otherwise = return conv

-------------------------------------------------------------------------------
-- Helpers

conversationCreated ::
  Members '[Error InternalError, P.TinyLog] r =>
  Local UserId ->
  Data.Conversation ->
  Sem r ConversationResponse
conversationCreated lusr cnv = Created <$> conversationView lusr cnv

conversationExisted ::
  Members '[Error InternalError, P.TinyLog] r =>
  Local UserId ->
  Data.Conversation ->
  Sem r ConversationResponse
conversationExisted lusr cnv = Existed <$> conversationView lusr cnv

notifyCreatedConversation ::
  Members '[Error InternalError, FederatorAccess, GundeckAccess, Input UTCTime, P.TinyLog] r =>
  Maybe UTCTime ->
  Local UserId ->
  Maybe ConnId ->
  Data.Conversation ->
  Sem r ()
notifyCreatedConversation dtime lusr conn c = do
  now <- maybe (input) pure dtime
  -- FUTUREWORK: Handle failures in notifying so it does not abort half way
  -- through (either when notifying remotes or locals)
  --
  -- Ask remote server to store conversation membership and notify remote users
  -- of being added to a conversation
  registerRemoteConversationMemberships now (tDomain lusr) c
  -- Notify local users
  E.push =<< mapM (toPush now) (Data.convLocalMembers c)
  where
    route
      | Data.convType c == RegularConv = RouteAny
      | otherwise = RouteDirect
    toPush t m = do
      let lconv = qualifyAs lusr (Data.convId c)
      c' <- conversationView (qualifyAs lusr (lmId m)) c
      let e = Event ConvCreate (qUntagged lconv) (qUntagged lusr) t (EdConversation c')
      return $
        newPushLocal1 ListComplete (tUnqualified lusr) (ConvEvent e) (list1 (recipient m) [])
          & pushConn .~ conn
          & pushRoute .~ route

toUUIDs ::
  Member (Error InvalidInput) r =>
  UserId ->
  UserId ->
  Sem r (U.UUID U.V4, U.UUID U.V4)
toUUIDs a b = do
  a' <- U.fromUUID (toUUID a) & note InvalidUUID4
  b' <- U.fromUUID (toUUID b) & note InvalidUUID4
  return (a', b')

accessRoles :: NewConv -> Set AccessRoleV2
accessRoles b = fromMaybe Data.defRole (newConvAccessRoles b)

access :: NewConv -> [Access]
access a = case Set.toList (newConvAccess a) of
  [] -> Data.defRegularConvAccess
  (x : xs) -> x : xs

newConvMembers :: Local x -> NewConv -> UserList UserId
newConvMembers loc body =
  UserList (newConvUsers body) []
    <> toUserList loc (newConvQualifiedUsers body)
