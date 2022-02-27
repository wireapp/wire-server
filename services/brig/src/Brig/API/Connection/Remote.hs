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

module Brig.API.Connection.Remote
  ( performLocalAction,
    performRemoteAction,
    createConnectionToRemoteUser,
    updateConnectionToRemoteUser,
  )
where

import Brig.API.Connection.Util (ConnectionM, checkLimit)
import Brig.API.Types (ConnectionError (..))
import Brig.App
import qualified Brig.Data.Connection as Data
import Brig.Federation.Client (sendConnectionAction)
import qualified Brig.IO.Intra as Intra
import Brig.Types
import Brig.Types.User.Event
import Control.Comonad
import Control.Error.Util ((??))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Id as Id
import Data.Qualified
import Galley.Types.Conversations.Intra (Actor (..), DesiredMembership (..), UpsertOne2OneConversationRequest (..), UpsertOne2OneConversationResponse (uuorConvId))
import Imports
import Network.Wai.Utilities.Error
import Wire.API.Connection (relationWithHistory)
import Wire.API.Federation.API.Brig
  ( NewConnectionResponse (..),
    RemoteConnectionAction (..),
  )
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))

data LocalConnectionAction
  = LocalConnect
  | LocalBlock
  | LocalIgnore
  | LocalRescind
  deriving (Eq)

data ConnectionAction
  = LCA LocalConnectionAction
  | RCA RemoteConnectionAction

-- | Connection state transition logic.
--
-- In the following, A is a local user, and B is a remote user.
--
-- LocalConnect: A communicates that they now want to connect. This
--   transitions Pending → Accepted, and every other state (but including Sent) to Sent.
-- LocalBlock: A communicates that they do not want to connect. This
--   transitions every state except Blocked to Blocked.
-- LocalIgnore: A ignores the connection. Pending → Ignored.
-- LocalRescind: A withdraws their intention to connect. Sent → Cancelled, Accepted → Pending.
-- RemoteConnect: B communicates that they now want to connect. Sent → Accepted, Cancelled → Pending, Accepted → Accepted.
-- RemoteRescind: B withdraws their intention to connect. Pending → Cancelled, Accepted → Sent.
--
-- Returns 'Nothing' if no transition is possible from the current state for
-- the given action. This results in an 'InvalidTransition' error if the
-- connection does not exist.
transition :: ConnectionAction -> Relation -> Maybe Relation
-- MissingLegalholdConsent is treated exactly like blocked
transition action MissingLegalholdConsent = transition action Blocked
transition (LCA LocalConnect) Pending = Just Accepted
transition (LCA LocalConnect) Accepted = Just Accepted
transition (LCA LocalConnect) _ = Just Sent
transition (LCA LocalBlock) Blocked = Nothing
transition (LCA LocalBlock) _ = Just Blocked
transition (LCA LocalIgnore) Pending = Just Ignored
transition (LCA LocalIgnore) _ = Nothing
transition (LCA LocalRescind) Sent = Just Cancelled
-- The following transition is to make sure we always end up in state P
-- when we start in S and receive the two actions RC and LR in an arbitrary
-- order.
transition (LCA LocalRescind) Accepted = Just Pending
transition (LCA LocalRescind) _ = Nothing
transition (RCA RemoteConnect) Sent = Just Accepted
transition (RCA RemoteConnect) Accepted = Just Accepted
transition (RCA RemoteConnect) Blocked = Nothing
transition (RCA RemoteConnect) _ = Just Pending
transition (RCA RemoteRescind) Pending = Just Cancelled
-- The following transition is to make sure we always end up in state S
-- when we start in P and receive the two actions LC and RR in an arbitrary
-- order.
transition (RCA RemoteRescind) Accepted = Just Sent
transition (RCA RemoteRescind) _ = Nothing

-- When user A has made a request -> Only user A's membership in conv is affected -> User A wants to be in one2one conv with B, or User A doesn't want to be in one2one conv with B
updateOne2OneConv ::
  Local UserId ->
  Maybe ConnId ->
  Remote UserId ->
  Maybe (Qualified ConvId) ->
  Relation ->
  Actor ->
  (AppIO r) (Qualified ConvId)
updateOne2OneConv lUsr _mbConn remoteUser mbConvId rel actor = do
  let request =
        UpsertOne2OneConversationRequest
          { uooLocalUser = lUsr,
            uooRemoteUser = remoteUser,
            uooActor = actor,
            uooActorDesiredMembership = desiredMembership actor rel,
            uooConvId = mbConvId
          }
  uuorConvId <$> Intra.upsertOne2OneConversation request
  where
    desiredMembership :: Actor -> Relation -> DesiredMembership
    desiredMembership a r =
      let isIncluded =
            a
              `elem` case r of
                Accepted -> [LocalActor, RemoteActor]
                Blocked -> []
                Pending -> [RemoteActor]
                Ignored -> [RemoteActor]
                Sent -> [LocalActor]
                Cancelled -> []
                MissingLegalholdConsent -> []
       in if isIncluded then Included else Excluded

-- | Perform a state transition on a connection, handle conversation updates and
-- push events.
--
-- NOTE: This function does not check whether the max connection limit has been
-- reached, the consumers must ensure of this.
--
-- Returns the connection, and whether it was updated or not.
transitionTo ::
  Local UserId ->
  Maybe ConnId ->
  Remote UserId ->
  Maybe UserConnection ->
  Maybe Relation ->
  Actor ->
  (ConnectionM r) (ResponseForExistedCreated UserConnection, Bool)
transitionTo self _ _ Nothing Nothing _ =
  -- This can only happen if someone tries to ignore as a first action on a
  -- connection. This shouldn't be possible.
  throwE (InvalidTransition (tUnqualified self))
transitionTo self mzcon other Nothing (Just rel) actor = lift $ do
  -- update 1-1 connection
  qcnv <- updateOne2OneConv self mzcon other Nothing rel actor

  -- create connection
  connection <-
    wrapClient $ Data.insertConnection
      self
      (qUntagged other)
      (relationWithHistory rel)
      qcnv

  -- send event
  pushEvent self mzcon connection
  pure (Created connection, True)
transitionTo _self _zcon _other (Just connection) Nothing _actor = pure (Existed connection, False)
transitionTo self mzcon other (Just connection) (Just rel) actor = lift $ do
  -- update 1-1 conversation
  void $ updateOne2OneConv self Nothing other (ucConvId connection) rel actor

  -- update connection
  connection' <- wrapClient $ Data.updateConnection connection (relationWithHistory rel)

  -- send event
  pushEvent self mzcon connection'
  pure (Existed connection', True)

-- | Send an event to the local user when the state of a connection changes.
pushEvent :: Local UserId -> Maybe ConnId -> UserConnection -> (AppIO r) ()
pushEvent self mzcon connection = do
  let event = ConnectionUpdated connection Nothing Nothing
  Intra.onConnectionEvent (tUnqualified self) mzcon event

performLocalAction ::
  Local UserId ->
  Maybe ConnId ->
  Remote UserId ->
  Maybe UserConnection ->
  LocalConnectionAction ->
  (ConnectionM r) (ResponseForExistedCreated UserConnection, Bool)
performLocalAction self mzcon other mconnection action = do
  let rel0 = maybe Cancelled ucStatus mconnection
  checkLimitForLocalAction self rel0 action
  mrel2 <- for (transition (LCA action) rel0) $ \rel1 -> do
    mreaction <- fmap join . for (remoteAction action) $ \ra -> do
      response <- sendConnectionAction self other ra !>> ConnectFederationError
      case (response :: NewConnectionResponse) of
        NewConnectionResponseOk reaction -> pure reaction
        NewConnectionResponseUserNotActivated -> throwE (InvalidUser (qUntagged other))
    pure $
      fromMaybe rel1 $ do
        reactionAction <- (mreaction :: Maybe RemoteConnectionAction)
        transition (RCA reactionAction) rel1
  transitionTo self mzcon other mconnection mrel2 LocalActor
  where
    remoteAction :: LocalConnectionAction -> Maybe RemoteConnectionAction
    remoteAction LocalConnect = Just RemoteConnect
    remoteAction LocalRescind = Just RemoteRescind
    remoteAction _ = Nothing

-- | The 'RemoteConnectionAction' "reaction" that may be returned is processed
-- by the remote caller. This extra action allows to automatically resolve some
-- inconsistent states, for example:
--
-- Without any reaction
-- @
--              A         B
-- A connects:  Sent      Pending
-- B ignores:   Sent      Ignore
-- B connects:  Accepted  Sent
-- @
--
-- Using the reaction returned by A
--
-- @
--                         A         B
-- A connects:             Sent      Pending
-- B ignores:              Sent      Ignore
-- B connects & A reacts:  Accepted  Accepted
-- @
performRemoteAction ::
  Local UserId ->
  Remote UserId ->
  Maybe UserConnection ->
  RemoteConnectionAction ->
  (AppIO r) (Maybe RemoteConnectionAction)
performRemoteAction self other mconnection action = do
  let rel0 = maybe Cancelled ucStatus mconnection
  let rel1 = transition (RCA action) rel0
  result <- runExceptT . void $ transitionTo self Nothing other mconnection rel1 RemoteActor
  pure $ either (const (Just RemoteRescind)) (const (reaction rel1)) result
  where
    reaction :: Maybe Relation -> Maybe RemoteConnectionAction
    reaction (Just Accepted) = Just RemoteConnect
    reaction (Just Sent) = Just RemoteConnect
    reaction _ = Nothing

createConnectionToRemoteUser ::
  Local UserId ->
  ConnId ->
  Remote UserId ->
  (ConnectionM r) (ResponseForExistedCreated UserConnection)
createConnectionToRemoteUser self zcon other = do
  mconnection <- lift . wrapClient $ Data.lookupConnection self (qUntagged other)
  fst <$> performLocalAction self (Just zcon) other mconnection LocalConnect

updateConnectionToRemoteUser ::
  Local UserId ->
  Remote UserId ->
  Relation ->
  Maybe ConnId ->
  (ConnectionM r) (Maybe UserConnection)
updateConnectionToRemoteUser self other rel1 zcon = do
  mconnection <- lift . wrapClient $ Data.lookupConnection self (qUntagged other)
  action <-
    actionForTransition rel1
      ?? InvalidTransition (tUnqualified self)
  (conn, wasUpdated) <- performLocalAction self zcon other mconnection action
  pure $ guard wasUpdated $> extract conn
  where
    actionForTransition Cancelled = Just LocalRescind
    actionForTransition Sent = Just LocalConnect
    actionForTransition Accepted = Just LocalConnect
    actionForTransition Blocked = Just LocalBlock
    actionForTransition Ignored = Just LocalIgnore
    actionForTransition Pending = Nothing
    actionForTransition MissingLegalholdConsent = Nothing

checkLimitForLocalAction :: Local UserId -> Relation -> LocalConnectionAction -> (ConnectionM r) ()
checkLimitForLocalAction u oldRel action =
  when (oldRel `notElem` [Accepted, Sent] && (action == LocalConnect)) $
    checkLimit u
