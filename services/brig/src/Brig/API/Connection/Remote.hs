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

import Brig.API.Connection.Util
import Brig.API.Types (ConnectionError (..))
import Brig.App
import Brig.Data.Connection qualified as Data
import Brig.Data.User qualified as Data
import Brig.Effects.FederationConfigStore
import Brig.Effects.GalleyProvider
import Brig.Federation.Client as Federation
import Brig.IO.Intra qualified as Intra
import Brig.Options
import Brig.Types.User.Event
import Control.Comonad
import Control.Error.Util ((??))
import Control.Lens (view)
import Control.Monad.Trans.Except
import Data.Id as Id
import Data.Qualified
import Galley.Types.Conversations.One2One (one2OneConvId)
import Imports
import Network.Wai.Utilities.Error
import Polysemy
import Wire.API.Connection
import Wire.API.Federation.API.Brig
  ( NewConnectionResponse (..),
    RemoteConnectionAction (..),
  )
import Wire.API.Routes.Internal.Galley.ConversationsIntra
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))
import Wire.API.User
import Wire.NotificationSubsystem

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

-- When user A has made a request -> Only user A's membership in conv is
-- affected -> User A wants to be in one2one conv with B, or User A doesn't want
-- to be in one2one conv with B
updateOne2OneConv ::
  Local UserId ->
  Maybe ConnId ->
  Remote UserId ->
  Qualified ConvId ->
  DesiredMembership ->
  Actor ->
  (AppT r) ()
updateOne2OneConv lUsr _mbConn remoteUser convId desiredMem actor = do
  let request =
        UpsertOne2OneConversationRequest
          { uooLocalUser = lUsr,
            uooRemoteUser = remoteUser,
            uooActor = actor,
            uooActorDesiredMembership = desiredMem,
            uooConvId = convId
          }
  void $ wrapHttp (Intra.upsertOne2OneConversation request)

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
  (Member NotificationSubsystem r, Member GalleyProvider r) =>
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
  -- Create 1-1 proteus conversation.
  --
  -- We do nothing here for MLS as haveing no pre-existing connection implies
  -- there was no conversation. Creating an MLS converstaion is special due to
  -- key packages, etc. so the clients have to make another call for this.
  let proteusConv = one2OneConvId BaseProtocolProteusTag (tUntagged self) (tUntagged other)
  updateOne2OneConv self mzcon other proteusConv (desiredMembership actor rel) actor

  -- create connection
  connection <-
    wrapClient $
      Data.insertConnection
        self
        (tUntagged other)
        (relationWithHistory rel)
        proteusConv

  -- send event
  pushEvent self mzcon connection
  pure (Created connection, True)
transitionTo _self _zcon _other (Just connection) Nothing _actor = pure (Existed connection, False)
transitionTo self mzcon other (Just connection) (Just rel) actor = do
  -- update 1-1 conversation
  let proteusConvId =
        fromMaybe
          (one2OneConvId BaseProtocolProteusTag (tUntagged self) (tUntagged other))
          $ ucConvId connection
  lift $ updateOne2OneConv self Nothing other proteusConvId (desiredMembership actor rel) actor
  mlsEnabled <- view (settings . enableMLS)
  when (fromMaybe False mlsEnabled) $ do
    let mlsConvId = one2OneConvId BaseProtocolMLSTag (tUntagged self) (tUntagged other)
    mlsConvEstablished <- lift . liftSem $ isMLSOne2OneEstablished self (tUntagged other)
    let desiredMem = desiredMembership actor rel
    lift . when (mlsConvEstablished && desiredMem == Excluded) $
      updateOne2OneConv self Nothing other mlsConvId desiredMem actor

  -- update connection
  connection' <- lift $ wrapClient $ Data.updateConnection connection (relationWithHistory rel)

  -- send event
  lift $ pushEvent self mzcon connection'
  pure (Existed connection', True)

-- | Send an event to the local user when the state of a connection changes.
pushEvent ::
  (Member NotificationSubsystem r) =>
  Local UserId ->
  Maybe ConnId ->
  UserConnection ->
  AppT r ()
pushEvent self mzcon connection = do
  let event = ConnectionUpdated connection Nothing Nothing
  liftSem $ Intra.onConnectionEvent (tUnqualified self) mzcon event

performLocalAction ::
  (Member NotificationSubsystem r, Member GalleyProvider r) =>
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
      mSelfTeam <- lift . wrapClient . Data.lookupUserTeam . tUnqualified $ self
      response <-
        sendConnectionAction
          self
          (qualifyAs self <$> mSelfTeam)
          other
          ra
          !>> ConnectFederationError
      case (response :: NewConnectionResponse) of
        NewConnectionResponseOk reaction -> pure reaction
        NewConnectionResponseNotFederating -> throwE ConnectTeamFederationError
        NewConnectionResponseUserNotActivated -> throwE (InvalidUser (tUntagged other))
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
  (Member NotificationSubsystem r, Member GalleyProvider r) =>
  Local UserId ->
  Remote UserId ->
  Maybe UserConnection ->
  RemoteConnectionAction ->
  (AppT r) (Maybe RemoteConnectionAction)
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
  ( Member FederationConfigStore r,
    Member NotificationSubsystem r,
    Member GalleyProvider r
  ) =>
  Local UserId ->
  ConnId ->
  Remote UserId ->
  ConnectionM r (ResponseForExistedCreated UserConnection)
createConnectionToRemoteUser self zcon other = do
  ensureNotSameAndActivated self (tUntagged other)
  ensureFederatesWith other
  mconnection <- lift . wrapClient $ Data.lookupConnection self (tUntagged other)
  fst <$> performLocalAction self (Just zcon) other mconnection LocalConnect

updateConnectionToRemoteUser ::
  ( Member NotificationSubsystem r,
    Member FederationConfigStore r,
    Member GalleyProvider r
  ) =>
  Local UserId ->
  Remote UserId ->
  Relation ->
  Maybe ConnId ->
  (ConnectionM r) (Maybe UserConnection)
updateConnectionToRemoteUser self other rel1 zcon = do
  ensureFederatesWith other
  mconnection <- lift . wrapClient $ Data.lookupConnection self (tUntagged other)
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

-- | Check if the local backend federates with the remote user's team. Throw an
-- exception if it does not federate.
ensureFederatesWith ::
  Member FederationConfigStore r =>
  Remote UserId ->
  ConnectionM r ()
ensureFederatesWith remote = do
  profiles <-
    withExceptT ConnectFederationError $
      getUsersByIds (tDomain remote) [tUnqualified remote]
  let rTeam = qualifyAs remote $ profileTeam =<< listToMaybe profiles
  unlessM (lift . liftSem . backendFederatesWith $ rTeam) $
    throwE ConnectTeamFederationError
