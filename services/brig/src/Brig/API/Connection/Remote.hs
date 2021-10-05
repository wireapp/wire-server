-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Tagged
import Data.UUID.V4
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

data ConnectionAction
  = LCA LocalConnectionAction
  | RCA RemoteConnectionAction

-- Inventory of states on both backends.
--
-- good: the two backends agree on the state of the connection
-- bad: no user action can result in the (A,A) state being reached
-- inconsistent: the two backends disagree on the state of the connection, but
--   it is not a bad state
--
-- A A good
-- A B good
-- A C inconsistent
-- A I inconsistent
-- A P inconsistent
-- A S bad
-- B B good
-- B C good
-- B I good
-- B P good
-- B S good
-- C C good
-- C I inconsistent
-- C P inconsistent
-- C S inconsistent
-- I I good
-- I P inconsistent
-- I S good
-- P P inconsistent
-- P S good
-- S S bad

-- LC (local connect): A communicates that they now want to connect. This transitions P → A, and every other state (but including S) to S.
-- LB (local block): A communicates that they do not want to connect. This transitions every state except B to B.
-- LI (local ignore): A ignores the connection. P → I.
-- LR (local rescind): A withdraws their intention to connect. S → C, A → P.
-- RC (remote connect): B communicates that they now want to connect. S → A, C → P, A → A.
-- RR (remote rescind): B withdraws their intention to connect. P → C, A → S.
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
  AppIO (Qualified ConvId)
updateOne2OneConv _ _ _ _ _ = do
  -- FUTUREWORK: use galley internal API to update 1-1 conversation and retrieve ID
  uid <- liftIO nextRandom
  unTagged <$> qualifyLocal (Id uid)

-- | Perform a state transition on a connection, handle conversation updates
-- and push events.
--
-- Returns the connection, and whether it was updated or not.
transitionTo ::
  Local UserId ->
  Maybe ConnId ->
  Remote UserId ->
  Maybe UserConnection ->
  Maybe Relation ->
  ConnectionM (ResponseForExistedCreated UserConnection, Bool)
transitionTo self _ _ Nothing Nothing =
  -- This can only happen if someone tries to ignore as a first action on a
  -- connection. This shouldn't be possible.
  throwE (InvalidTransition (lUnqualified self))
transitionTo self mzcon other Nothing (Just rel) = do
  -- enforce connection limit
  when (rel `elem` [Accepted, Sent]) $
    checkLimit self

  lift $ do
    -- update 1-1 connection
    qcnv <- updateOne2OneConv self mzcon other Nothing rel

    -- create connection
    connection <-
      Data.insertConnection
        self
        (unTagged other)
        (relationWithHistory rel)
        qcnv

    -- send event
    pushEvent self mzcon connection
    pure (Created connection, True)
transitionTo _self _zcon _other (Just connection) Nothing = pure (Existed connection, False)
transitionTo self mzcon other (Just connection) (Just rel) = do
  -- enforce connection limit
  when (notElem (ucStatus connection) [Accepted, Sent] && (rel `elem` [Accepted, Sent])) $
    checkLimit self
  lift $ do
    -- update 1-1 connection
    void $ updateOne2OneConv self Nothing other (ucConvId connection) rel

    -- update connection
    connection' <- Data.updateConnection connection (relationWithHistory rel)

    -- send event
    pushEvent self mzcon connection'
    pure (Existed connection', True)

-- | Send an event to the local user when the state of a connection changes.
pushEvent :: Local UserId -> Maybe ConnId -> UserConnection -> AppIO ()
pushEvent self mzcon connection = do
  let event = ConnectionUpdated connection Nothing Nothing
  Intra.onConnectionEvent (lUnqualified self) mzcon event

performLocalAction ::
  Local UserId ->
  Maybe ConnId ->
  Remote UserId ->
  Maybe UserConnection ->
  LocalConnectionAction ->
  ConnectionM (ResponseForExistedCreated UserConnection, Bool)
performLocalAction self mzcon other mconnection action = do
  let rel0 = maybe Cancelled ucStatus mconnection
  mrel2 <- for (transition (LCA action) rel0) $ \rel1 -> do
    mreaction <- fmap join . for (remoteAction action) $ \ra -> do
      response <- sendConnectionAction self other ra !>> ConnectFederationError
      case (response :: NewConnectionResponse) of
        NewConnectionResponseOk reaction -> pure reaction
        NewConnectionResponseUserNotActivated -> throwE (InvalidUser (unTagged other))
    pure $
      fromMaybe rel1 $ do
        reactionAction <- (mreaction :: Maybe RemoteConnectionAction)
        transition (RCA reactionAction) rel1
  transitionTo self mzcon other mconnection mrel2
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
--
--              A         B
-- A connects:  Sent      Pending
-- B ignores:   Sent      Ignore
-- B connects:  Accepted  Sent
--
--
-- Using the reaction returned by A
--
--                         A         B
-- A connects:             Sent      Pending
-- B ignores:              Sent      Ignore
-- B connects & A reacts:  Accepted  Accepted
performRemoteAction ::
  Local UserId ->
  Remote UserId ->
  Maybe UserConnection ->
  RemoteConnectionAction ->
  AppIO (Maybe RemoteConnectionAction)
performRemoteAction self other mconnection action = do
  let rel0 = maybe Cancelled ucStatus mconnection
      rel1 = transition (RCA action) rel0
  result <- runExceptT . void $ transitionTo self Nothing other mconnection rel1
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
  ConnectionM (ResponseForExistedCreated UserConnection)
createConnectionToRemoteUser self zcon other = do
  mconnection <- lift $ Data.lookupConnection self (unTagged other)
  fst <$> performLocalAction self (Just zcon) other mconnection LocalConnect

updateConnectionToRemoteUser ::
  Local UserId ->
  Remote UserId ->
  Relation ->
  Maybe ConnId ->
  ConnectionM (Maybe UserConnection)
updateConnectionToRemoteUser self other rel1 zcon = do
  mconnection <- lift $ Data.lookupConnection self (unTagged other)
  action <-
    actionForTransition rel1
      ?? InvalidTransition (lUnqualified self)
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
