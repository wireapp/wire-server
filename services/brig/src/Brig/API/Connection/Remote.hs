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

module Brig.API.Connection.Remote (performLocalAction, performRemoteAction) where

import Brig.App
import qualified Brig.Data.Connection as Data
import qualified Brig.IO.Intra as Intra
import Brig.Types
import Data.Id as Id
import Data.Qualified
import Data.Tagged
import Imports
import Wire.API.Connection (relationWithHistory)

data LocalConnectionAction
  = LocalConnect
  | LocalBlock
  | LocalIgnore
  | LocalRescind

data RemoteConnectionAction
  = RemoteConnect
  | RemoteRescind

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

transitionTo :: Local UserId -> ConnId -> Remote UserId -> Maybe UserConnection -> Relation -> AppIO UserConnection
transitionTo self conn other mconnection rel = do
  connection <- case mconnection of
    Nothing -> do
      qcnv <- Intra.createConnectConv self (unTagged other) Nothing (Just conn)
      Data.insertConnection self (unTagged other) (relationWithHistory rel) qcnv
    Just connection ->
      Data.updateConnection connection (relationWithHistory rel)
  pure connection

performLocalAction ::
  Local UserId ->
  ConnId ->
  Remote UserId ->
  Maybe UserConnection ->
  LocalConnectionAction ->
  AppIO (Maybe UserConnection)
performLocalAction self conn other mconnection action = do
  let rel0 = maybe Cancelled ucStatus mconnection
  for (transition (LCA action) rel0) $ \rel1 -> do
    mreaction <- join <$> traverse (notifyRemote self other) (remoteAction action)
    let rel2 = fromMaybe rel1 $ do
          reactionAction <- mreaction
          transition (RCA reactionAction) rel1
    transitionTo self conn other mconnection rel2
  where
    remoteAction :: LocalConnectionAction -> Maybe RemoteConnectionAction
    remoteAction LocalConnect = Just RemoteConnect
    remoteAction LocalRescind = Just RemoteRescind
    remoteAction _ = Nothing

    -- send an RPC to the remote backend
    notifyRemote ::
      Local UserId ->
      Remote UserId ->
      RemoteConnectionAction ->
      AppIO (Maybe RemoteConnectionAction)
    notifyRemote _self _other _action = pure Nothing

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
  ConnId ->
  Remote UserId ->
  Maybe UserConnection ->
  RemoteConnectionAction ->
  AppIO (Maybe UserConnection, Maybe RemoteConnectionAction)
performRemoteAction self conn other mconnection action = do
  let rel0 = maybe Cancelled ucStatus mconnection
  case transition (RCA action) rel0 of
    Nothing -> pure (Nothing, Nothing)
    Just rel1 -> do
      connection <- transitionTo self conn other mconnection rel1
      pure (Just connection, reaction rel1)
  where
    reaction :: Relation -> Maybe RemoteConnectionAction
    reaction Accepted = Just RemoteConnect
    reaction Sent = Just RemoteConnect
    reaction _ = Nothing
