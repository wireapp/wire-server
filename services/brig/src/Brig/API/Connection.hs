{-# LANGUAGE RecordWildCards #-}

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

-- TODO: Move to Brig.User.Connection (& split out Brig.User.Invitation?)

-- | > docs/reference/user/connection.md {#RefConnection}
--
-- User connection logic.
module Brig.API.Connection
  ( -- * Connections
    autoConnect,
    createConnection,
    updateConnection,
    lookupConnections,
    Data.lookupConnection,
    Data.lookupConnectionStatus,
    Data.lookupContactList,
  )
where

import Brig.API.Error (userNotFound)
import Brig.API.Types
import Brig.API.User (getLegalHoldStatus)
import Brig.App
import qualified Brig.Data.Connection as Data
import qualified Brig.Data.User as Data
import qualified Brig.IO.Intra as Intra
import Brig.Options (setUserMaxConnections)
import Brig.Types
import Brig.Types.Intra
import Brig.Types.User.Event
import Control.Error
import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Data.Id as Id
import qualified Data.LegalHold as LH
import Data.Range
import qualified Data.Set as Set
import Galley.Types (ConvType (..), cnvType)
import qualified Galley.Types.Teams as Team
import Imports
import qualified System.Logger.Class as Log
import System.Logger.Message

createConnection ::
  UserId ->
  ConnectionRequest ->
  ConnId ->
  ExceptT ConnectionError AppIO ConnectionResult
createConnection self req conn = do
  createConnectionToLocalUser self (crUser req) req conn

createConnectionToLocalUser ::
  UserId ->
  UserId ->
  ConnectionRequest ->
  ConnId ->
  ExceptT ConnectionError AppIO ConnectionResult
createConnectionToLocalUser self crUser ConnectionRequest {crName, crMessage} conn = do
  when (self == crUser) $
    throwE $
      InvalidUser crUser
  selfActive <- lift $ Data.isActivated self
  unless selfActive $
    throwE ConnectNoIdentity
  otherActive <- lift $ Data.isActivated crUser
  unless otherActive $
    throwE $
      InvalidUser crUser
  checkLegalholdPolicyConflict self crUser
  -- Users belonging to the same team are always treated as connected, so creating a
  -- connection between them is useless. {#RefConnectionTeam}
  sameTeam <- lift $ belongSameTeam
  when sameTeam $
    throwE ConnectSameBindingTeamUsers
  s2o <- lift $ Data.lookupConnection self crUser
  o2s <- lift $ Data.lookupConnection crUser self
  case update <$> s2o <*> o2s of
    Just rs -> rs
    Nothing -> do
      checkLimit self
      ConnectionCreated <$> insert Nothing Nothing
  where
    insert :: Maybe UserConnection -> Maybe UserConnection -> ExceptT ConnectionError AppIO UserConnection
    insert s2o o2s = lift $ do
      Log.info $
        logConnection self crUser
          . msg (val "Creating connection")
      cnv <- Intra.createConnectConv self crUser (Just crName) (Just crMessage) (Just conn)
      s2o' <- Data.insertConnection self crUser Sent (Just crMessage) cnv
      o2s' <- Data.insertConnection crUser self Pending (Just crMessage) cnv
      e2o <- ConnectionUpdated o2s' (ucStatus <$> o2s) <$> Data.lookupName self
      let e2s = ConnectionUpdated s2o' (ucStatus <$> s2o) Nothing
      mapM_ (Intra.onConnectionEvent self (Just conn)) [e2o, e2s]
      return s2o'

    update :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO ConnectionResult
    update s2o o2s = case (ucStatus s2o, ucStatus o2s) of
      (Accepted, Accepted) -> return $ ConnectionExists s2o
      (Accepted, Blocked) -> return $ ConnectionExists s2o
      (Sent, Blocked) -> return $ ConnectionExists s2o
      (Blocked, _) -> throwE $ InvalidTransition self Sent
      (MissingLegalholdConsent, _) -> throwE $ InvalidTransition self Sent
      (_, MissingLegalholdConsent) -> throwE $ InvalidTransition self Sent
      -- TODO: but how do we recover the old state, before MissingLegalholdConsent?  just move
      -- to "pending" on both sides?, and make one of the two re-accept?  is that going to
      -- work with the existing convs?  or store the old state in a new column in the
      -- connections table?  horrible idea...
      (_, Blocked) -> change s2o Sent
      (_, Sent) -> accept s2o o2s
      (_, Accepted) -> accept s2o o2s
      (_, Ignored) -> resend s2o o2s
      (_, Pending) -> resend s2o o2s
      (_, Cancelled) -> resend s2o o2s

    accept :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO ConnectionResult
    accept s2o o2s = do
      when (ucStatus s2o `notElem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logConnection self (ucTo s2o)
          . msg (val "Accepting connection")
      cnv <- lift $ for (ucConvId s2o) $ Intra.acceptConnectConv self (Just conn)
      s2o' <- lift $ Data.updateConnection s2o Accepted
      o2s' <-
        lift $
          if (cnvType <$> cnv) == Just ConnectConv
            then Data.updateConnection o2s Blocked
            else Data.updateConnection o2s Accepted
      e2o <- lift $ ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
      let e2s = ConnectionUpdated s2o' (Just $ ucStatus s2o) Nothing
      lift $ mapM_ (Intra.onConnectionEvent self (Just conn)) [e2o, e2s]
      return $ ConnectionExists s2o'

    resend :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO ConnectionResult
    resend s2o o2s = do
      when (ucStatus s2o `notElem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logConnection self (ucTo s2o)
          . msg (val "Resending connection request")
      s2o' <- insert (Just s2o) (Just o2s)
      return $ ConnectionExists s2o'

    change :: UserConnection -> Relation -> ExceptT ConnectionError AppIO ConnectionResult
    change c s = ConnectionExists <$> lift (Data.updateConnection c s)

    belongSameTeam :: AppIO Bool
    belongSameTeam = do
      selfTeam <- Intra.getTeamId self
      crTeam <- Intra.getTeamId crUser
      pure $ isJust selfTeam && selfTeam == crTeam

-- | Throw error if one user ahs a LH device and the other status `no_consent` or vice versa.
--
-- FUTUREWORK: we may want to move this to the LH application logic, so we can recycle it for
-- group conv creation and possibly other situations.
checkLegalholdPolicyConflict :: UserId -> UserId -> ExceptT ConnectionError AppIO ()
checkLegalholdPolicyConflict uid1 uid2 = do
  let catchProfileNotFound =
        -- Does not fit into 'ExceptT', so throw in 'AppIO'.  Anyway at the time of writing
        -- this, users are guaranteed to exist when called from 'createConnectionToLocalUser'.
        maybe (throwM userNotFound) return

  status1 <- lift (getLegalHoldStatus uid1) >>= catchProfileNotFound
  status2 <- lift (getLegalHoldStatus uid2) >>= catchProfileNotFound

  let oneway s1 s2 = case (s1, s2) of
        (LH.UserLegalHoldNoConsent, LH.UserLegalHoldNoConsent) -> pure ()
        (LH.UserLegalHoldNoConsent, LH.UserLegalHoldDisabled) -> pure ()
        (LH.UserLegalHoldNoConsent, LH.UserLegalHoldPending) -> throwE ConnectMissingLegalholdConsent
        (LH.UserLegalHoldNoConsent, LH.UserLegalHoldEnabled) -> throwE ConnectMissingLegalholdConsent
        (LH.UserLegalHoldDisabled, _) -> pure ()
        (LH.UserLegalHoldPending, _) -> pure ()
        (LH.UserLegalHoldEnabled, _) -> pure ()

  oneway status1 status2
  oneway status2 status1

-- | Change the status of a connection from one user to another.
--
-- Note: 'updateConnection' doesn't explicitly check that users don't belong to the same team,
-- because a connection between two team members can not exist in the first place.
-- {#RefConnectionTeam}
updateConnection ::
  -- | From
  UserId ->
  -- | To
  UserId ->
  -- | Desired relation status
  Relation ->
  -- | Acting device connection ID
  Maybe ConnId ->
  ExceptT ConnectionError AppIO (Maybe UserConnection)
updateConnection self other newStatus conn = do
  s2o <- connection self other
  o2s <- connection other self
  s2o' <- case (ucStatus s2o, ucStatus o2s, newStatus) of
    -- Pending -> {Blocked, Ignored, Accepted}
    (Pending, _, Blocked) -> block s2o
    (Pending, _, Ignored) -> change s2o Ignored
    (Pending, _, Accepted) -> accept s2o o2s
    -- Ignored -> {Accepted, Blocked}
    (Ignored, _, Accepted) -> accept s2o o2s
    (Ignored, _, Blocked) -> block s2o
    -- Blocked -> {Accepted, Sent}
    (Blocked, Accepted, Accepted) -> unblock s2o o2s Accepted
    (Blocked, Blocked, Accepted) -> unblock s2o o2s Accepted
    (Blocked, Sent, Accepted) -> unblock s2o o2s Accepted
    (Blocked, Pending, Accepted) -> unblock s2o o2s Sent
    (Blocked, Ignored, Accepted) -> unblock s2o o2s Sent
    (Blocked, Cancelled, Accepted) -> unblock s2o o2s Sent
    (Blocked, Accepted, Sent) -> unblock s2o o2s Accepted
    (Blocked, Blocked, Sent) -> unblock s2o o2s Accepted
    (Blocked, Sent, Sent) -> unblock s2o o2s Accepted
    (Blocked, Pending, Sent) -> unblock s2o o2s Sent
    (Blocked, Ignored, Sent) -> unblock s2o o2s Sent
    (Blocked, Cancelled, Sent) -> unblock s2o o2s Sent
    -- Accepted -> {Blocked}
    (Accepted, _, Blocked) -> block s2o
    -- Sent -> {Blocked, Cancelled, Accepted}
    (Sent, _, Blocked) -> block s2o
    (Sent, Sent, Accepted) -> change s2o Accepted >> change o2s Accepted
    (Sent, Accepted, Accepted) -> change s2o Accepted
    (Sent, Blocked, Cancelled) -> change s2o Cancelled
    (Sent, Cancelled, Cancelled) -> change s2o Cancelled
    (Sent, Pending, Cancelled) -> cancel s2o o2s
    (Sent, Ignored, Cancelled) -> cancel s2o o2s
    -- Cancelled -> {Blocked}
    (Cancelled, _, Blocked) -> block s2o
    -- no change
    (old, _, new) | old == new -> return Nothing
    -- missing legalhold consent
    (MissingLegalholdConsent, _, _) -> error "TODO"
    (_, MissingLegalholdConsent, _) -> error "TODO"
    (_, _, MissingLegalholdConsent) -> error "TODO"
    -- invalid
    _ -> throwE $ InvalidTransition self newStatus
  lift . for_ s2o' $ \c ->
    let e2s = ConnectionUpdated c (Just $ ucStatus s2o) Nothing
     in Intra.onConnectionEvent self conn e2s
  return s2o'
  where
    accept :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    accept s2o o2s = do
      checkLimit self
      Log.info $
        logConnection self (ucTo s2o)
          . msg (val "Accepting connection")
      cnv <- lift . for (ucConvId s2o) $ Intra.acceptConnectConv self conn
      -- Note: The check for @Pending@ accounts for situations in which both
      --       sides are pending, which can occur due to rare race conditions
      --       when sending mutual connection requests, combined with untimely
      --       crashes.
      when (ucStatus o2s `elem` [Sent, Pending]) . lift $ do
        o2s' <-
          if (cnvType <$> cnv) /= Just ConnectConv
            then Data.updateConnection o2s Accepted
            else Data.updateConnection o2s Blocked
        e2o <- ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
        Intra.onConnectionEvent self conn e2o
      lift $ Just <$> Data.updateConnection s2o Accepted

    block :: UserConnection -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    block s2o = lift $ do
      Log.info $
        logConnection self (ucTo s2o)
          . msg (val "Blocking connection")
      for_ (ucConvId s2o) $ Intra.blockConv (ucFrom s2o) conn
      Just <$> Data.updateConnection s2o Blocked

    unblock :: UserConnection -> UserConnection -> Relation -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    unblock s2o o2s new = do
      when (new `elem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logConnection self (ucTo s2o)
          . msg (val "Unblocking connection")
      cnv <- lift . for (ucConvId s2o) $ Intra.unblockConv (ucFrom s2o) conn
      when (ucStatus o2s == Sent && new == Accepted) . lift $ do
        o2s' <-
          if (cnvType <$> cnv) /= Just ConnectConv
            then Data.updateConnection o2s Accepted
            else Data.updateConnection o2s Blocked
        e2o <- ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
        Intra.onConnectionEvent self conn e2o
      lift $ Just <$> Data.updateConnection s2o new

    cancel :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    cancel s2o o2s = do
      Log.info $
        logConnection self (ucTo s2o)
          . msg (val "Cancelling connection")
      lift . for_ (ucConvId s2o) $ Intra.blockConv (ucFrom s2o) conn
      o2s' <- lift $ Data.updateConnection o2s Cancelled
      let e2o = ConnectionUpdated o2s' (Just $ ucStatus o2s) Nothing
      lift $ Intra.onConnectionEvent self conn e2o
      change s2o Cancelled

    change :: UserConnection -> Relation -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    change c s = lift $ Just <$> Data.updateConnection c s

    connection :: UserId -> UserId -> ExceptT ConnectionError AppIO UserConnection
    connection a b = lift (Data.lookupConnection a b) >>= tryJust (NotConnected a b)

autoConnect ::
  UserId ->
  Set UserId ->
  Maybe ConnId ->
  ExceptT ConnectionError AppIO [UserConnection]
autoConnect from (Set.toList -> to) conn = do
  selfActive <- lift $ Data.isActivated from
  -- FIXME: checkLimit from
  -- Checking the limit here is currently a too heavy operation
  -- for this code path and needs to be optimised / rethought.
  unless selfActive $
    throwE ConnectNoIdentity
  othersActive <- lift $ Data.filterActive to
  nonTeamMembers <- filterOutTeamMembers othersActive
  lift $ connectAll nonTeamMembers
  where
    filterOutTeamMembers us = do
      -- FUTUREWORK: This is only used for test purposes. If getTeamContacts is truncated
      --       tests might fail in strange ways. Maybe we want to fail hard if this
      --       returns a truncated list. I think the whole function can be removed.
      mems <- lift $ Intra.getTeamContacts from
      return $ maybe us (Team.notTeamMember us . view Team.teamMembers) mems
    connectAll activeOthers = do
      others <- selectOthers activeOthers
      convs <- mapM (createConv from) others
      self <- Data.lookupName from
      ucs <- Data.connectUsers from convs
      let events = map (toEvent self) ucs
      forM_ events $ Intra.onConnectionEvent from conn
      return ucs
    -- Assumption: if there's an existing connection, don't touch it.
    -- The exception to this rule _could_ be a sent/pending connection
    -- but for sure we would not override states like `blocked` and `ignored`
    -- For simplicity, let's just not touch them.
    selectOthers usrs = do
      existing <- map csFrom <$> Data.lookupConnectionStatus usrs [from]
      return $ filter (`notElem` existing) usrs
    createConv s o = do
      c <- Intra.createConnectConv s o Nothing Nothing conn
      _ <- Intra.acceptConnectConv o conn c
      return (o, c)
    -- Note: The events sent to the users who got auto-connected to 'from'
    --       get the user name of the user whom they got connected to included.
    toEvent self uc = ConnectionUpdated uc Nothing (mfilter (const $ ucFrom uc /= from) self)

lookupConnections :: UserId -> Maybe UserId -> Range 1 500 Int32 -> AppIO UserConnectionList
lookupConnections from start size = do
  rs <- Data.lookupConnections from start size
  return $! UserConnectionList (Data.resultList rs) (Data.resultHasMore rs)

-- Helpers

checkLimit :: UserId -> ExceptT ConnectionError AppIO ()
checkLimit u = do
  n <- lift $ Data.countConnections u [Accepted, Sent]
  l <- setUserMaxConnections <$> view settings
  unless (n < l) $
    throwE $
      TooManyConnections u
