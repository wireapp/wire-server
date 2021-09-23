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
    createConnection,
    updateConnection,
    UpdateConnectionsInternal (..),
    updateConnectionInternal,
    lookupLocalConnection,
    lookupConnections,
    Data.lookupConnectionStatus,
    Data.lookupConnectionStatus',
    Data.lookupContactList,
  )
where

import Brig.API.Error (errorDescriptionTypeToWai)
import Brig.API.Types
import Brig.API.User (getLegalHoldStatus)
import Brig.App
import Brig.Data.Connection (LocalConnection (..), localToUserConn)
import qualified Brig.Data.Connection as Data
import Brig.Data.Types (resultHasMore, resultList)
import qualified Brig.Data.User as Data
import qualified Brig.IO.Intra as Intra
import Brig.Options (setUserMaxConnections)
import Brig.Types
import Brig.Types.User.Event
import Control.Error
import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Data.Id as Id
import qualified Data.LegalHold as LH
import Data.Proxy (Proxy (Proxy))
import Data.Qualified (Qualified (Qualified))
import Data.Range
import Galley.Types (ConvType (..), cnvType)
import Imports
import qualified System.Logger.Class as Log
import System.Logger.Message
import Wire.API.Connection (RelationWithHistory (..))
import qualified Wire.API.Conversation as Conv
import Wire.API.ErrorDescription
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))

lookupLocalConnection :: UserId -> UserId -> AppIO (Maybe UserConnection)
lookupLocalConnection uid1 uid2 = do
  localDomain <- viewFederationDomain
  Data.localToUserConn localDomain <$$> Data.lookupLocalConnection uid1 uid2

createConnection ::
  UserId ->
  ConnectionRequest ->
  ConnId ->
  ExceptT ConnectionError AppIO (ResponseForExistedCreated UserConnection)
createConnection self req conn =
  createConnectionToLocalUser self (crUser req) req conn

createConnectionToLocalUser ::
  UserId ->
  UserId ->
  ConnectionRequest ->
  ConnId ->
  ExceptT ConnectionError AppIO (ResponseForExistedCreated UserConnection)
createConnectionToLocalUser self crUser ConnectionRequest {crName} conn = do
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
  sameTeam <- lift belongSameTeam
  when sameTeam $
    throwE ConnectSameBindingTeamUsers
  s2o <- lift $ Data.lookupLocalConnection self crUser
  o2s <- lift $ Data.lookupLocalConnection crUser self
  localDomain <- viewFederationDomain
  case update <$> s2o <*> o2s of
    Just rs -> localToUserConn localDomain <$$> rs
    Nothing -> do
      checkLimit self
      Created . localToUserConn localDomain <$> insert Nothing Nothing
  where
    insert :: Maybe LocalConnection -> Maybe LocalConnection -> ExceptT ConnectionError AppIO LocalConnection
    insert s2o o2s = lift $ do
      localDomain <- viewFederationDomain
      Log.info $
        logConnection self (Qualified crUser localDomain)
          . msg (val "Creating connection")
      cnv <- Intra.createConnectConv self crUser (Just (fromRange crName)) (Just conn)
      s2o' <- Data.insertLocalConnection self crUser SentWithHistory cnv
      o2s' <- Data.insertLocalConnection crUser self PendingWithHistory cnv
      e2o <- ConnectionUpdated (localToUserConn localDomain o2s') (lcStatus <$> o2s) <$> Data.lookupName self
      let e2s = ConnectionUpdated (localToUserConn localDomain s2o') (lcStatus <$> s2o) Nothing
      mapM_ (Intra.onConnectionEvent self (Just conn)) [e2o, e2s]
      return s2o'

    update :: LocalConnection -> LocalConnection -> ExceptT ConnectionError AppIO (ResponseForExistedCreated LocalConnection)
    update s2o o2s = case (lcStatus s2o, lcStatus o2s) of
      (MissingLegalholdConsent, _) -> throwE $ InvalidTransition self Sent
      (_, MissingLegalholdConsent) -> throwE $ InvalidTransition self Sent
      (Accepted, Accepted) -> return $ Existed s2o
      (Accepted, Blocked) -> return $ Existed s2o
      (Sent, Blocked) -> return $ Existed s2o
      (Blocked, _) -> throwE $ InvalidTransition self Sent
      (_, Blocked) -> change s2o SentWithHistory
      (_, Sent) -> accept s2o o2s
      (_, Accepted) -> accept s2o o2s
      (_, Ignored) -> resend s2o o2s
      (_, Pending) -> resend s2o o2s
      (_, Cancelled) -> resend s2o o2s

    accept :: LocalConnection -> LocalConnection -> ExceptT ConnectionError AppIO (ResponseForExistedCreated LocalConnection)
    accept s2o o2s = do
      localDomain <- viewFederationDomain
      when (lcStatus s2o `notElem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logLocalConnection self (lcTo s2o)
          . msg (val "Accepting connection")
      cnv <- lift $ for (lcConv s2o) $ Intra.acceptConnectConv self (Just conn)
      s2o' <- lift $ Data.updateLocalConnection s2o AcceptedWithHistory
      o2s' <-
        lift $
          if (cnvType <$> cnv) == Just ConnectConv
            then Data.updateLocalConnection o2s BlockedWithHistory
            else Data.updateLocalConnection o2s AcceptedWithHistory
      e2o <- lift $ ConnectionUpdated (localToUserConn localDomain o2s') (Just $ lcStatus o2s) <$> Data.lookupName self
      let e2s = ConnectionUpdated (localToUserConn localDomain s2o') (Just $ lcStatus s2o) Nothing
      lift $ mapM_ (Intra.onConnectionEvent self (Just conn)) [e2o, e2s]
      return $ Existed s2o'

    resend :: LocalConnection -> LocalConnection -> ExceptT ConnectionError AppIO (ResponseForExistedCreated LocalConnection)
    resend s2o o2s = do
      when (lcStatus s2o `notElem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logLocalConnection self (lcTo s2o)
          . msg (val "Resending connection request")
      s2o' <- insert (Just s2o) (Just o2s)
      return $ Existed s2o'

    change :: LocalConnection -> RelationWithHistory -> ExceptT ConnectionError AppIO (ResponseForExistedCreated LocalConnection)
    change c s = Existed <$> lift (Data.updateLocalConnection c s)

    belongSameTeam :: AppIO Bool
    belongSameTeam = do
      selfTeam <- Intra.getTeamId self
      crTeam <- Intra.getTeamId crUser
      pure $ isJust selfTeam && selfTeam == crTeam

-- | Throw error if one user has a LH device and the other status `no_consent` or vice versa.
--
-- FUTUREWORK: we may want to move this to the LH application logic, so we can recycle it for
-- group conv creation and possibly other situations.
checkLegalholdPolicyConflict :: UserId -> UserId -> ExceptT ConnectionError AppIO ()
checkLegalholdPolicyConflict uid1 uid2 = do
  let catchProfileNotFound =
        -- Does not fit into 'ExceptT', so throw in 'AppIO'.  Anyway at the time of writing
        -- this, users are guaranteed to exist when called from 'createConnectionToLocalUser'.
        maybe (throwM (errorDescriptionTypeToWai @UserNotFound)) return

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
  s2o <- localConnection self other
  o2s <- localConnection other self
  s2o' <- case (lcStatus s2o, lcStatus o2s, newStatus) of
    -- missing legalhold consent: call 'updateConectionInternal' instead.
    (MissingLegalholdConsent, _, _) -> throwE $ InvalidTransition self newStatus
    (_, MissingLegalholdConsent, _) -> throwE $ InvalidTransition self newStatus
    (_, _, MissingLegalholdConsent) -> throwE $ InvalidTransition self newStatus
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
    -- invalid
    _ -> throwE $ InvalidTransition self newStatus
  localDomain <- viewFederationDomain
  let s2oUserConn = Data.localToUserConn localDomain <$> s2o'
  lift . for_ s2oUserConn $ \c ->
    let e2s = ConnectionUpdated c (Just $ lcStatus s2o) Nothing
     in Intra.onConnectionEvent self conn e2s
  return s2oUserConn
  where
    accept :: LocalConnection -> LocalConnection -> ExceptT ConnectionError AppIO (Maybe LocalConnection)
    accept s2o o2s = do
      localDomain <- viewFederationDomain
      checkLimit self
      Log.info $
        logLocalConnection self (lcTo s2o)
          . msg (val "Accepting connection")
      cnv <- lift . for (lcConv s2o) $ Intra.acceptConnectConv self conn
      -- Note: The check for @Pending@ accounts for situations in which both
      --       sides are pending, which can occur due to rare race conditions
      --       when sending mutual connection requests, combined with untimely
      --       crashes.
      when (lcStatus o2s `elem` [Sent, Pending]) . lift $ do
        o2s' <-
          if (cnvType <$> cnv) /= Just ConnectConv
            then Data.updateLocalConnection o2s AcceptedWithHistory
            else Data.updateLocalConnection o2s BlockedWithHistory
        e2o <- ConnectionUpdated (localToUserConn localDomain o2s') (Just $ lcStatus o2s) <$> Data.lookupName self
        Intra.onConnectionEvent self conn e2o
      lift $ Just <$> Data.updateLocalConnection s2o AcceptedWithHistory

    block :: LocalConnection -> ExceptT ConnectionError AppIO (Maybe LocalConnection)
    block s2o = lift $ do
      Log.info $
        logLocalConnection self (lcTo s2o)
          . msg (val "Blocking connection")
      for_ (lcConv s2o) $ Intra.blockConv (lcFrom s2o) conn
      Just <$> Data.updateLocalConnection s2o BlockedWithHistory

    unblock :: LocalConnection -> LocalConnection -> Relation -> ExceptT ConnectionError AppIO (Maybe LocalConnection)
    unblock s2o o2s new = do
      localDomain <- viewFederationDomain
      -- FUTUREWORK: new is always in [Sent, Accepted]. Refactor to total function.
      when (new `elem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logLocalConnection self (lcTo s2o)
          . msg (val "Unblocking connection")
      cnv :: Maybe Conv.Conversation <- lift . for (lcConv s2o) $ Intra.unblockConv (lcFrom s2o) conn
      when (lcStatus o2s == Sent && new == Accepted) . lift $ do
        o2s' <-
          if (cnvType <$> cnv) /= Just ConnectConv
            then Data.updateLocalConnection o2s AcceptedWithHistory
            else Data.updateLocalConnection o2s BlockedWithHistory
        e2o :: ConnectionEvent <- ConnectionUpdated (localToUserConn localDomain o2s') (Just $ lcStatus o2s) <$> Data.lookupName self
        -- TODO: is this correct? shouldnt o2s be sent to other?
        Intra.onConnectionEvent self conn e2o
      lift $ Just <$> Data.updateLocalConnection s2o (mkRelationWithHistory (error "impossible") new)

    cancel :: LocalConnection -> LocalConnection -> ExceptT ConnectionError AppIO (Maybe LocalConnection)
    cancel s2o o2s = do
      localDomain <- viewFederationDomain
      Log.info $
        logLocalConnection self (lcTo s2o)
          . msg (val "Cancelling connection")
      lift . for_ (lcConv s2o) $ Intra.blockConv (lcFrom s2o) conn
      o2s' <- lift $ Data.updateLocalConnection o2s CancelledWithHistory
      let e2o = ConnectionUpdated (localToUserConn localDomain o2s') (Just $ lcStatus o2s) Nothing
      lift $ Intra.onConnectionEvent self conn e2o
      change s2o Cancelled

    change :: LocalConnection -> Relation -> ExceptT ConnectionError AppIO (Maybe LocalConnection)
    change c s = do
      -- FUTUREWORK: refactor to total function. Gets only called with either Ignored, Accepted, Cancelled
      lift $ Just <$> Data.updateLocalConnection c (mkRelationWithHistory (error "impossible") s)

localConnection :: UserId -> UserId -> ExceptT ConnectionError AppIO LocalConnection
localConnection a b = do
  lift (Data.lookupLocalConnection a b)
    >>= tryJust (NotConnected a b)

mkRelationWithHistory :: HasCallStack => Relation -> Relation -> RelationWithHistory
mkRelationWithHistory oldRel = \case
  Accepted -> AcceptedWithHistory
  Blocked -> BlockedWithHistory
  Pending -> PendingWithHistory
  Ignored -> IgnoredWithHistory
  Sent -> SentWithHistory
  Cancelled -> CancelledWithHistory
  MissingLegalholdConsent ->
    case oldRel of
      Accepted -> MissingLegalholdConsentFromAccepted
      Blocked -> MissingLegalholdConsentFromBlocked
      Pending -> MissingLegalholdConsentFromPending
      Ignored -> MissingLegalholdConsentFromIgnored
      Sent -> MissingLegalholdConsentFromSent
      Cancelled -> MissingLegalholdConsentFromCancelled
      MissingLegalholdConsent -> error "impossible old relation"

updateConnectionInternal ::
  UpdateConnectionsInternal ->
  ExceptT ConnectionError AppIO ()
updateConnectionInternal = \case
  BlockForMissingLHConsent uid others -> blockForMissingLegalholdConsent uid others
  RemoveLHBlocksInvolving uid -> removeLHBlocksInvolving uid
  where
    -- inspired by @block@ in 'updateConnection'.
    blockForMissingLegalholdConsent :: UserId -> [UserId] -> ExceptT ConnectionError AppIO ()
    blockForMissingLegalholdConsent self others = do
      localDomain <- viewFederationDomain
      for_ others $ \other -> do
        Log.info $
          logConnection self (Qualified other localDomain)
            . msg (val "Blocking connection (legalhold device present, but missing consent)")

        s2o <- localConnection self other
        o2s <- localConnection other self
        for_ [s2o, o2s] $ \(uconn :: LocalConnection) -> lift $ do
          Intra.blockConv (lcFrom uconn) Nothing `mapM_` lcConv uconn
          uconn' <- Data.updateLocalConnection uconn (mkRelationWithHistory (lcStatus uconn) MissingLegalholdConsent)
          let ev = ConnectionUpdated (Data.localToUserConn localDomain uconn') (Just $ lcStatus uconn) Nothing
          Intra.onConnectionEvent self Nothing ev

    removeLHBlocksInvolving :: UserId -> ExceptT ConnectionError AppIO ()
    removeLHBlocksInvolving self =
      iterateConnections self (toRange (Proxy @500)) $ \conns -> do
        localDomain <- viewFederationDomain
        for_ conns $ \s2o ->
          when (Data.lcStatus s2o == MissingLegalholdConsent) $ do
            -- (this implies @ucStatus o2s == MissingLegalholdConsent@)
            let other = Data.lcTo s2o
            o2s <- localConnection other self
            Log.info $
              logConnection (Data.lcFrom s2o) (Qualified (Data.lcTo s2o) localDomain)
                . msg (val "Unblocking connection (legalhold device removed or consent given)")
            unblockDirected s2o o2s
            unblockDirected o2s s2o
      where
        iterateConnections :: UserId -> Range 1 500 Int32 -> ([Data.LocalConnection] -> ExceptT ConnectionError AppIO ()) -> ExceptT ConnectionError AppIO ()
        iterateConnections user pageSize handleConns = go Nothing
          where
            go :: Maybe UserId -> ExceptT ConnectionError (AppT IO) ()
            go mbStart = do
              page <- lift $ Data.lookupLocalConnections user mbStart pageSize
              handleConns (resultList page)
              case resultList page of
                (conn : rest) ->
                  if resultHasMore page
                    then go (Just (maximum (Data.lcTo <$> (conn : rest))))
                    else pure ()
                [] -> pure ()

        unblockDirected :: Data.LocalConnection -> Data.LocalConnection -> ExceptT ConnectionError AppIO ()
        unblockDirected uconn uconnRev = do
          void . lift . for (Data.lcConv uconn) $ Intra.unblockConv (Data.lcFrom uconn) Nothing
          uconnRevRel :: RelationWithHistory <- relationWithHistory (Data.lcFrom uconnRev) (Data.lcTo uconnRev)
          uconnRev' <- lift $ Data.updateLocalConnection uconnRev (undoRelationHistory uconnRevRel)
          localDomain <- viewFederationDomain
          connName <- lift $ Data.lookupName (Data.lcFrom uconn)
          let connEvent =
                ConnectionUpdated
                  { ucConn = Data.localToUserConn localDomain uconnRev',
                    ucPrev = Just $ Data.lcStatus uconnRev,
                    ucName = connName
                  }
          lift $ Intra.onConnectionEvent (Data.lcFrom uconn) Nothing connEvent
    relationWithHistory :: UserId -> UserId -> ExceptT ConnectionError AppIO RelationWithHistory
    relationWithHistory a b = lift (Data.lookupRelationWithHistory a b) >>= tryJust (NotConnected a b)

    undoRelationHistory :: RelationWithHistory -> RelationWithHistory
    undoRelationHistory = \case
      -- these cases are relevant.
      MissingLegalholdConsentFromAccepted -> AcceptedWithHistory
      MissingLegalholdConsentFromBlocked -> BlockedWithHistory
      MissingLegalholdConsentFromPending -> PendingWithHistory
      MissingLegalholdConsentFromIgnored -> IgnoredWithHistory
      MissingLegalholdConsentFromSent -> SentWithHistory
      MissingLegalholdConsentFromCancelled -> CancelledWithHistory
      -- these cases should not be reachable, but if they are, this is probably what is expected from this function.
      AcceptedWithHistory -> AcceptedWithHistory
      BlockedWithHistory -> BlockedWithHistory
      PendingWithHistory -> PendingWithHistory
      IgnoredWithHistory -> IgnoredWithHistory
      SentWithHistory -> SentWithHistory
      CancelledWithHistory -> CancelledWithHistory

lookupConnections :: UserId -> Maybe UserId -> Range 1 500 Int32 -> AppIO UserConnectionList
lookupConnections from start size = do
  rs <- Data.lookupLocalConnections from start size
  localDomain <- viewFederationDomain
  return $! UserConnectionList (Data.localToUserConn localDomain <$> Data.resultList rs) (Data.resultHasMore rs)

-- Helpers

checkLimit :: UserId -> ExceptT ConnectionError AppIO ()
checkLimit u = do
  n <- lift $ Data.countConnections u [Accepted, Sent]
  l <- setUserMaxConnections <$> view settings
  unless (n < l) $
    throwE $
      TooManyConnections u
