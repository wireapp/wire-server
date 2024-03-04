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

-- | > docs/reference/user/connection.md {#RefConnection}
--
-- User connection logic.
module Brig.API.Connection
  ( -- * Connections
    createConnection,
    createConnectionToLocalUser,
    updateConnection,
    updateConnectionToLocalUser,
    UpdateConnectionsInternal (..),
    updateConnectionInternal,
    lookupConnections,
    Data.lookupConnectionStatus,
    Data.lookupConnectionStatus',
    Data.lookupContactList,
  )
where

import Brig.API.Connection.Remote
import Brig.API.Connection.Util
import Brig.API.Types
import Brig.API.User (getLegalHoldStatus)
import Brig.App
import Brig.Data.Connection qualified as Data
import Brig.Data.Types (resultHasMore, resultList)
import Brig.Data.User qualified as Data
import Brig.Effects.FederationConfigStore
import Brig.Effects.GalleyProvider
import Brig.Effects.GalleyProvider qualified as GalleyProvider
import Brig.IO.Intra qualified as Intra
import Brig.Options
import Brig.Types.Connection
import Brig.Types.User.Event
import Control.Error
import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Data.Id as Id
import Data.LegalHold qualified as LH
import Data.Proxy (Proxy (Proxy))
import Data.Qualified
import Data.Range
import Data.UUID.V4 qualified as UUID
import Galley.Types.Conversations.One2One
import Imports
import Polysemy
import Polysemy.TinyLog
import System.Logger.Class qualified as Log
import System.Logger.Message
import Wire.API.Connection hiding (relationWithHistory)
import Wire.API.Conversation hiding (Member)
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))
import Wire.API.User
import Wire.NotificationSubsystem

ensureNotSameTeam :: Member GalleyProvider r => Local UserId -> Local UserId -> (ConnectionM r) ()
ensureNotSameTeam self target = do
  selfTeam <- lift $ liftSem $ GalleyProvider.getTeamId (tUnqualified self)
  targetTeam <- lift $ liftSem $ GalleyProvider.getTeamId (tUnqualified target)
  when (isJust selfTeam && selfTeam == targetTeam) $
    throwE ConnectSameBindingTeamUsers

createConnection ::
  ( Member FederationConfigStore r,
    Member GalleyProvider r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  ConnectionM r (ResponseForExistedCreated UserConnection)
createConnection self con target = do
  -- branch according to whether we are connecting to a local or remote user
  foldQualified
    self
    (createConnectionToLocalUser self con)
    (createConnectionToRemoteUser self con)
    target

createConnectionToLocalUser ::
  forall r.
  ( Member GalleyProvider r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r
  ) =>
  Local UserId ->
  ConnId ->
  Local UserId ->
  ConnectionM r (ResponseForExistedCreated UserConnection)
createConnectionToLocalUser self conn target = do
  ensureNotSameAndActivated self (tUntagged target)
  noteT (InvalidUser (tUntagged target)) $
    ensureIsActivated target
  checkLegalholdPolicyConflict (tUnqualified self) (tUnqualified target)
  ensureNotSameTeam self target
  s2o <- lift . wrapClient $ Data.lookupConnection self (tUntagged target)
  o2s <- lift . wrapClient $ Data.lookupConnection target (tUntagged self)

  case update <$> s2o <*> o2s of
    Just rs -> rs
    Nothing -> do
      checkLimit self
      Created <$> insert Nothing Nothing
  where
    insert :: Maybe UserConnection -> Maybe UserConnection -> ExceptT ConnectionError (AppT r) UserConnection
    insert s2o o2s = lift $ do
      Log.info $
        logConnection (tUnqualified self) (tUntagged target)
          . msg (val "Creating connection")
      qcnv <- Intra.createConnectConv (tUntagged self) (tUntagged target) Nothing (Just conn)
      s2o' <- wrapClient $ Data.insertConnection self (tUntagged target) SentWithHistory qcnv
      o2s' <- wrapClient $ Data.insertConnection target (tUntagged self) PendingWithHistory qcnv
      e2o <-
        ConnectionUpdated o2s' (ucStatus <$> o2s)
          <$> wrapClient (Data.lookupName (tUnqualified self))
      let e2s = ConnectionUpdated s2o' (ucStatus <$> s2o) Nothing
      liftSem $ mapM_ (Intra.onConnectionEvent (tUnqualified self) (Just conn)) [e2o, e2s]
      pure s2o'

    update :: UserConnection -> UserConnection -> ExceptT ConnectionError (AppT r) (ResponseForExistedCreated UserConnection)
    update s2o o2s = case (ucStatus s2o, ucStatus o2s) of
      (MissingLegalholdConsent, _) -> throwE $ InvalidTransition (tUnqualified self)
      (_, MissingLegalholdConsent) -> throwE $ InvalidTransition (tUnqualified self)
      (Accepted, Accepted) -> pure $ Existed s2o
      (Accepted, Blocked) -> pure $ Existed s2o
      (Sent, Blocked) -> pure $ Existed s2o
      (Blocked, _) -> throwE $ InvalidTransition (tUnqualified self)
      (_, Blocked) -> change s2o SentWithHistory
      (_, Sent) -> accept s2o o2s
      (_, Accepted) -> accept s2o o2s
      (_, Ignored) -> resend s2o o2s
      (_, Pending) -> resend s2o o2s
      (_, Cancelled) -> resend s2o o2s

    accept :: UserConnection -> UserConnection -> ExceptT ConnectionError (AppT r) (ResponseForExistedCreated UserConnection)
    accept s2o o2s = do
      unless (ucStatus s2o `elem` [Sent, Accepted]) $
        checkLimit self
      lift . Log.info $
        logLocalConnection (tUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Accepting connection")
      cnv <- lift $ for (ucConvId s2o) $ Intra.acceptConnectConv self (Just conn)
      s2o' <- lift . wrapClient $ Data.updateConnection s2o AcceptedWithHistory
      o2s' <-
        lift . wrapClient $
          if (cnvType <$> cnv) == Just ConnectConv
            then Data.updateConnection o2s BlockedWithHistory
            else Data.updateConnection o2s AcceptedWithHistory
      e2o <-
        lift . wrapClient $
          ConnectionUpdated o2s' (Just $ ucStatus o2s)
            <$> Data.lookupName (tUnqualified self)
      let e2s = ConnectionUpdated s2o' (Just $ ucStatus s2o) Nothing
      lift $ liftSem $ mapM_ (Intra.onConnectionEvent (tUnqualified self) (Just conn)) [e2o, e2s]
      pure $ Existed s2o'

    resend :: UserConnection -> UserConnection -> ExceptT ConnectionError (AppT r) (ResponseForExistedCreated UserConnection)
    resend s2o o2s = do
      unless (ucStatus s2o `elem` [Sent, Accepted]) $
        checkLimit self
      lift . Log.info $
        logLocalConnection (tUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Resending connection request")
      s2o' <- insert (Just s2o) (Just o2s)
      pure $ Existed s2o'

    change :: UserConnection -> RelationWithHistory -> ExceptT ConnectionError (AppT r) (ResponseForExistedCreated UserConnection)
    change c s = Existed <$> lift (wrapClient $ Data.updateConnection c s)

-- | Throw error if one user has a LH device and the other status `no_consent` or vice versa.
--
-- FUTUREWORK: we may want to move this to the LH application logic, so we can recycle it for
-- group conv creation and possibly other situations.
checkLegalholdPolicyConflict ::
  Member GalleyProvider r =>
  UserId ->
  UserId ->
  ExceptT ConnectionError (AppT r) ()
checkLegalholdPolicyConflict uid1 uid2 = do
  let catchProfileNotFound =
        -- Does not fit into 'ExceptT', so throw in '(AppT r)'.  Anyway at the time of writing
        -- this, users are guaranteed to exist when called from 'createConnectionToLocalUser'.
        maybe (throwM (errorToWai @'E.UserNotFound)) pure

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

updateConnection ::
  ( Member FederationConfigStore r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member GalleyProvider r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Relation ->
  Maybe ConnId ->
  (ConnectionM r) (Maybe UserConnection)
updateConnection self other newStatus conn =
  let doUpdate =
        foldQualified
          self
          (updateConnectionToLocalUser self)
          (updateConnectionToRemoteUser self)
   in doUpdate other newStatus conn

-- | Change the status of a connection from one user to another.
--
-- Note: 'updateConnection' doesn't explicitly check that users don't belong to the same team,
-- because a connection between two team members can not exist in the first place.
-- {#RefConnectionTeam}
updateConnectionToLocalUser ::
  forall r.
  ( Member (Embed HttpClientIO) r,
    Member GalleyProvider r,
    Member NotificationSubsystem r,
    Member TinyLog r
  ) =>
  -- | From
  Local UserId ->
  -- | To
  Local UserId ->
  -- | Desired relation status
  Relation ->
  -- | Acting device connection ID
  Maybe ConnId ->
  (ConnectionM r) (Maybe UserConnection)
updateConnectionToLocalUser self other newStatus conn = do
  s2o <- localConnection self other
  o2s <- localConnection other self
  s2o' <- case (ucStatus s2o, ucStatus o2s, newStatus) of
    -- missing legalhold consent: call 'updateConectionInternal' instead.
    (MissingLegalholdConsent, _, _) -> throwE $ InvalidTransition (tUnqualified self)
    (_, MissingLegalholdConsent, _) -> throwE $ InvalidTransition (tUnqualified self)
    (_, _, MissingLegalholdConsent) -> throwE $ InvalidTransition (tUnqualified self)
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
    (old, _, new) | old == new -> pure Nothing
    -- invalid
    _ -> throwE $ InvalidTransition (tUnqualified self)
  let s2oUserConn = s2o'
  lift . liftSem . for_ s2oUserConn $ \c ->
    let e2s = ConnectionUpdated c (Just $ ucStatus s2o) Nothing
     in Intra.onConnectionEvent (tUnqualified self) conn e2s
  pure s2oUserConn
  where
    accept :: UserConnection -> UserConnection -> ExceptT ConnectionError (AppT r) (Maybe UserConnection)
    accept s2o o2s = do
      checkLimit self
      lift . Log.info $
        logLocalConnection (tUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Accepting connection")
      cnv <- lift $ traverse (Intra.acceptConnectConv self conn) (ucConvId s2o)
      -- Note: The check for @Pending@ accounts for situations in which both
      --       sides are pending, which can occur due to rare race conditions
      --       when sending mutual connection requests, combined with untimely
      --       crashes.
      when (ucStatus o2s `elem` [Sent, Pending]) . lift $ do
        o2s' <-
          wrapClient $
            if (cnvType <$> cnv) /= Just ConnectConv
              then Data.updateConnection o2s AcceptedWithHistory
              else Data.updateConnection o2s BlockedWithHistory
        e2o <-
          ConnectionUpdated o2s' (Just $ ucStatus o2s)
            <$> wrapClient (Data.lookupName (tUnqualified self))
        liftSem $ Intra.onConnectionEvent (tUnqualified self) conn e2o
      lift . wrapClient $ Just <$> Data.updateConnection s2o AcceptedWithHistory

    block :: UserConnection -> ExceptT ConnectionError (AppT r) (Maybe UserConnection)
    block s2o = lift $ do
      Log.info $
        logLocalConnection (tUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Blocking connection")
      traverse_ (liftSem . Intra.blockConv self) (ucConvId s2o)
      mlsEnabled <- view (settings . enableMLS)
      liftSem $ when (fromMaybe False mlsEnabled) $ do
        let mlsConvId = one2OneConvId BaseProtocolMLSTag (tUntagged self) (tUntagged other)
        mlsConvEstablished <- isMLSOne2OneEstablished self (tUntagged other)
        when mlsConvEstablished $ Intra.blockConv self mlsConvId
      wrapClient $ Just <$> Data.updateConnection s2o BlockedWithHistory

    unblock :: UserConnection -> UserConnection -> Relation -> ExceptT ConnectionError (AppT r) (Maybe UserConnection)
    unblock s2o o2s new = do
      -- FUTUREWORK: new is always in [Sent, Accepted]. Refactor to total function.
      when (new `elem` [Sent, Accepted]) $
        checkLimit self
      lift . Log.info $
        logLocalConnection (tUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Unblocking connection")
      cnv <- lift $ traverse (Intra.unblockConv self conn) (ucConvId s2o)
      when (ucStatus o2s == Sent && new == Accepted) . lift $ do
        o2s' <-
          wrapClient $
            if (cnvType <$> cnv) /= Just ConnectConv
              then Data.updateConnection o2s AcceptedWithHistory
              else Data.updateConnection o2s BlockedWithHistory
        e2o :: ConnectionEvent <-
          wrapClient $
            ConnectionUpdated o2s' (Just $ ucStatus o2s)
              <$> Data.lookupName (tUnqualified self)
        -- TODO: is this correct? shouldnt o2s be sent to other?
        liftSem $ Intra.onConnectionEvent (tUnqualified self) conn e2o
      lift . wrapClient $ Just <$> Data.updateConnection s2o (mkRelationWithHistory (error "impossible") new)

    cancel :: UserConnection -> UserConnection -> ExceptT ConnectionError (AppT r) (Maybe UserConnection)
    cancel s2o o2s = do
      lift . Log.info $
        logLocalConnection (tUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Cancelling connection")
      lfrom <- qualifyLocal (ucFrom s2o)
      lift $ traverse_ (liftSem . Intra.blockConv lfrom) (ucConvId s2o)
      o2s' <- lift . wrapClient $ Data.updateConnection o2s CancelledWithHistory
      let e2o = ConnectionUpdated o2s' (Just $ ucStatus o2s) Nothing
      lift $ liftSem $ Intra.onConnectionEvent (tUnqualified self) conn e2o
      change s2o Cancelled

    change :: UserConnection -> Relation -> ExceptT ConnectionError (AppT r) (Maybe UserConnection)
    change c s = do
      -- FUTUREWORK: refactor to total function. Gets only called with either Ignored, Accepted, Cancelled
      lift . wrapClient $ Just <$> Data.updateConnection c (mkRelationWithHistory (error "impossible") s)

localConnection ::
  Local UserId ->
  Local UserId ->
  ExceptT ConnectionError (AppT r) UserConnection
localConnection la lb = do
  lift (wrapClient $ Data.lookupConnection la (tUntagged lb))
    >>= tryJust (NotConnected (tUnqualified la) (tUntagged lb))

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
  forall r.
  ( Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r
  ) =>
  UpdateConnectionsInternal ->
  ExceptT ConnectionError (AppT r) ()
updateConnectionInternal = \case
  BlockForMissingLHConsent uid others -> do
    self <- qualifyLocal uid
    blockForMissingLegalholdConsent self others
  RemoveLHBlocksInvolving uid -> removeLHBlocksInvolving =<< qualifyLocal uid
  CreateConnectionForTest usr other -> do
    lusr <- qualifyLocal usr
    lift $
      foldQualified
        lusr
        (createLocalConnectionUnchecked lusr)
        (createRemoteConnectionUnchecked lusr)
        other
  where
    -- inspired by @block@ in 'updateConnection'.
    blockForMissingLegalholdConsent :: Local UserId -> [UserId] -> ExceptT ConnectionError (AppT r) ()
    blockForMissingLegalholdConsent self others = do
      for_ others $ \(qualifyAs self -> other) -> do
        lift . Log.info $
          logConnection (tUnqualified self) (tUntagged other)
            . msg (val "Blocking connection (legalhold device present, but missing consent)")

        s2o <- localConnection self other
        o2s <- localConnection other self
        for_ [s2o, o2s] $ \(uconn :: UserConnection) -> lift $ do
          lfrom <- qualifyLocal (ucFrom uconn)
          traverse_ (liftSem . Intra.blockConv lfrom) (ucConvId uconn)
          uconn' <- wrapClient $ Data.updateConnection uconn (mkRelationWithHistory (ucStatus uconn) MissingLegalholdConsent)
          let ev = ConnectionUpdated uconn' (Just $ ucStatus uconn) Nothing
          liftSem $ Intra.onConnectionEvent (tUnqualified self) Nothing ev

    removeLHBlocksInvolving :: Local UserId -> ExceptT ConnectionError (AppT r) ()
    removeLHBlocksInvolving self =
      iterateConnections self (toRange (Proxy @500)) $ \conns -> do
        for_ conns $ \s2o ->
          when (ucStatus s2o == MissingLegalholdConsent) $ do
            -- (this implies @ucStatus o2s == MissingLegalholdConsent@)
            -- Here we are using the fact that s2o is a local connection
            other <- qualifyLocal (qUnqualified (ucTo s2o))
            o2s <- localConnection other self
            lift . Log.info $
              logConnection (ucFrom s2o) (ucTo s2o)
                . msg (val "Unblocking connection (legalhold device removed or consent given)")
            unblockDirected s2o o2s
            unblockDirected o2s s2o
      where
        iterateConnections :: Local UserId -> Range 1 500 Int32 -> ([UserConnection] -> ExceptT ConnectionError (AppT r) ()) -> ExceptT ConnectionError (AppT r) ()
        iterateConnections user pageSize handleConns = go Nothing
          where
            go :: Maybe UserId -> ExceptT ConnectionError (AppT r) ()
            go mbStart = do
              page <- lift . wrapClient $ Data.lookupLocalConnections user mbStart pageSize
              handleConns (resultList page)
              case resultList page of
                (conn : rest) ->
                  when (resultHasMore page) $ go (Just (maximum (qUnqualified . ucTo <$> (conn : rest))))
                [] -> pure ()

        unblockDirected :: UserConnection -> UserConnection -> ExceptT ConnectionError (AppT r) ()
        unblockDirected uconn uconnRev = do
          lfrom <- qualifyLocal (ucFrom uconnRev)
          void . lift . for (ucConvId uconn) $ Intra.unblockConv lfrom Nothing
          uconnRevRel :: RelationWithHistory <- relationWithHistory lfrom (ucTo uconnRev)
          uconnRev' <- lift . wrapClient $ Data.updateConnection uconnRev (undoRelationHistory uconnRevRel)
          connName <- lift . wrapClient $ Data.lookupName (tUnqualified lfrom)
          let connEvent =
                ConnectionUpdated
                  { ucConn = uconnRev',
                    ucPrev = Just $ ucStatus uconnRev,
                    ucName = connName
                  }
          lift $ liftSem $ Intra.onConnectionEvent (ucFrom uconn) Nothing connEvent

    relationWithHistory ::
      Local UserId ->
      Qualified UserId ->
      ExceptT ConnectionError (AppT r) RelationWithHistory
    relationWithHistory self target =
      lift (wrapClient $ Data.lookupRelationWithHistory self target)
        >>= tryJust (NotConnected (tUnqualified self) target)

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

createLocalConnectionUnchecked :: Local UserId -> Local UserId -> (AppT r) ()
createLocalConnectionUnchecked self other = do
  qcnv <- liftIO $ tUntagged . qualifyAs self <$> (Id <$> UUID.nextRandom)
  wrapClient $ do
    void $ Data.insertConnection self (tUntagged other) AcceptedWithHistory qcnv
    void $ Data.insertConnection other (tUntagged self) AcceptedWithHistory qcnv

createRemoteConnectionUnchecked :: Local UserId -> Remote UserId -> (AppT r) ()
createRemoteConnectionUnchecked self other = do
  qcnv <- liftIO $ tUntagged . qualifyAs self <$> (Id <$> UUID.nextRandom)
  void . wrapClient $ Data.insertConnection self (tUntagged other) AcceptedWithHistory qcnv

lookupConnections :: UserId -> Maybe UserId -> Range 1 500 Int32 -> (AppT r) UserConnectionList
lookupConnections from start size = do
  lusr <- qualifyLocal from
  rs <- wrapClient $ Data.lookupLocalConnections lusr start size
  pure $! UserConnectionList (Data.resultList rs) (Data.resultHasMore rs)
