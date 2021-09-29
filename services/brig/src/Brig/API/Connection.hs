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
import Data.Qualified
import Data.Range
import Data.Tagged
import Galley.Types (ConvType (..), cnvType)
import Imports
import qualified System.Logger.Class as Log
import System.Logger.Message
import Wire.API.Connection (RelationWithHistory (..))
import Wire.API.ErrorDescription
import Wire.API.Federation.Error (federationNotImplemented)
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))

type ConnectionM = ExceptT ConnectionError AppIO

createConnection ::
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  ConnectionM (ResponseForExistedCreated UserConnection)
createConnection lusr con target = do
  let connect =
        foldQualified
          lusr
          createConnectionToLocalUser
          createConnectionToRemoteUser
  connect target lusr con

createConnectionToLocalUser ::
  Local UserId ->
  Local UserId ->
  ConnId ->
  ConnectionM (ResponseForExistedCreated UserConnection)
createConnectionToLocalUser self target conn = do
  when (self == target) $
    throwE (InvalidUser (unTagged target))
  selfActive <- lift $ Data.isActivated (lUnqualified self)
  unless selfActive $
    throwE ConnectNoIdentity
  otherActive <- lift $ Data.isActivated (lUnqualified target)
  unless otherActive $
    throwE (InvalidUser (unTagged target))
  checkLegalholdPolicyConflict (lUnqualified self) (lUnqualified target)
  -- Users belonging to the same team are always treated as connected, so creating a
  -- connection between them is useless. {#RefConnectionTeam}
  sameTeam <- lift belongSameTeam
  when sameTeam $
    throwE ConnectSameBindingTeamUsers
  s2o <- lift $ Data.lookupConnection self (unTagged target)
  o2s <- lift $ Data.lookupConnection target (unTagged self)

  case update <$> s2o <*> o2s of
    Just rs -> rs
    Nothing -> do
      checkLimit self
      Created <$> insert Nothing Nothing
  where
    insert :: Maybe UserConnection -> Maybe UserConnection -> ExceptT ConnectionError AppIO UserConnection
    insert s2o o2s = lift $ do
      Log.info $
        logConnection (lUnqualified self) (unTagged target)
          . msg (val "Creating connection")
      qcnv <- Intra.createConnectConv self (unTagged target) Nothing (Just conn)
      s2o' <- Data.insertConnection self (unTagged target) SentWithHistory qcnv
      o2s' <- Data.insertConnection target (unTagged self) PendingWithHistory qcnv
      e2o <-
        ConnectionUpdated o2s' (ucStatus <$> o2s)
          <$> Data.lookupName (lUnqualified self)
      let e2s = ConnectionUpdated s2o' (ucStatus <$> s2o) Nothing
      mapM_ (Intra.onConnectionEvent (lUnqualified self) (Just conn)) [e2o, e2s]
      return s2o'

    update :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO (ResponseForExistedCreated UserConnection)
    update s2o o2s = case (ucStatus s2o, ucStatus o2s) of
      (MissingLegalholdConsent, _) -> throwE $ InvalidTransition (lUnqualified self) Sent
      (_, MissingLegalholdConsent) -> throwE $ InvalidTransition (lUnqualified self) Sent
      (Accepted, Accepted) -> return $ Existed s2o
      (Accepted, Blocked) -> return $ Existed s2o
      (Sent, Blocked) -> return $ Existed s2o
      (Blocked, _) -> throwE $ InvalidTransition (lUnqualified self) Sent
      (_, Blocked) -> change s2o SentWithHistory
      (_, Sent) -> accept s2o o2s
      (_, Accepted) -> accept s2o o2s
      (_, Ignored) -> resend s2o o2s
      (_, Pending) -> resend s2o o2s
      (_, Cancelled) -> resend s2o o2s

    accept :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO (ResponseForExistedCreated UserConnection)
    accept s2o o2s = do
      when (ucStatus s2o `notElem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logLocalConnection (lUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Accepting connection")
      cnv <- lift $ for (ucConvId s2o) $ Intra.acceptConnectConv self (Just conn)
      s2o' <- lift $ Data.updateConnection s2o AcceptedWithHistory
      o2s' <-
        lift $
          if (cnvType <$> cnv) == Just ConnectConv
            then Data.updateConnection o2s BlockedWithHistory
            else Data.updateConnection o2s AcceptedWithHistory
      e2o <-
        lift $
          ConnectionUpdated o2s' (Just $ ucStatus o2s)
            <$> Data.lookupName (lUnqualified self)
      let e2s = ConnectionUpdated s2o' (Just $ ucStatus s2o) Nothing
      lift $ mapM_ (Intra.onConnectionEvent (lUnqualified self) (Just conn)) [e2o, e2s]
      return $ Existed s2o'

    resend :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO (ResponseForExistedCreated UserConnection)
    resend s2o o2s = do
      when (ucStatus s2o `notElem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logLocalConnection (lUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Resending connection request")
      s2o' <- insert (Just s2o) (Just o2s)
      return $ Existed s2o'

    change :: UserConnection -> RelationWithHistory -> ExceptT ConnectionError AppIO (ResponseForExistedCreated UserConnection)
    change c s = Existed <$> lift (Data.updateConnection c s)

    belongSameTeam :: AppIO Bool
    belongSameTeam = do
      selfTeam <- Intra.getTeamId (lUnqualified self)
      crTeam <- Intra.getTeamId (lUnqualified target)
      pure $ isJust selfTeam && selfTeam == crTeam

createConnectionToRemoteUser ::
  Remote UserId ->
  Local UserId ->
  ConnId ->
  ConnectionM (ResponseForExistedCreated UserConnection)
createConnectionToRemoteUser _ _ _ = throwM federationNotImplemented

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
  Local UserId ->
  -- | To
  Local UserId ->
  -- | Desired relation status
  Relation ->
  -- | Acting device connection ID
  Maybe ConnId ->
  ExceptT ConnectionError AppIO (Maybe UserConnection)
updateConnection self other newStatus conn = do
  s2o <- localConnection self other
  o2s <- localConnection other self
  s2o' <- case (ucStatus s2o, ucStatus o2s, newStatus) of
    -- missing legalhold consent: call 'updateConectionInternal' instead.
    (MissingLegalholdConsent, _, _) -> throwE $ InvalidTransition (lUnqualified self) newStatus
    (_, MissingLegalholdConsent, _) -> throwE $ InvalidTransition (lUnqualified self) newStatus
    (_, _, MissingLegalholdConsent) -> throwE $ InvalidTransition (lUnqualified self) newStatus
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
    _ -> throwE $ InvalidTransition (lUnqualified self) newStatus
  let s2oUserConn = s2o'
  lift . for_ s2oUserConn $ \c ->
    let e2s = ConnectionUpdated c (Just $ ucStatus s2o) Nothing
     in Intra.onConnectionEvent (lUnqualified self) conn e2s
  return s2oUserConn
  where
    accept :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    accept s2o o2s = do
      checkLimit self
      Log.info $
        logLocalConnection (lUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Accepting connection")
      cnv <- lift $ traverse (Intra.acceptConnectConv self conn) (ucConvId s2o)
      -- Note: The check for @Pending@ accounts for situations in which both
      --       sides are pending, which can occur due to rare race conditions
      --       when sending mutual connection requests, combined with untimely
      --       crashes.
      when (ucStatus o2s `elem` [Sent, Pending]) . lift $ do
        o2s' <-
          if (cnvType <$> cnv) /= Just ConnectConv
            then Data.updateConnection o2s AcceptedWithHistory
            else Data.updateConnection o2s BlockedWithHistory
        e2o <-
          ConnectionUpdated o2s' (Just $ ucStatus o2s)
            <$> Data.lookupName (lUnqualified self)
        Intra.onConnectionEvent (lUnqualified self) conn e2o
      lift $ Just <$> Data.updateConnection s2o AcceptedWithHistory

    block :: UserConnection -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    block s2o = lift $ do
      Log.info $
        logLocalConnection (lUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Blocking connection")
      traverse_ (Intra.blockConv self conn) (ucConvId s2o)
      Just <$> Data.updateConnection s2o BlockedWithHistory

    unblock :: UserConnection -> UserConnection -> Relation -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    unblock s2o o2s new = do
      -- FUTUREWORK: new is always in [Sent, Accepted]. Refactor to total function.
      when (new `elem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        logLocalConnection (lUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Unblocking connection")
      cnv <- lift $ traverse (Intra.unblockConv self conn) (ucConvId s2o)
      when (ucStatus o2s == Sent && new == Accepted) . lift $ do
        o2s' <-
          if (cnvType <$> cnv) /= Just ConnectConv
            then Data.updateConnection o2s AcceptedWithHistory
            else Data.updateConnection o2s BlockedWithHistory
        e2o :: ConnectionEvent <-
          ConnectionUpdated o2s' (Just $ ucStatus o2s)
            <$> Data.lookupName (lUnqualified self)
        -- TODO: is this correct? shouldnt o2s be sent to other?
        Intra.onConnectionEvent (lUnqualified self) conn e2o
      lift $ Just <$> Data.updateConnection s2o (mkRelationWithHistory (error "impossible") new)

    cancel :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    cancel s2o o2s = do
      Log.info $
        logLocalConnection (lUnqualified self) (qUnqualified (ucTo s2o))
          . msg (val "Cancelling connection")
      lfrom <- qualifyLocal (ucFrom s2o)
      lift $ traverse_ (Intra.blockConv lfrom conn) (ucConvId s2o)
      o2s' <- lift $ Data.updateConnection o2s CancelledWithHistory
      let e2o = ConnectionUpdated o2s' (Just $ ucStatus o2s) Nothing
      lift $ Intra.onConnectionEvent (lUnqualified self) conn e2o
      change s2o Cancelled

    change :: UserConnection -> Relation -> ExceptT ConnectionError AppIO (Maybe UserConnection)
    change c s = do
      -- FUTUREWORK: refactor to total function. Gets only called with either Ignored, Accepted, Cancelled
      lift $ Just <$> Data.updateConnection c (mkRelationWithHistory (error "impossible") s)

localConnection ::
  Local UserId ->
  Local UserId ->
  ExceptT ConnectionError AppIO UserConnection
localConnection la lb = do
  lift (Data.lookupConnection la (unTagged lb))
    >>= tryJust (NotConnected (lUnqualified la) (unTagged lb))

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
  BlockForMissingLHConsent uid others -> do
    self <- qualifyLocal uid
    blockForMissingLegalholdConsent self others
  RemoveLHBlocksInvolving uid -> removeLHBlocksInvolving =<< qualifyLocal uid
  where
    -- inspired by @block@ in 'updateConnection'.
    blockForMissingLegalholdConsent :: Local UserId -> [UserId] -> ExceptT ConnectionError AppIO ()
    blockForMissingLegalholdConsent self others = do
      for_ others $ \(qualifyAs self -> other) -> do
        Log.info $
          logConnection (lUnqualified self) (unTagged other)
            . msg (val "Blocking connection (legalhold device present, but missing consent)")

        s2o <- localConnection self other
        o2s <- localConnection other self
        for_ [s2o, o2s] $ \(uconn :: UserConnection) -> lift $ do
          lfrom <- qualifyLocal (ucFrom uconn)
          traverse_ (Intra.blockConv lfrom Nothing) (ucConvId uconn)
          uconn' <- Data.updateConnection uconn (mkRelationWithHistory (ucStatus uconn) MissingLegalholdConsent)
          let ev = ConnectionUpdated uconn' (Just $ ucStatus uconn) Nothing
          Intra.onConnectionEvent (lUnqualified self) Nothing ev

    removeLHBlocksInvolving :: Local UserId -> ExceptT ConnectionError AppIO ()
    removeLHBlocksInvolving self =
      iterateConnections self (toRange (Proxy @500)) $ \conns -> do
        for_ conns $ \s2o ->
          when (ucStatus s2o == MissingLegalholdConsent) $ do
            -- (this implies @ucStatus o2s == MissingLegalholdConsent@)
            -- Here we are using the fact that s2o is a local connection
            other <- qualifyLocal (qUnqualified (ucTo s2o))
            o2s <- localConnection other self
            Log.info $
              logConnection (ucFrom s2o) (ucTo s2o)
                . msg (val "Unblocking connection (legalhold device removed or consent given)")
            unblockDirected s2o o2s
            unblockDirected o2s s2o
      where
        iterateConnections :: Local UserId -> Range 1 500 Int32 -> ([UserConnection] -> ExceptT ConnectionError AppIO ()) -> ExceptT ConnectionError AppIO ()
        iterateConnections user pageSize handleConns = go Nothing
          where
            go :: Maybe UserId -> ExceptT ConnectionError (AppT IO) ()
            go mbStart = do
              page <- lift $ Data.lookupLocalConnections user mbStart pageSize
              handleConns (resultList page)
              case resultList page of
                (conn : rest) ->
                  if resultHasMore page
                    then go (Just (maximum (qUnqualified . ucTo <$> (conn : rest))))
                    else pure ()
                [] -> pure ()

        unblockDirected :: UserConnection -> UserConnection -> ExceptT ConnectionError AppIO ()
        unblockDirected uconn uconnRev = do
          lfrom <- qualifyLocal (ucFrom uconnRev)
          void . lift . for (ucConvId uconn) $ Intra.unblockConv lfrom Nothing
          uconnRevRel :: RelationWithHistory <- relationWithHistory lfrom (ucTo uconnRev)
          uconnRev' <- lift $ Data.updateConnection uconnRev (undoRelationHistory uconnRevRel)
          connName <- lift $ Data.lookupName (lUnqualified lfrom)
          let connEvent =
                ConnectionUpdated
                  { ucConn = uconnRev',
                    ucPrev = Just $ ucStatus uconnRev,
                    ucName = connName
                  }
          lift $ Intra.onConnectionEvent (ucFrom uconn) Nothing connEvent

    relationWithHistory :: Local UserId -> Qualified UserId -> ExceptT ConnectionError AppIO RelationWithHistory
    relationWithHistory self target =
      lift (Data.lookupRelationWithHistory self target)
        >>= tryJust (NotConnected (lUnqualified self) target)

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
  lusr <- qualifyLocal from
  rs <- Data.lookupLocalConnections lusr start size
  return $! UserConnectionList (Data.resultList rs) (Data.resultHasMore rs)

-- Helpers

checkLimit :: Local UserId -> ExceptT ConnectionError AppIO ()
checkLimit u = do
  n <- lift $ Data.countConnections u [Accepted, Sent]
  l <- setUserMaxConnections <$> view settings
  unless (n < l) $
    throwE $
      TooManyConnections (lUnqualified u)
