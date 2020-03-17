{-# LANGUAGE RecordWildCards #-}

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

import Brig.API.Types
import Brig.API.Util (resolveOpaqueUserId)
import Brig.App
import qualified Brig.Data.Connection as Data
import qualified Brig.Data.User as Data
import qualified Brig.IO.Intra as Intra
import Brig.Options (setUserMaxConnections)
import Brig.Types
import Brig.Types.Intra
import Brig.User.Event
import qualified Brig.User.Event.Log as Log
import Control.Error
import Control.Lens (view)
import Data.Id as Id
import Data.IdMapping (IdMapping (IdMapping, idMappingLocal), MappedOrLocalId (Local, Mapped))
import Data.Range
import qualified Data.Set as Set
import Galley.Types (ConvType (..), cnvType)
import qualified Galley.Types.Teams as Team
import Imports
import qualified System.Logger.Class as Log
import System.Logger.Message

-- ConnectionUpdated event to self and other, unless both states already exist and any state is blocked or both already accepted
--
-- potentially can cause events to be sent from Galley:
-- if only the other already was member of connect conversation and has connection status Sent or Accepted:
--   via galley: MemberJoin EdMembersJoin event to you and other
--
--  Also possible (for details check 'Galley.API.Create.createConnectConversation'):
--   via galley: ConvCreate EdConversation event to self
--   via galley: ConvConnect EdConnect event to self
createConnection ::
  N ->
  E ->
  UserId ->
  ConnectionRequest ->
  ConnId ->
  ExceptT ConnectionError AppIO ConnectionResult
createConnection N E self req conn = do
  resolveOpaqueUserId (crUser req) >>= \case
    Local u ->
      createConnectionToLocalUser N E self u req conn
    Mapped IdMapping {idMappingLocal} ->
      -- FUTUREWORK(federation): allow creating connections to remote users
      throwE $ InvalidUser (makeMappedIdOpaque idMappingLocal)

-- ConnectionUpdated event to self and other, unless both states already exist and any state is blocked or both already accepted
--
-- potentially can cause events to be sent from Galley:
-- if only the other already was member of connect conversation and has connection status Sent or Accepted:
--   via galley: MemberJoin EdMembersJoin event to you and other
--
--  Also possible (for details check 'Galley.API.Create.createConnectConversation'):
--   via galley: ConvCreate EdConversation event to self
--   via galley: ConvConnect EdConnect event to self
createConnectionToLocalUser ::
  N ->
  E ->
  UserId ->
  UserId ->
  ConnectionRequest ->
  ConnId ->
  ExceptT ConnectionError AppIO ConnectionResult
createConnectionToLocalUser N E self crUser ConnectionRequest {crName, crMessage} conn = do
  when (self == crUser)
    $ throwE
    $ InvalidUser (makeIdOpaque crUser)
  selfActive <- lift $ Data.isActivated self
  unless selfActive $
    throwE ConnectNoIdentity
  otherActive <- lift $ Data.isActivated crUser
  unless otherActive
    $ throwE
    $ InvalidUser (makeIdOpaque crUser)
  -- Users belonging to the same team are always treated as connected, so creating a
  -- connection between them is useless. {#RefConnectionTeam}
  sameTeam <- lift $ belongSameTeam
  when sameTeam $
    throwE ConnectSameBindingTeamUsers
  s2o <- lift $ Data.lookupConnection self crUser
  o2s <- lift $ Data.lookupConnection crUser self
  case update N E <$> s2o <*> o2s of
    Just rs ->
      -- ConnectionUpdated event to self, unless any state is blocked or both already accepted
      -- ConnectionUpdated event to other, unless any state is blocked or both already accepted
      -- if only the other already was member of connect conversation before and has status Sent or Accepted:
      --   via galley: MemberJoin EdMembersJoin event to you
      --   via galley: MemberJoin EdMembersJoin event to other
      rs
    Nothing -> do
      checkLimit self
      -- ConnectionUpdated event to self
      -- ConnectionUpdated event to other
      --
      -- if connect conversation did not exist before:
      --   via galley: ConvCreate EdConversation event to self
      --   via galley: ConvConnect EdConnect event to self
      -- if conversation existed, but other didn't join/accept yet;
      --   via galley: ConvConnect EdConnect event to self
      ConnectionCreated <$> insert E Nothing Nothing
  where
    -- ConnectionUpdated event to self
    -- ConnectionUpdated event to other
    --
    -- if connect conversation did not exist before:
    --   via galley: ConvCreate EdConversation event to self
    --   via galley: ConvConnect EdConnect event to self
    -- if conversation existed, but other didn't join/accept yet;
    --   via galley: ConvConnect EdConnect event to self
    insert E s2o o2s = lift $ do
      Log.info $
        Log.connection self crUser
          . msg (val "Creating connection")
      -- via galley, if conversation did not exist before:
      --   ConvCreate EdConversation event to self
      --   ConvConnect EdConnect event to self
      -- via galley, if conversation existed, but other didn't join/accept yet;
      --   ConvConnect EdConnect event to self
      cnv <- Intra.createConnectConv N self crUser (Just crName) (Just crMessage) (Just conn)
      s2o' <- Data.insertConnection self crUser Sent (Just crMessage) cnv
      o2s' <- Data.insertConnection crUser self Pending (Just crMessage) cnv
      e2o <- ConnectionUpdated o2s' (ucStatus <$> o2s) <$> Data.lookupName self
      e2s <- pure $ ConnectionUpdated s2o' (ucStatus <$> s2o) Nothing
      -- ConnectionUpdated event to self
      -- ConnectionUpdated event to other
      mapM_ (Intra.onConnectionEvent E self (Just conn)) [e2o, e2s]
      return s2o'
    -- ConnectionUpdated event to self, unless any state is blocked or both already accepted
    -- ConnectionUpdated event to other, unless any state is blocked or both already accepted
    -- if only the other already was member of connect conversation before and has status Sent or Accepted:
    --   via galley: MemberJoin EdMembersJoin event to you
    --   via galley: MemberJoin EdMembersJoin event to other
    --
    -- if conversation existed, but other didn't join/accept yet;
    --   via galley: ConvConnect EdConnect event to self
    update N E s2o o2s = case (ucStatus s2o, ucStatus o2s) of
      (Accepted, Accepted) -> return $ ConnectionExists s2o
      (Accepted, Blocked) -> return $ ConnectionExists s2o
      (Sent, Blocked) -> return $ ConnectionExists s2o
      (Blocked, _) -> throwE $ InvalidTransition self Sent
      (_, Blocked) -> change s2o Sent
      -- ConnectionUpdated event to self
      -- ConnectionUpdated event to other
      -- if the conversation existed and had < 2 members before
      --   via galley: MemberJoin EdMembersJoin event to you
      -- if the conversation existed and only the other already was member before
      --   via galley: MemberJoin EdMembersJoin event to other
      (_, Sent) -> accept N E s2o o2s
      (_, Accepted) -> accept N E s2o o2s
      -- ConnectionUpdated event to self
      -- ConnectionUpdated event to other
      --
      -- if connect conversation did not exist before (not possible here?):
      --   via galley: ConvCreate EdConversation event to self
      --   via galley: ConvConnect EdConnect event to self
      -- if conversation existed, but other didn't join/accept yet;
      --   via galley: ConvConnect EdConnect event to self
      (_, Ignored) -> resend E s2o o2s
      (_, Pending) -> resend E s2o o2s
      (_, Cancelled) -> resend E s2o o2s
    -- ConnectionUpdated event to self
    -- ConnectionUpdated event to other
    --
    -- if the conversation existed and had < 2 members before
    --   via galley: MemberJoin EdMembersJoin event to you
    -- if the conversation existed and only the other already was member before
    --   via galley: MemberJoin EdMembersJoin event to other
    accept N E s2o o2s = do
      when (ucStatus s2o `notElem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        Log.connection self (ucTo s2o)
          . msg (val "Accepting connection")
      -- if the conversation existed and had < 2 members before
      --   via galley: MemberJoin EdMembersJoin event to you
      -- if the conversation existed and only the other already was member before
      --   via galley: MemberJoin EdMembersJoin event to other
      cnv <- lift $ for (ucConvId s2o) $ Intra.acceptConnectConv N self (Just conn)
      s2o' <- lift $ Data.updateConnection s2o Accepted
      o2s' <-
        lift $
          if (cnvType <$> cnv) == Just ConnectConv
            then Data.updateConnection o2s Blocked
            else Data.updateConnection o2s Accepted
      e2o <- lift $ ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
      e2s <- pure $ ConnectionUpdated s2o' (Just $ ucStatus s2o) Nothing
      -- ConnectionUpdated event to self
      -- ConnectionUpdated event to other
      lift $ mapM_ (Intra.onConnectionEvent E self (Just conn)) [e2o, e2s]
      return $ ConnectionExists s2o'
    -- ConnectionUpdated event to self
    -- ConnectionUpdated event to other
    --
    -- if connect conversation did not exist before:
    --   via galley: ConvCreate EdConversation event to self
    --   via galley: ConvConnect EdConnect event to self
    -- if conversation existed, but other didn't join/accept yet;
    --   via galley: ConvConnect EdConnect event to self
    resend E s2o o2s = do
      when (ucStatus s2o `notElem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        Log.connection self (ucTo s2o)
          . msg (val "Resending connection request")
      -- ConnectionUpdated event to self
      -- ConnectionUpdated event to other
      --
      -- if connect conversation did not exist before:
      --   via galley: ConvCreate EdConversation event to self
      --   via galley: ConvConnect EdConnect event to self
      -- if conversation existed, but other didn't join/accept yet;
      --   via galley: ConvConnect EdConnect event to self
      s2o' <- insert E (Just s2o) (Just o2s)
      return $ ConnectionExists s2o'
    change c s = ConnectionExists <$> lift (Data.updateConnection c s)
    belongSameTeam = do
      selfTeam <- Intra.getTeamId self
      crTeam <- Intra.getTeamId crUser
      pure $ isJust selfTeam && selfTeam == crTeam

-- | Change the status of a connection from one user to another.
--
-- Note: 'updateConnection' doesn't explicitly check that users don't belong to the same team,
-- because a connection between two team members can not exist in the first place.
-- {#RefConnectionTeam}
--
-- ConnectionUpdated event to self, if our state changes
-- ConnectionUpdated event to other, if their state changes as well
--
-- when moving to Sent or Accepted, this potentially can cause events to be sent from Galley when joining the connect conversation:
--   via galley: MemberJoin EdMembersJoin event to you
--   via galley: MemberJoin EdMembersJoin event to other
updateConnection ::
  N ->
  E ->
  -- | From
  UserId ->
  -- | To
  UserId ->
  -- | Desired relation status
  Relation ->
  -- | Acting device connection ID
  Maybe ConnId ->
  ExceptT ConnectionError AppIO (Maybe UserConnection)
updateConnection N E self other newStatus conn = do
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
    (Sent, Sent, Accepted) ->
      change s2o Accepted
        >> change o2s Accepted
    (Sent, Accepted, Accepted) -> change s2o Accepted
    (Sent, Blocked, Cancelled) -> change s2o Cancelled
    (Sent, Cancelled, Cancelled) -> change s2o Cancelled
    (Sent, Pending, Cancelled) -> cancel s2o o2s
    (Sent, Ignored, Cancelled) -> cancel s2o o2s
    -- Cancelled -> {Blocked}
    (Cancelled, _, Blocked) -> block s2o
    (old, _, new)
      | old == new -> return Nothing
    _ -> throwE $ InvalidTransition self newStatus
  lift $ for_ s2o' $ \c ->
    let e2s = ConnectionUpdated c (Just $ ucStatus s2o) Nothing
     in -- ConnectionUpdated event to self
        Intra.onConnectionEvent E self conn e2s
  return s2o'
  where
    -- if the conversation existed and had < 2 members before
    --   via galley: MemberJoin EdMembersJoin event to you
    -- if the conversation existed and only the other already was member before
    --   via galley: MemberJoin EdMembersJoin event to other
    accept s2o o2s = do
      checkLimit self
      Log.info $
        Log.connection self (ucTo s2o)
          . msg (val "Accepting connection")
      -- if the conversation existed and had < 2 members before
      --   via galley: MemberJoin EdMembersJoin event to you
      -- if the conversation existed and only the other already was member before
      --   via galley: MemberJoin EdMembersJoin event to other
      cnv <- lift $ for (ucConvId s2o) $ Intra.acceptConnectConv N self conn
      -- Note: The check for @Pending@ accounts for situations in which both
      --       sides are pending, which can occur due to rare race conditions
      --       when sending mutual connection requests, combined with untimely
      --       crashes.
      when (ucStatus o2s `elem` [Sent, Pending]) $ lift $ do
        o2s' <-
          if (cnvType <$> cnv) /= Just ConnectConv
            then Data.updateConnection o2s Accepted
            else Data.updateConnection o2s Blocked
        e2o <- ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
        -- ConnectionUpdated event to other
        Intra.onConnectionEvent E self conn e2o
      lift $ Just <$> Data.updateConnection s2o Accepted
    block s2o = lift $ do
      Log.info $
        Log.connection self (ucTo s2o)
          . msg (val "Blocking connection")
      for_ (ucConvId s2o) $ Intra.blockConv (ucFrom s2o) conn
      Just <$> Data.updateConnection s2o Blocked
    -- if the conversation existed and had < 2 members before
    --   via galley: MemberJoin EdMembersJoin event to you
    -- if the conversation existed and only the other already was member before
    --   via galley: MemberJoin EdMembersJoin event to other
    unblock s2o o2s new = do
      when (new `elem` [Sent, Accepted]) $
        checkLimit self
      Log.info $
        Log.connection self (ucTo s2o)
          . msg (val "Unblocking connection")
      -- if the conversation existed and had < 2 members before
      --   via galley: MemberJoin EdMembersJoin event to you
      -- if the conversation existed and only the other already was member before
      --   via galley: MemberJoin EdMembersJoin event to other
      cnv <- lift $ for (ucConvId s2o) $ Intra.unblockConv N (ucFrom s2o) conn
      when (ucStatus o2s == Sent && new == Accepted) $ lift $ do
        o2s' <-
          if (cnvType <$> cnv) /= Just ConnectConv
            then Data.updateConnection o2s Accepted
            else Data.updateConnection o2s Blocked
        e2o <- ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
        -- ConnectionUpdated event to other
        Intra.onConnectionEvent E self conn e2o
      lift $ Just <$> Data.updateConnection s2o new
    cancel s2o o2s = do
      Log.info $
        Log.connection self (ucTo s2o)
          . msg (val "Cancelling connection")
      lift $ for_ (ucConvId s2o) $ Intra.blockConv (ucFrom s2o) conn
      o2s' <- lift $ Data.updateConnection o2s Cancelled
      let e2o = ConnectionUpdated o2s' (Just $ ucStatus o2s) Nothing
      -- ConnectionUpdated event to other
      lift $ Intra.onConnectionEvent E self conn e2o
      change s2o Cancelled
    change c s = lift $ Just <$> Data.updateConnection c s
    connection a b = lift (Data.lookupConnection a b) >>= tryJust (NotConnected a b)

-- ConnectionUpdated event to self and users connecting with
--
-- This will cause events to be sent from Galley:
-- for connect conversations that did not exist before:
--   via galley: ConvCreate EdConversation event to self
-- if others didn't join a connect conversation with self before:
--   via galley: ConvConnect EdConnect event to self
--   via galley: MemberJoin EdMembersJoin event to you and other
autoConnect ::
  N ->
  E ->
  UserId ->
  Set UserId ->
  Maybe ConnId ->
  ExceptT ConnectionError AppIO [UserConnection]
autoConnect N E from (Set.toList -> to) conn = do
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
      mems <- lift $ Intra.getTeamContacts from
      return $ maybe us (Team.notTeamMember us . view Team.teamMembers) mems
    connectAll activeOthers = do
      others <- selectOthers activeOthers
      -- if conversation did not exist before:
      --   via galley: ConvCreate EdConversation event to self
      -- if other didn't join the connect conversation yet:
      --   via galley: ConvConnect EdConnect event to self
      --   via galley: MemberJoin EdMembersJoin event to you and other
      convs <- mapM (createConv from) others
      self <- Data.lookupName from
      ucs <- Data.connectUsers from convs
      let events = map (toEvent self) ucs
      -- ConnectionUpdated event to self and users connecting with
      forM_ events $ Intra.onConnectionEvent E from conn
      return ucs
    -- Assumption: if there's an existing connection, don't touch it.
    -- The exception to this rule _could_ be a sent/pending connection
    -- but for sure we would not override states like `blocked` and `ignored`
    -- For simplicity, let's just not touch them.
    selectOthers usrs = do
      existing <- map csFrom <$> Data.lookupConnectionStatus usrs [from]
      return $ filter (`notElem` existing) usrs
    -- Can send some events via galley:
    --
    -- if conversation did not exist before:
    --   via galley: ConvCreate EdConversation event to self
    -- if other didn't join the connect conversation yet:
    --   via galley: ConvConnect EdConnect event to self
    --   via galley: MemberJoin EdMembersJoin event to you and other
    createConv s o = do
      -- via galley, if conversation did not exist before:
      --   ConvCreate EdConversation event to self
      --   ConvConnect EdConnect event to self
      -- via galley, if conversation existed, but other didn't join/accept yet;
      --   ConvConnect EdConnect event to self
      c <- Intra.createConnectConv N s o Nothing Nothing conn
      -- if the conversation existed and had < 2 members before
      --   via galley: MemberJoin EdMembersJoin event to you
      -- if the conversation existed and only the other already was member before
      --   via galley: MemberJoin EdMembersJoin event to other
      -- NOTE that this accepts as the other!
      _ <- Intra.acceptConnectConv N o conn c
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
  unless (n < l)
    $ throwE
    $ TooManyConnections u
