{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- TODO: Move to Brig.User.Connection (& split out Brig.User.Invitation?)
module Brig.API.Connection
    ( -- * Connections
      autoConnect
    , createConnection
    , updateConnection
    , lookupConnections
    , Data.lookupConnection
    , Data.lookupConnectionStatus
    , Data.lookupContactList

      -- * Invitations
    , createInvitation
    , lookupInvitations
    , deleteInvitation
    , Data.lookupInvitation
    , Data.lookupInvitationCode
    , Data.lookupInvitationByCode

      -- * Onboarding
    , onboarding
    ) where

import Brig.App
import Brig.API.Types
import Brig.Data.UserKey (userEmailKey)
import Brig.Options (setUserMaxConnections)
import Brig.Types
import Brig.Types.Intra
import Brig.User.Email (sendInvitationMail, validateEmail)
import Brig.User.Event
import Control.Concurrent.Async (mapConcurrently)
import Control.Error
import Control.Lens (view, (^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Data.ByteString (ByteString)
import Data.Id
import Data.Int (Int32)
import Data.Foldable (for_)
import Data.List.Extra (chunksOf)
import Data.Range
import Data.Set (Set, fromList)
import Data.Traversable (for)
import Galley.Types (cnvType, ConvType (..))
import System.Logger.Message

import qualified Brig.Blacklist       as Blacklist
import qualified Brig.Data.Connection as Data
import qualified Brig.Data.Invitation as Data
import qualified Brig.Data.User       as Data
import qualified Brig.Data.UserKey    as Data
import qualified Brig.IO.Intra        as Intra
import qualified Brig.User.Event.Log  as Log
import qualified Data.Set             as Set
import qualified Galley.Types.Teams   as Team
import qualified System.Logger.Class  as Log

createConnection :: UserId
                 -> ConnectionRequest
                 -> ConnId
                 -> ExceptT ConnectionError AppIO ConnectionResult
createConnection self ConnectionRequest{..} conn = do
    when (self == crUser) $
        throwE $ InvalidUser crUser

    selfActive <- lift $ Data.isActivated self
    unless selfActive $
        throwE ConnectNoIdentity

    otherActive <- lift $ Data.isActivated crUser
    unless otherActive $
        throwE $ InvalidUser crUser
    
    sameTeam <- lift $ belongSameTeam
    when sameTeam $
        throwE ConnectSameBindingTeamUsers
    
    s2o <- lift $ Data.lookupConnection self   crUser
    o2s <- lift $ Data.lookupConnection crUser self

    case update <$> s2o <*> o2s of
        Just rs -> rs
        Nothing -> do
            checkLimit self
            ConnectionCreated <$> insert Nothing Nothing
  where
    insert s2o o2s = lift $ do
        Log.info $ Log.connection self crUser
                 . msg (val "Creating connection")
        cnv  <- Intra.createConnectConv self crUser (Just crName) (Just crMessage) (Just conn)
        s2o' <- Data.insertConnection self   crUser Sent    (Just crMessage) cnv
        o2s' <- Data.insertConnection crUser self   Pending (Just crMessage) cnv
        e2o  <- ConnectionUpdated o2s' (ucStatus <$> o2s) <$> Data.lookupName self
        e2s  <- pure $ ConnectionUpdated s2o' (ucStatus <$> s2o) Nothing
        mapM_ (Intra.onConnectionEvent self (Just conn)) [e2o, e2s]
        return s2o'

    update s2o o2s = case (ucStatus s2o, ucStatus o2s) of
        (Accepted,  Accepted) -> return $ ConnectionExists s2o
        (Accepted,   Blocked) -> return $ ConnectionExists s2o
        (    Sent,   Blocked) -> return $ ConnectionExists s2o
        ( Blocked,         _) -> throwE $ InvalidTransition self Sent
        (       _,   Blocked) -> change s2o Sent
        (       _,      Sent) -> accept s2o o2s
        (       _,  Accepted) -> accept s2o o2s
        (       _,   Ignored) -> resend s2o o2s
        (       _,   Pending) -> resend s2o o2s
        (       _, Cancelled) -> resend s2o o2s

    accept s2o o2s = do
        when (ucStatus s2o `notElem` [Sent, Accepted]) $
            checkLimit self
        Log.info $ Log.connection self (ucTo s2o)
                 . msg (val "Accepting connection")
        cnv  <- lift $ for (ucConvId s2o) $ Intra.acceptConnectConv self (Just conn)
        s2o' <- lift $ Data.updateConnection s2o Accepted
        o2s' <- lift $ if (cnvType <$> cnv) == Just ConnectConv
            then Data.updateConnection o2s Blocked
            else Data.updateConnection o2s Accepted
        e2o  <- lift $ ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
        e2s  <- pure $ ConnectionUpdated s2o' (Just $ ucStatus s2o) Nothing
        lift $ mapM_ (Intra.onConnectionEvent self (Just conn)) [e2o, e2s]
        return $ ConnectionExists s2o'

    resend s2o o2s = do
        when (ucStatus s2o `notElem` [Sent, Accepted]) $
            checkLimit self
        Log.info $ Log.connection self (ucTo s2o)
                 . msg (val "Resending connection request")
        s2o' <- insert (Just s2o) (Just o2s)
        return $ ConnectionExists s2o'

    change c s = ConnectionExists <$> lift (Data.updateConnection c s)

    belongSameTeam = Intra.getTeamContacts self >>= \case
        Just mems -> return $ Team.isTeamMember crUser (mems^.Team.teamMembers)
        _         -> return False

updateConnection :: UserId       -- ^ From
                 -> UserId       -- ^ To
                 -> Relation     -- ^ Desired relation status
                 -> Maybe ConnId -- ^ Acting device connection ID
                 -> ExceptT ConnectionError AppIO (Maybe UserConnection)
updateConnection self other newStatus conn = do
    s2o  <- connection self other
    o2s  <- connection other self
    s2o' <- case (ucStatus s2o, ucStatus o2s, newStatus) of
        -- Pending -> {Blocked, Ignored, Accepted}
        (Pending,          _,   Blocked) -> block s2o
        (Pending,          _,   Ignored) -> change s2o Ignored
        (Pending,          _,  Accepted) -> accept s2o o2s

        -- Ignored -> {Accepted, Blocked}
        (Ignored,          _,  Accepted) -> accept s2o o2s
        (Ignored,          _,   Blocked) -> block s2o

        -- Blocked -> {Accepted, Sent}
        (Blocked,   Accepted,  Accepted) -> unblock s2o o2s Accepted
        (Blocked,    Blocked,  Accepted) -> unblock s2o o2s Accepted
        (Blocked,       Sent,  Accepted) -> unblock s2o o2s Accepted
        (Blocked,    Pending,  Accepted) -> unblock s2o o2s Sent
        (Blocked,    Ignored,  Accepted) -> unblock s2o o2s Sent
        (Blocked,  Cancelled,  Accepted) -> unblock s2o o2s Sent
        (Blocked,   Accepted,      Sent) -> unblock s2o o2s Accepted
        (Blocked,    Blocked,      Sent) -> unblock s2o o2s Accepted
        (Blocked,       Sent,      Sent) -> unblock s2o o2s Accepted
        (Blocked,    Pending,      Sent) -> unblock s2o o2s Sent
        (Blocked,    Ignored,      Sent) -> unblock s2o o2s Sent
        (Blocked,  Cancelled,      Sent) -> unblock s2o o2s Sent

        -- Accepted -> {Blocked}
        (Accepted,         _,   Blocked) -> block s2o

        -- Sent -> {Blocked, Cancelled, Accepted}
        (Sent,             _,   Blocked) -> block s2o
        (Sent,          Sent,  Accepted) -> change s2o Accepted
                                         >> change o2s Accepted
        (Sent,      Accepted,  Accepted) -> change s2o Accepted
        (Sent,       Blocked, Cancelled) -> change s2o Cancelled
        (Sent,     Cancelled, Cancelled) -> change s2o Cancelled
        (Sent,       Pending, Cancelled) -> cancel s2o o2s
        (Sent,       Ignored, Cancelled) -> cancel s2o o2s

        -- Cancelled -> {Blocked}
        (Cancelled,        _,   Blocked) -> block s2o

        (old,              _,      new)
            | old == new                -> return Nothing
        _                               -> throwE $ InvalidTransition self newStatus
    lift $ for_ s2o' $ \c ->
        let e2s = ConnectionUpdated c (Just $ ucStatus s2o) Nothing
        in Intra.onConnectionEvent self conn e2s
    return s2o'
  where
    accept s2o o2s = do
        checkLimit self
        Log.info $ Log.connection self (ucTo s2o)
                 . msg (val "Accepting connection")
        cnv <- lift $ for (ucConvId s2o) $ Intra.acceptConnectConv self conn
        -- Note: The check for @Pending@ accounts for situations in which both
        --       sides are pending, which can occur due to rare race conditions
        --       when sending mutual connection requests, combined with untimely
        --       crashes.
        when (ucStatus o2s `elem` [Sent, Pending]) $ lift $ do
            o2s' <- if (cnvType <$> cnv) /= Just ConnectConv
                then Data.updateConnection o2s Accepted
                else Data.updateConnection o2s Blocked
            e2o  <- ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
            Intra.onConnectionEvent self conn e2o
        lift $ Just <$> Data.updateConnection s2o Accepted

    block s2o = lift $ do
        Log.info $ Log.connection self (ucTo s2o)
                 . msg (val "Blocking connection")
        for_ (ucConvId s2o) $ Intra.blockConv (ucFrom s2o) conn
        Just <$> Data.updateConnection s2o Blocked

    unblock s2o o2s new = do
        when (new `elem` [Sent, Accepted]) $
            checkLimit self
        Log.info $ Log.connection self (ucTo s2o)
                 . msg (val "Unblocking connection")
        cnv <- lift $ for (ucConvId s2o) $ Intra.unblockConv (ucFrom s2o) conn
        when (ucStatus o2s == Sent && new == Accepted) $ lift $ do
            o2s' <- if (cnvType <$> cnv) /= Just ConnectConv
                then Data.updateConnection o2s Accepted
                else Data.updateConnection o2s Blocked
            e2o  <- ConnectionUpdated o2s' (Just $ ucStatus o2s) <$> Data.lookupName self
            Intra.onConnectionEvent self conn e2o
        lift $ Just <$> Data.updateConnection s2o new

    cancel s2o o2s = do
        Log.info $ Log.connection self (ucTo s2o)
                 . msg (val "Cancelling connection")
        lift $ for_ (ucConvId s2o) $ Intra.blockConv (ucFrom s2o) conn
        o2s' <- lift $ Data.updateConnection o2s Cancelled
        let e2o = ConnectionUpdated o2s' (Just $ ucStatus o2s) Nothing
        lift $ Intra.onConnectionEvent self conn e2o
        change s2o Cancelled

    change c s = lift $ Just <$> Data.updateConnection c s

    connection a b = lift (Data.lookupConnection a b) >>= tryJust (NotConnected a b)

createInvitation :: UserId -> InvitationRequest -> ConnId -> ExceptT ConnectionError AppIO (Either ConnectionResult Invitation)
createInvitation self InvitationRequest{..} conn = do
    selfName <- lift (Data.lookupUser self) >>= \case
        Just profile | isJust $ userIdentity profile -> return $ userName profile
                     | otherwise                     -> throwE ConnectNoIdentity
        Nothing -> throwE $ InvalidUser self
    email <- maybe (throwE $ ConnectInvalidEmail irEmail) return (validateEmail irEmail)
    let uk = userEmailKey email
    blacklisted <- lift $ Blacklist.exists uk
    when blacklisted $
        throwE (ConnectBlacklistedUserKey uk)
    user <- lift $ Data.lookupKey uk
    case user of
        Just uid -> Left  <$> createConnection self (ConnectionRequest uid (fromName irName) irMessage) conn
        Nothing  -> Right <$> doInvite email selfName
  where
    doInvite email nm = lift $ do
        (newInv, code) <- Data.insertInvitation self email irName
        void $ sendInvitationMail email irName irMessage nm code irLocale
        return newInv

autoConnect :: UserId
            -> Set UserId
            -> Maybe ConnId
            -> ExceptT ConnectionError AppIO [UserConnection]
autoConnect from (Set.toList -> to) conn = do
    selfActive <- lift $ Data.isActivated from
    -- FIXME: checkLimit from
    -- Checking the limit here is currently a too heavy operation
    -- for this code path and needs to be optimised / rethought.
    unless selfActive $
        throwE ConnectNoIdentity
    othersActive   <- lift $ Data.filterActive to
    nonTeamMembers <- filterOutTeamMembers othersActive
    lift $ connectAll nonTeamMembers
  where
    filterOutTeamMembers us = do
        mems <- lift $ Intra.getTeamContacts from
        return $ maybe us (Team.notTeamMember us . view Team.teamMembers) mems

    connectAll activeOthers = do
        others <- selectOthers activeOthers
        convs  <- mapM (createConv from) others
        self   <- Data.lookupName from
        ucs    <- Data.connectUsers from convs
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

lookupInvitations :: UserId -> Maybe InvitationId -> Range 1 500 Int32 -> AppIO InvitationList
lookupInvitations from start size = do
    rs <- Data.lookupInvitations from start size
    return $! InvitationList (Data.resultList rs) (Data.resultHasMore rs)

deleteInvitation :: UserId -> InvitationId -> AppIO ()
deleteInvitation = Data.deleteInvitation

onboarding :: UserId -> AddressBook -> ExceptT ConnectionError AppIO MatchingResult
onboarding uid ab = do
    -- The choice of 25 is arbitrary and is here only to avoid having a user
    -- auto-connect to too many users; thus the upper limit
    ms    <- lift $ collectMatches 25 [] (chunksOf 25 (abCards ab))
    autos <- autoConnect uid (fromList $ map fst ms) Nothing
    let connected = map ucTo $ filter ((== uid) . ucFrom) autos
    return $ MatchingResult (toMatches connected ms) connected
  where
    collectMatches :: Int -> [(UserId, Maybe CardId)] -> [[Card]] -> AppIO [(UserId, Maybe CardId)]
    collectMatches 0 acc _     = return acc
    collectMatches _ acc []    = return acc
    collectMatches n acc cards = do
        -- Make 4 parallel requests, each will have at most 25 keys to look up
        let (cur, rest) = splitAt 4 cards
        e  <- ask
        ms <- take n <$> filter ((/= uid) . fst) . join
                     <$> liftIO (mapConcurrently (runAppT e . lookupHashes) cur)
        collectMatches (n - length ms) (acc ++ ms) rest

    lookupHashes :: [Card] -> AppIO [(UserId, Maybe CardId)]
    lookupHashes xs = concatMap findCards <$>
        Data.lookupPhoneHashes (map abEntrySha256 (concatMap cEntries xs))
      where
        findCards :: (ByteString, UserId) -> [(UserId, Maybe CardId)]
        findCards (h, u) = map ((u, ) . cCardId)
                         $ filter ((h `elem`) . (map abEntrySha256 . cEntries)) xs

    toMatches :: [UserId] -> [(UserId, Maybe CardId)] -> [Match]
    toMatches uids = map (\(u, c) -> Match u c (maybeToList c))
                   . filter ((`elem` uids) . fst)

-- Helpers

checkLimit :: UserId -> ExceptT ConnectionError AppIO ()
checkLimit u = do
    n <- lift $ Data.countConnections u [Accepted, Sent]
    l <- setUserMaxConnections <$> view settings
    unless (n < l) $
        throwE $ TooManyConnections u
