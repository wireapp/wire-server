{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Galley.Data
    ( ResultSet (..)
    , schemaVersion

    -- * Teams
    , addTeamMember
    , updateTeamMember
    , createTeam
    , removeTeamMember
    , team
    , Galley.Data.teamName
    , teamConversation
    , teamConversations
    , teamIdsFrom
    , teamIdsOf
    , teamMember
    , teamMembers
    , userTeams
    , oneUserTeam
    , Galley.Data.teamBinding
    , teamCreationTime
    , deleteTeam
    , removeTeamConv
    , updateTeam
    , updateTeamStatus

    -- * Conversations
    , Conversation (..)
    , acceptConnect
    , conversation
    , conversationIdsFrom
    , conversationIdsOf
    , conversationMeta
    , conversations
    , createConnectConversation
    , createConversation
    , createOne2OneConversation
    , createSelfConversation
    , isConvAlive
    , updateConversation
    , deleteConversation

    -- * Conversation Members
    , addMember
    , addMembers
    , member
    , members
    , removeMember
    , removeMembers
    , updateMember

    -- * Clients
    , eraseClients
    , lookupClients
    , updateClient

    -- * Utilities
    , one2OneConvId
    , newMember
    ) where

import Cassandra
import Cassandra.Util
import Control.Applicative
import Control.Arrow (second)
import Control.Concurrent.Async.Lifted.Safe
import Control.Lens hiding ((<|))
import Control.Monad (join, forM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.ByteString.Conversion hiding (parser)
import Data.Foldable (toList, foldrM, for_)
import Data.Id
import Data.Range
import Data.List.Split (chunksOf)
import Data.List1 (List1, list1, singleton)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID.V4 (nextRandom)
import Galley.App
import Galley.Data.Types
import Galley.Data.Instances ()
import Galley.Types hiding (Conversation)
import Galley.Types.Bot (newServiceRef)
import Galley.Types.Clients (Clients)
import Galley.Types.Teams hiding (teamMembers, teamConversations, Event, EventType (..))
import Galley.Types.Teams.Intra
import Prelude hiding (max)
import System.Logger.Class (MonadLogger)
import System.Logger.Message (msg, (+++), val)

import qualified Data.Map.Strict      as Map
import qualified Data.UUID.Tagged     as U
import qualified Galley.Data.Queries  as Cql
import qualified Galley.Types.Clients as Clients
import qualified System.Logger.Class  as Log

-- We use this newtype to highlight the fact that the 'Page' wrapped in here
-- can not reliably used for paging.
--
-- The reason for this is that Cassandra returns 'hasMore' as true if the
-- page size requested is equal to result size. To work around this we
-- actually request for one additional element and drop the last value if
-- necessary. This means however that 'nextPage' does not work properly as
-- we would miss a value on every page size.
newtype ResultSet a = ResultSet { page :: Page a }

schemaVersion :: Int32
schemaVersion = 24

-- Teams --------------------------------------------------------------------

team :: MonadClient m => TeamId -> m (Maybe TeamData)
team tid =
    fmap toTeam <$> retry x1 (query1 Cql.selectTeam (params Quorum (Identity tid)))
  where
    toTeam (u, n, i, k, d, s, st, b) =
        let t       = newTeam tid u n i (fromMaybe NonBinding b) & teamIconKey .~ k
            status  = if d then PendingDelete else fromMaybe Active s
        in TeamData t status (writeTimeToUTC <$> st)

teamName :: MonadClient m => TeamId -> m (Maybe Text)
teamName tid = fmap runIdentity <$>
    retry x1 (query1 Cql.selectTeamName (params Quorum (Identity tid)))

teamIdsOf :: MonadClient m => UserId -> Range 1 32 (List TeamId) -> m [TeamId]
teamIdsOf usr (fromList . fromRange -> tids) =
    map runIdentity <$> retry x1 (query Cql.selectUserTeamsIn (params Quorum (usr, tids)))

teamIdsFrom :: MonadClient m => UserId -> Maybe TeamId -> Range 1 100 Int32 -> m (ResultSet TeamId)
teamIdsFrom usr range (fromRange -> max) =
    ResultSet . fmap runIdentity . strip <$> case range of
        Just c  -> paginate Cql.selectUserTeamsFrom (paramsP Quorum (usr, c) (max + 1))
        Nothing -> paginate Cql.selectUserTeams (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p { result = take (fromIntegral max) (result p) }

teamConversation :: MonadClient m => TeamId -> ConvId -> m (Maybe TeamConversation)
teamConversation t c = fmap (newTeamConversation c . runIdentity) <$>
    retry x1 (query1 Cql.selectTeamConv (params Quorum (t, c)))

teamConversations :: MonadClient m => TeamId -> m [TeamConversation]
teamConversations t = map (uncurry newTeamConversation) <$>
    retry x1 (query Cql.selectTeamConvs (params Quorum (Identity t)))

teamMembers :: MonadClient m => TeamId -> m [TeamMember]
teamMembers t = map (uncurry newTeamMember) <$>
    retry x1 (query Cql.selectTeamMembers (params Quorum (Identity t)))

teamMember :: MonadClient m => TeamId -> UserId -> m (Maybe TeamMember)
teamMember t u = fmap (newTeamMember u . runIdentity) <$>
    retry x1 (query1 Cql.selectTeamMember (params Quorum (t, u)))

userTeams :: MonadClient m => UserId -> m [TeamId]
userTeams u = map runIdentity <$>
    retry x1 (query Cql.selectUserTeams (params Quorum (Identity u)))

oneUserTeam :: MonadClient m => UserId -> m (Maybe TeamId)
oneUserTeam u = fmap runIdentity <$>
    retry x1 (query1 Cql.selectOneUserTeam (params Quorum (Identity u)))

teamCreationTime :: MonadClient m => TeamId -> m (Maybe TeamCreationTime)
teamCreationTime t = checkCreation . fmap runIdentity <$>
    retry x1 (query1 Cql.selectTeamBindingWritetime (params Quorum (Identity t)))
  where
    checkCreation (Just (Just ts)) = Just $ TeamCreationTime ts
    checkCreation _                = Nothing

teamBinding :: MonadClient m => TeamId -> m (Maybe TeamBinding)
teamBinding t = checkBinding . fmap runIdentity <$>
    retry x1 (query1 Cql.selectTeamBinding (params Quorum (Identity t)))
  where
    checkBinding :: Maybe (Maybe TeamBinding) -> Maybe TeamBinding
    checkBinding (Just (Just Binding)) = Just Binding
    checkBinding (Just _             ) = Just NonBinding
    checkBinding Nothing               = Nothing

createTeam :: MonadClient m
           => Maybe TeamId
           -> UserId
           -> Range 1 256 Text
           -> Range 1 256 Text
           -> Maybe (Range 1 256 Text)
           -> TeamBinding
           -> m Team
createTeam t uid (fromRange -> n) (fromRange -> i) k b = do
    tid <- maybe (Id <$> liftIO nextRandom) return t
    retry x5 $ write Cql.insertTeam (params Quorum (tid, uid, n, i, fromRange <$> k, initialStatus b, b))
    pure (newTeam tid uid n i b & teamIconKey .~ (fromRange <$> k))
  where
    initialStatus Binding    = PendingActive -- Team becomes Active after User account activation
    initialStatus NonBinding = Active

deleteTeam :: MonadClient m => TeamId -> m ()
deleteTeam tid = do
    retry x5 $ write Cql.markTeamDeleted (params Quorum (PendingDelete, tid))
    mm <- teamMembers tid
    for_ mm $ removeTeamMember tid . view userId
    cc <- teamConversations tid
    for_ cc $ removeTeamConv tid . view conversationId
    retry x5 $ write Cql.deleteTeam (params Quorum (Deleted, tid))

addTeamMember :: MonadClient m => TeamId -> TeamMember -> m ()
addTeamMember t m =
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        addPrepQuery Cql.insertTeamMember (t, m^.userId, m^.permissions)
        addPrepQuery Cql.insertUserTeam   (m^.userId, t)

updateTeamMember :: MonadClient m => TeamId -> UserId -> Permissions -> m ()
updateTeamMember t u p = retry x5 $ write Cql.updatePermissions (params Quorum (p, t, u))

removeTeamMember :: MonadClient m => TeamId -> UserId -> m ()
removeTeamMember t m =
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        addPrepQuery Cql.deleteTeamMember (t, m)
        addPrepQuery Cql.deleteUserTeam   (m, t)

removeTeamConv :: MonadClient m => TeamId -> ConvId -> m ()
removeTeamConv tid cid = do
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        addPrepQuery Cql.markConvDeleted (Identity cid)
        addPrepQuery Cql.deleteTeamConv (tid, cid)
    deleteConversation cid

updateTeamStatus :: MonadClient m => TeamId -> TeamStatus -> m ()
updateTeamStatus t s = retry x5 $ write Cql.updateTeamStatus (params Quorum (s, t))

updateTeam :: MonadClient m => TeamId -> TeamUpdateData -> m ()
updateTeam tid u = retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ (u^.nameUpdate) $ \n ->
        addPrepQuery Cql.updateTeamName (fromRange n, tid)
    for_ (u^.iconUpdate) $ \i ->
        addPrepQuery Cql.updateTeamIcon (fromRange i, tid)
    for_ (u^.iconKeyUpdate) $ \k ->
        addPrepQuery Cql.updateTeamIconKey (fromRange k, tid)

-- Conversations ------------------------------------------------------------

isConvAlive :: MonadClient m => ConvId -> m Bool
isConvAlive cid = do
    result <- retry x1 (query1 Cql.isConvDeleted (params Quorum (Identity cid)))
    case runIdentity <$> result of
        Nothing           -> pure False
        Just Nothing      -> pure True
        Just (Just True)  -> pure False
        Just (Just False) -> pure True

conversation :: (MonadBaseControl IO m, MonadClient m, Forall (Pure m))
             => ConvId
             -> m (Maybe Conversation)
conversation conv = do
    cdata <- async $ retry x1 (query1 Cql.selectConv (params Quorum (Identity conv)))
    toConv conv <$> members conv <*> wait cdata

conversations :: [ConvId] -> Galley [Conversation]
conversations []  = return []
conversations ids = do
    convs  <- async fetchConvs
    mems   <- memberLists ids
    cs     <- zipWith3 toConv ids mems <$> wait convs
    foldrM flatten [] (zip ids cs)
  where
    fetchConvs = do
        cs <- retry x1 $ query Cql.selectConvs (params Quorum (Identity ids))
        let m = Map.fromList $ map (\(c,t,u,n,a,i,d) -> (c, (t,u,n,a,i,d))) cs
        return $ map (`Map.lookup` m) ids

    flatten (i, c) cc = case c of
        Nothing -> do
            Log.warn $ msg (val "No conversation for: " +++ toByteString i)
            return cc
        Just c' -> return (c':cc)

toConv :: ConvId
       -> [Member]
       -> Maybe (ConvType, UserId, Maybe (Set Access), Maybe Text, Maybe TeamId, Maybe Bool)
       -> Maybe Conversation
toConv cid mms conv =
    f mms <$> conv
  where
    f ms (cty, uid, acc, nme, ti, del) = Conversation cid cty uid nme (defAccess cty acc) ms ti del

conversationMeta :: MonadClient m => ConvId -> m (Maybe ConversationMeta)
conversationMeta conv = fmap toConvMeta <$>
    retry x1 (query1 Cql.selectConv (params Quorum (Identity conv)))
  where
    toConvMeta (t, c, a, n, i, _) = ConversationMeta conv t c (defAccess t a) n i

conversationIdsFrom :: MonadClient m => UserId -> Maybe ConvId -> Range 1 1000 Int32 -> m (ResultSet ConvId)
conversationIdsFrom usr range (fromRange -> max) =
    ResultSet . fmap runIdentity . strip <$> case range of
        Just c  -> paginate Cql.selectUserConvsFrom (paramsP Quorum (usr, c) (max + 1))
        Nothing -> paginate Cql.selectUserConvs (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p { result = take (fromIntegral max) (result p) }

conversationIdsOf :: MonadClient m => UserId -> Range 1 32 (List ConvId) -> m [ConvId]
conversationIdsOf usr (fromList . fromRange -> cids) =
    map runIdentity <$> retry x1 (query Cql.selectUserConvsIn (params Quorum (usr, cids)))

createConversation :: UserId
                   -> Maybe (Range 1 256 Text)
                   -> List1 Access
                   -> Range 0 127 [UserId]
                   -> Maybe ConvTeamInfo
                   -> Galley Conversation
createConversation usr name acc others tinfo = do
    conv <- Id <$> liftIO nextRandom
    now  <- liftIO getCurrentTime
    retry x5 $ case tinfo of
        Nothing -> write Cql.insertConv (params Quorum (conv, RegularConv, usr, Set (toList acc), fromRange <$> name, Nothing))
        Just ti -> batch $ do
            setType BatchLogged
            setConsistency Quorum
            addPrepQuery Cql.insertConv (conv, RegularConv, usr, Set (toList acc), fromRange <$> name, Just (cnvTeamId ti))
            addPrepQuery Cql.insertTeamConv (cnvTeamId ti, conv, cnvManaged ti)
    mems <- snd <$> addMembers now conv usr (rcast $ rsingleton usr `rappend` others)
    return $ newConv conv RegularConv usr (toList mems) acc name (cnvTeamId <$> tinfo)

createSelfConversation :: UserId -> Maybe (Range 1 256 Text) -> Galley Conversation
createSelfConversation usr name = do
    let conv = selfConv usr
    now <- liftIO getCurrentTime
    retry x5 $
        write Cql.insertConv (params Quorum (conv, SelfConv, usr, privateOnly, fromRange <$> name, Nothing))
    mems <- snd <$> addMembers now conv usr (rcast $ rsingleton usr)
    return $ newConv conv SelfConv usr (toList mems) (singleton PrivateAccess) name Nothing

createConnectConversation :: U.UUID U.V4
                          -> U.UUID U.V4
                          -> Maybe (Range 1 256 Text)
                          -> Connect
                          -> Galley (Conversation, Event)
createConnectConversation a b name conn = do
    let conv = one2OneConvId a b
        a'   = Id . U.unpack $ a
    now <- liftIO getCurrentTime
    retry x5 $
        write Cql.insertConv (params Quorum (conv, ConnectConv, a', privateOnly, fromRange <$> name, Nothing))
    -- We add only one member, second one gets added later,
    -- when the other user accepts the connection request.
    mems <- snd <$> addMembers now conv a' (rcast $ rsingleton a')
    let e = Event ConvConnect conv a' now (Just $ EdConnect conn)
    return (newConv conv ConnectConv a' (toList mems) (singleton PrivateAccess) name Nothing, e)

createOne2OneConversation :: U.UUID U.V4
                          -> U.UUID U.V4
                          -> Maybe (Range 1 256 Text)
                          -> Maybe TeamId
                          -> Galley Conversation
createOne2OneConversation a b name ti = do
    let conv = one2OneConvId a b
        a'   = Id (U.unpack a)
        b'   = Id (U.unpack b)
    now <- liftIO getCurrentTime
    retry x5 $ case ti of
        Nothing  -> write Cql.insertConv (params Quorum (conv, One2OneConv, a', privateOnly, fromRange <$> name, Nothing))
        Just tid -> batch $ do
            setType BatchLogged
            setConsistency Quorum
            addPrepQuery Cql.insertConv (conv, One2OneConv, a', privateOnly, fromRange <$> name, Just tid)
            addPrepQuery Cql.insertTeamConv (tid, conv, False)
    mems <- snd <$> addMembers now conv a' (rcast $ a' <| rsingleton b')
    return $ newConv conv One2OneConv a' (toList mems) (singleton PrivateAccess) name ti

updateConversation :: MonadClient m => ConvId -> Range 1 256 Text -> m ()
updateConversation cid name = retry x5 $ write Cql.updateConvName (params Quorum (fromRange name, cid))

deleteConversation :: MonadClient m => ConvId -> m ()
deleteConversation cid = do
    retry x5 $ write Cql.markConvDeleted (params Quorum (Identity cid))
    mm <- members cid
    for_ mm $ \m -> removeMember (memId m) cid
    retry x5 $ write Cql.deleteConv (params Quorum (Identity cid))

acceptConnect :: MonadClient m => ConvId -> m ()
acceptConnect cid = retry x5 $ write Cql.updateConvType (params Quorum (One2OneConv, cid))

one2OneConvId :: U.UUID U.V4 -> U.UUID U.V4 -> ConvId
one2OneConvId a b = Id . U.unpack $ U.addv4 a b

newConv :: ConvId
        -> ConvType
        -> UserId
        -> [Member]
        -> List1 Access
        -> Maybe (Range 1 256 Text)
        -> Maybe TeamId
        -> Conversation
newConv cid ct usr mems acc name tid = Conversation
    { convId      = cid
    , convType    = ct
    , convCreator = usr
    , convName    = fromRange <$> name
    , convAccess  = acc
    , convMembers = mems
    , convTeam    = tid
    , convDeleted = Nothing
    }

defAccess :: ConvType -> Maybe (Set Access) -> List1 Access
defAccess SelfConv    Nothing             = singleton PrivateAccess
defAccess ConnectConv Nothing             = singleton PrivateAccess
defAccess One2OneConv Nothing             = singleton PrivateAccess
defAccess RegularConv Nothing             = singleton InviteAccess
defAccess SelfConv    (Just (Set []))     = singleton PrivateAccess
defAccess ConnectConv (Just (Set []))     = singleton PrivateAccess
defAccess One2OneConv (Just (Set []))     = singleton PrivateAccess
defAccess RegularConv (Just (Set []))     = singleton InviteAccess
defAccess _           (Just (Set (x:xs))) = list1 x xs

privateOnly :: Set Access
privateOnly = Set [PrivateAccess]

-- Conversation Members -----------------------------------------------------

member :: MonadClient m => ConvId -> UserId -> m (Maybe Member)
member cnv usr = (toMember =<<) <$> retry x1 (query1 Cql.selectMember (params Quorum (cnv, usr)))

memberLists :: MonadClient m => [ConvId] -> m [[Member]]
memberLists convs = do
    mems <- retry x1 $ query Cql.selectMembers (params Quorum (Identity convs))
    let m = foldr (insert . mkMem) Map.empty mems
    return $ map (\i -> fromMaybe [] (Map.lookup i m)) convs
  where
    insert Nothing            acc = acc
    insert (Just (conv, mem)) acc =
        let f = (Just . maybe [mem] (mem :))
        in Map.alter f conv acc

    mkMem (cnv, usr, srv, prv, st, omu, omur, oar, oarr, hid, hidr) =
        (cnv, ) <$> toMember (usr, srv, prv, st, omu, omur, oar, oarr, hid, hidr)

members :: MonadClient m => ConvId -> m [Member]
members conv = join <$> memberLists [conv]

addMember :: ConvId -> UserId -> Galley ()
addMember c u = do
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        addPrepQuery Cql.insertUserConv (u, c)
        addPrepQuery Cql.insertMember   (c, u, Nothing, Nothing)
    pure ()

addMembers :: UTCTime -> ConvId -> UserId -> Range 1 128 [UserId] -> Galley (Event, List1 Member)
addMembers t conv orig usrs = do
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        for_ (fromRange usrs) $ \u -> do
            addPrepQuery Cql.insertUserConv (u, conv)
            addPrepQuery Cql.insertMember   (conv, u, Nothing, Nothing)
    let e = Event MemberJoin conv orig t (Just . EdMembers . Members . fromRange $ usrs)
    let (u:us) = fromRange usrs
    return (e, newMember <$> list1 u us)

updateMember :: (MonadLogger m, MonadClient m) => ConvId -> UserId -> MemberUpdate -> m MemberUpdateData
updateMember cid uid mup = do
    retry x5 $ batch $ do
        setType BatchUnLogged
        setConsistency Quorum
        for_ (mupOtrMute mup) $ \m ->
            addPrepQuery Cql.updateOtrMemberMuted (m, mupOtrMuteRef mup, cid, uid)
        for_ (mupOtrArchive mup) $ \a ->
            addPrepQuery Cql.updateOtrMemberArchived (a, mupOtrArchiveRef mup, cid, uid)
        for_ (mupHidden mup) $ \h ->
            addPrepQuery Cql.updateMemberHidden (h, mupHiddenRef mup, cid, uid)
    return MemberUpdateData
        { misOtrMuted = mupOtrMute mup
        , misOtrMutedRef = mupOtrMuteRef mup
        , misOtrArchived = mupOtrArchive mup
        , misOtrArchivedRef = mupOtrArchiveRef mup
        , misHidden = mupHidden mup
        , misHiddenRef = mupHiddenRef mup
        }

removeMembers :: Conversation -> UserId -> List1 UserId -> Galley Event
removeMembers conv orig victims = do
    t <- liftIO getCurrentTime
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        for_ (toList victims) $ \u -> do
            addPrepQuery Cql.removeMember   (convId conv, u)
            addPrepQuery Cql.deleteUserConv (u, convId conv)
    return $ Event MemberLeave (convId conv) orig t (Just . EdMembers . Members . toList $ victims)

removeMember :: MonadClient m => UserId -> ConvId -> m ()
removeMember usr cnv = retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.removeMember (cnv, usr)
    addPrepQuery Cql.deleteUserConv (usr, cnv)

newMember :: UserId -> Member
newMember u = Member
    { memId             = u
    , memService        = Nothing
    , memOtrMuted       = False
    , memOtrMutedRef    = Nothing
    , memOtrArchived    = False
    , memOtrArchivedRef = Nothing
    , memHidden         = False
    , memHiddenRef      = Nothing
    }

toMember :: ( UserId, Maybe ServiceId, Maybe ProviderId, Maybe Cql.MemberStatus
            , Maybe Bool, Maybe Text -- otr muted
            , Maybe Bool, Maybe Text -- otr archived
            , Maybe Bool, Maybe Text -- hidden
            ) -> Maybe Member
toMember (usr, srv, prv, sta, omu, omur, oar, oarr, hid, hidr) =
    if sta /= Just 0
        then Nothing
        else Just $ Member
            { memId             = usr
            , memService        = newServiceRef <$> srv <*> prv
            , memOtrMuted       = fromMaybe False omu
            , memOtrMutedRef    = omur
            , memOtrArchived    = fromMaybe False oar
            , memOtrArchivedRef = oarr
            , memHidden         = fromMaybe False hid
            , memHiddenRef      = hidr
            }

-- Clients ------------------------------------------------------------------

updateClient :: MonadClient m => Bool -> UserId -> ClientId -> m ()
updateClient add usr cls = do
    let q = if add then Cql.addMemberClient else Cql.rmMemberClient
    retry x5 $ write (q cls) (params Quorum (Identity usr))

-- Do, at most, 16 parallel lookups of up to 128 users each
lookupClients :: (MonadClient m, MonadBaseControl IO m, Forall (Pure m)) 
              => [UserId] -> m Clients
lookupClients users = Clients.fromList . concat . concat <$>
    forM (chunksOf 2048 users) (mapConcurrently getClients . chunksOf 128)
  where
    getClients us = map (second fromSet) <$>
        retry x1 (query Cql.selectClients (params Quorum (Identity us)))

eraseClients :: MonadClient m => UserId -> m ()
eraseClients user = retry x5 (write Cql.rmClients (params Quorum (Identity user)))
