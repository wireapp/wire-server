{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Galley.Data
    ( Conversation (..)
    , ResultSet    (..)
    , schemaVersion

      -- * Write
    , createConversation
    , createSelfConversation
    , createOne2OneConversation
    , createConnectConversation
    , updateConversation
    , updateMember
    , addMembers
    , rmMembers
    , deleteMember
    , acceptConnect
    , updateClient
    , eraseClients

      -- * Read
    , conversation
    , conversationMeta
    , conversations
    , conversationIdsFrom
    , conversationIdsOf
    , member
    , members
    , lookupClients

      -- * Utilities
    , one2OneConvId
    , newMember
    ) where

import Cassandra
import Control.Applicative
import Control.Arrow (second)
import Control.Concurrent.Async.Lifted.Safe
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.ByteString.Conversion hiding (parser)
import Data.Foldable (toList, foldrM, for_)
import Data.Functor.Identity
import Data.Id
import Data.Range
import Data.List1 (List1, list1, singleton)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID.V4 (nextRandom)
import Galley.App
import Galley.Data.Queries as Cql
import Galley.Data.Types
import Galley.Data.Instances ()
import Galley.Types hiding (Conversation)
import Galley.Types.Bot (newServiceRef)
import Galley.Types.Clients (Clients)
import Prelude hiding (max)
import System.Logger.Class (MonadLogger)
import System.Logger.Message (msg, (+++), val)

import qualified Data.Map.Strict      as Map
import qualified Data.UUID.Tagged     as U
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
schemaVersion = 20

-- | Get conversation by ID.
conversation :: (MonadBaseControl IO m, MonadClient m, Forall (Pure m))
             => ConvId
             -> m (Maybe Conversation)
conversation conv = do
    cdata <- async $ retry x1 (query1 selectConv (params Quorum (Identity conv)))
    toConv conv <$> members conv <*> wait cdata

-- | Return conversations corresponding to the given IDs.
conversations :: [ConvId] -> Galley [Conversation]
conversations []  = return []
conversations ids = do
    convs  <- async fetchConvs
    mems   <- memberLists ids
    cs     <- zipWith3 toConv ids mems <$> wait convs
    foldrM flatten [] (zip ids cs)
  where
    fetchConvs = do
        cs <- retry x1 $ query selectConvs (params Quorum (Identity ids))
        let m = Map.fromList $ map (\(c,t,u,n,a) -> (c, (t,u,n,a))) cs
        return $ map (`Map.lookup` m) ids

    flatten (i, c) cc = case c of
        Nothing -> do
            Log.warn $ msg (val "No conversation for: " +++ toByteString i)
            return cc
        Just c' -> return (c':cc)

-- | Get conversation metadata by ID.
conversationMeta :: MonadClient m => ConvId -> m (Maybe ConversationMeta)
conversationMeta conv = fmap toConvMeta <$>
    retry x1 (query1 selectConv (params Quorum (Identity conv)))
  where
    toConvMeta (t, c, a, n) = ConversationMeta conv t c (defAccess t a) n

-- | Get a set of conversation IDs (optionally starting from the given ID).
conversationIdsFrom :: MonadClient m => UserId -> Maybe ConvId -> Range 1 1000 Int32 -> m (ResultSet ConvId)
conversationIdsFrom usr range (fromRange -> max) =
    ResultSet . fmap runIdentity . strip <$> case range of
        Just c  -> paginate selectUserConvsFrom (paramsP Quorum (usr, c) (max + 1))
        Nothing -> paginate selectUserConvs (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p { result = take (fromIntegral max) (result p) }

-- | Given a set of conversation IDs, return the ones that the given user
-- is associated to.
conversationIdsOf :: MonadClient m => UserId -> Range 1 32 (List ConvId) -> m [ConvId]
conversationIdsOf usr (fromList . fromRange -> cids) =
    map runIdentity <$> retry x1 (query selectUserConvsIn (params Quorum (usr, cids)))

member :: MonadClient m => ConvId -> UserId -> m (Maybe Member)
member cnv usr = (toMember =<<) <$> retry x1 (query1 selectMember (params Quorum (cnv, usr)))

memberLists :: MonadClient m => [ConvId] -> m [[Member]]
memberLists convs = do
    mems <- retry x1 $ query selectMembers (params Quorum (Identity convs))
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

createConversation :: UserId -> Maybe (Range 1 256 Text) -> List1 Access -> Range 0 64 [UserId] -> Galley Conversation
createConversation usr name acc others = do
    conv <- Id <$> liftIO nextRandom
    now  <- liftIO getCurrentTime
    retry x5 $
        write insertConv (params Quorum (conv, RegularConv, usr, Set (toList acc), fromRange <$> name))
    mems <- snd <$> addMembers now conv usr (rcast $ rsingleton usr `rappend` others)
    return $ newConv conv RegularConv usr mems acc name

createSelfConversation :: UserId -> Maybe (Range 1 256 Text) -> Galley Conversation
createSelfConversation usr name = do
    let conv = Id (toUUID usr)
    now <- liftIO getCurrentTime
    retry x5 $
        write insertConv (params Quorum (conv, SelfConv, usr, privateOnly,  fromRange <$> name))
    mems <- snd <$> addMembers now conv usr (rcast $ rsingleton usr)
    return $ newConv conv SelfConv usr mems (singleton PrivateAccess) name

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
        write insertConv (params Quorum (conv, ConnectConv, a', privateOnly, fromRange <$> name))
    -- We add only one member, second one gets added later,
    -- when the other user accepts the connection request.
    mems <- snd <$> addMembers now conv a' (rcast $ rsingleton a')
    let e = Event ConvConnect conv a' now (Just $ EdConnect conn)
    return (newConv conv ConnectConv a' mems (singleton PrivateAccess) name, e)

createOne2OneConversation :: U.UUID U.V4
                          -> U.UUID U.V4
                          -> Maybe (Range 1 256 Text)
                          -> Galley Conversation
createOne2OneConversation a b name = do
    let conv = one2OneConvId a b
        a'   = Id (U.unpack a)
        b'   = Id (U.unpack b)
    now <- liftIO getCurrentTime
    retry x5 $
        write insertConv (params Quorum (conv, One2OneConv, a', privateOnly, fromRange <$> name))
    mems <- snd <$> addMembers now conv a' (rcast $ a' <| rsingleton b')
    return $ newConv conv One2OneConv a' mems (singleton PrivateAccess) name

updateConversation :: MonadClient m => ConvId -> Range 1 256 Text -> m ()
updateConversation cid name = retry x5 $ write updateConvName (params Quorum (fromRange name, cid))

addMembers :: UTCTime -> ConvId -> UserId -> Range 1 128 [UserId] -> Galley (Event, List1 Member)
addMembers t conv orig usrs = do
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        for_ (fromRange usrs) $ \u -> do
            addPrepQuery insertUserConv (u, conv)
            addPrepQuery insertMember   (conv, u, Nothing, Nothing)
    let e = Event MemberJoin conv orig t (Just . EdMembers . Members . fromRange $ usrs)
    let (u:us) = fromRange usrs
    return (e, newMember <$> list1 u us)

updateMember :: (MonadLogger m, MonadClient m) => ConvId -> UserId -> MemberUpdate -> m MemberUpdateData
updateMember cid uid mup = do
    retry x5 $ batch $ do
        setType BatchUnLogged
        setConsistency Quorum
        for_ (mupOtrMute mup) $ \m ->
            addPrepQuery updateOtrMemberMuted (m, mupOtrMuteRef mup, cid, uid)
        for_ (mupOtrArchive mup) $ \a ->
            addPrepQuery updateOtrMemberArchived (a, mupOtrArchiveRef mup, cid, uid)
        for_ (mupHidden mup) $ \h ->
            addPrepQuery updateMemberHidden (h, mupHiddenRef mup, cid, uid)
    return MemberUpdateData
        { misOtrMuted = mupOtrMute mup
        , misOtrMutedRef = mupOtrMuteRef mup
        , misOtrArchived = mupOtrArchive mup
        , misOtrArchivedRef = mupOtrArchiveRef mup
        , misHidden = mupHidden mup
        , misHiddenRef = mupHiddenRef mup
        }

updateClient :: MonadClient m => Bool -> UserId -> ClientId -> m ()
updateClient add usr cls = do
    let q = if add then addMemberClient else rmMemberClient
    retry x5 $ write (q cls) (params Quorum (Identity usr))

lookupClients :: MonadClient m => [UserId] -> m Clients
lookupClients usrs = Clients.fromList . map (second fromSet) <$>
    retry x1 (query selectClients (params Quorum (Identity usrs)))

eraseClients :: MonadClient m => UserId -> m ()
eraseClients user = retry x5 (write rmClients (params Quorum (Identity user)))

rmMembers :: Conversation -> UserId -> List1 UserId -> Galley Event
rmMembers conv orig victims = do
    t <- liftIO getCurrentTime
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        for_ (toList victims) $ \u -> do
            addPrepQuery Cql.removeMember   (convId conv, u)
            addPrepQuery Cql.deleteUserConv (u, convId conv)
    return $ Event MemberLeave (convId conv) orig t (Just . EdMembers . Members . toList $ victims)

deleteMember :: MonadClient m => UserId -> ConvId -> m ()
deleteMember usr cnv = retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.removeMember (cnv, usr)
    addPrepQuery Cql.deleteUserConv (usr, cnv)

acceptConnect :: MonadClient m => ConvId -> m ()
acceptConnect cid = retry x5 $ write updateConvType (params Quorum (One2OneConv, cid))

-- Helpers

one2OneConvId :: U.UUID U.V4 -> U.UUID U.V4 -> ConvId
one2OneConvId a b = Id . U.unpack $ U.addv4 a b

newConv :: ConvId -> ConvType -> UserId -> List1 Member -> List1 Access -> Maybe (Range 1 256 Text) -> Conversation
newConv cid ct usr mems acc name = Conversation
    { convId      = cid
    , convType    = ct
    , convCreator = usr
    , convName    = fromRange <$> name
    , convAccess  = acc
    , convMembers = toList mems
    }

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

toConv :: ConvId
       -> [Member]
       -> Maybe (ConvType, UserId, Maybe (Set Access), Maybe Text)
       -> Maybe Conversation
toConv cid mms conv =
    f mms <$> conv
  where
    f ms (cty, uid, acc, nme) = Conversation cid cty uid nme (defAccess cty acc) ms

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

toMember :: ( UserId, Maybe ServiceId, Maybe ProviderId, Cql.MemberStatus
            , Maybe Bool, Maybe Text -- otr muted
            , Maybe Bool, Maybe Text -- otr archived
            , Maybe Bool, Maybe Text -- hidden
            ) -> Maybe Member
toMember (usr, srv, prv, sta, omu, omur, oar, oarr, hid, hidr) =
    if sta /= 0
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

privateOnly :: Set Access
privateOnly = Set [PrivateAccess]

