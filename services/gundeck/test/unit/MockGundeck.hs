{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module MockGundeck where

import Imports
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson
import Data.Id
import Data.List1
import Data.Misc ((<$$>), Milliseconds(Ms))
import Data.Range
import Data.String.Conversions
import Data.UUID ()
import Gundeck.Aws.Arn as Aws
import Gundeck.Options
import Gundeck.Push
import Gundeck.Push.Native as Native
import Gundeck.Push.Websocket as Web
import Gundeck.Types
import Gundeck.Types.BulkPush
import System.Logger.Class as Log hiding (trace)
import System.Random
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Network.URI as URI


----------------------------------------------------------------------
-- helpers (move to some more general-purpose test library?)

-- | (it may be possible to drop this type in favor of more sophisticated use of quickcheck's
-- counterexample.)
newtype Pretty a = Pretty a
  deriving (Eq, Ord)

instance Aeson.ToJSON a => Show (Pretty a) where
  show (Pretty a) = cs $ Aeson.encodePretty a

shrinkPretty :: (a -> [a]) -> Pretty a -> [Pretty a]
shrinkPretty shrnk (Pretty xs) = Pretty <$> shrnk xs


----------------------------------------------------------------------
-- env

type Payload = List1 Aeson.Object

data MockEnv = MockEnv
  { _meRecipients      :: [Recipient]
  , _meNativeAddress   :: Map UserId (Map ClientId (Address "no-keys"))
  , _meWSReachable     :: Set (UserId, ClientId)
  , _meNativeReachable :: Set (Address "no-keys")
  }
  deriving (Eq, Show)

data MockState = MockState
  { _msNonRandomGen    :: StdGen
  , _msWSQueue         :: NotifQueue
  , _msNativeQueue     :: NotifQueue
  }
  deriving (Eq, Show)

type NotifQueue = Map (UserId, ClientId) (Set Int)

instance Eq StdGen where
  (==) = (==) `on` show

makeLenses ''MockEnv
makeLenses ''MockState

whipeRandomGen :: MockState -> MockState
whipeRandomGen = msNonRandomGen .~ (mempty ^. msNonRandomGen)

instance Semigroup MockState where
  (MockState _ ws nat) <> (MockState gen ws' nat') = MockState gen (ws <> ws') (nat <> nat')

instance Monoid MockState where
  mempty = MockState (mkStdGen 0) mempty mempty

-- (serializing test cases makes replay easier.)
instance ToJSON MockEnv where
  toJSON env = object
    [ "meRecipients"      Aeson..= (env ^. meRecipients)
    , "meNativeAddress"   Aeson..= Map.toList (Map.toList <$> (env ^. meNativeAddress))
    , "meWSReachable"     Aeson..= Set.toList (env ^. meWSReachable)
    , "meNativeReachable" Aeson..= Set.toList (env ^. meNativeReachable)
    ]

instance ToJSON (Address s) where
  toJSON adr = Aeson.object
    [ "addrUser"      Aeson..= (adr ^. addrUser)
    , "addrTransport" Aeson..= (adr ^. addrTransport)
    , "addrApp"       Aeson..= (adr ^. addrApp)
    , "addrToken"     Aeson..= (adr ^. addrToken)
    , "addrEndpoint"  Aeson..= (serializeFakeAddrEndpoint $ adr ^. addrEndpoint)
    , "addrConn"      Aeson..= (adr ^. addrConn)
    , "addrClient"    Aeson..= (adr ^. addrClient)
    ]

serializeFakeAddrEndpoint :: EndpointArn -> (Text, Transport, AppName)
serializeFakeAddrEndpoint ((^. snsTopic) -> eptopic) =
  ( case eptopic ^. endpointId of EndpointId txt -> txt
  , eptopic ^. endpointTransport
  , eptopic ^. endpointAppName
  )

instance FromJSON MockEnv where
  parseJSON = withObject "MockEnv" $ \env -> MockEnv
    <$> (env Aeson..: "meRecipients")
    <*> (Map.fromList <$$> (Map.fromList <$> (env Aeson..: "meNativeAddress")))
    <*> (Set.fromList <$> (env Aeson..: "meWSReachable"))
    <*> (Set.fromList <$> (env Aeson..: "meNativeReachable"))

instance FromJSON (Address s) where
  parseJSON = withObject "Address" $ \adr -> Address
    <$> (adr Aeson..: "addrUser")
    <*> (adr Aeson..: "addrTransport")
    <*> (adr Aeson..: "addrApp")
    <*> (adr Aeson..: "addrToken")
    <*> (mkFakeAddrEndpoint <$> adr Aeson..: "addrEndpoint")
    <*> (adr Aeson..: "addrConn")
    <*> (adr Aeson..: "addrClient")

mkFakeAddrEndpoint :: (Text, Transport, AppName) -> EndpointArn
mkFakeAddrEndpoint (epid, transport, app) = Aws.mkSnsArn Tokyo (Account "acc") eptopic
  where eptopic = mkEndpointTopic (ArnEnv "") transport app (EndpointId epid)


-- | Generate an environment probabilistically containing the following situations:
--
-- 1. web socket delivery will work
-- 2. web socket delivery will NOT work, native push token registered, push will succeed
-- 4. web socket delivery will NOT work, native push token registered, push will fail
-- 5. web socket delivery will NOT work, no native push token registered
genMockEnv :: Gen MockEnv
genMockEnv = do
  _meRecipients :: [Recipient]
    <- nubBy ((==) `on` (^. recipientId)) <$> listOf1 genRecipient

  protoaddrs :: [UserId -> ClientId -> Address "no-keys"]
    <- do
      len <- let l = length _meRecipients in choose (l `div` 2, l)
      vectorOf len genProtoAddress

  let addrs :: [(Recipient, Address "no-keys")]
      addrs = mconcat $ zipWith go _meRecipients protoaddrs
        where go rcp@(Recipient uid _ cids) adr = (\cid -> (rcp, adr uid cid)) <$> cids

      _meNativeAddress :: Map UserId (Map ClientId (Address "no-keys"))
      _meNativeAddress = foldl' go mempty (snd <$> addrs)
        where
          go :: Map UserId (Map ClientId (Address "no-keys"))
             -> Address "no-keys"
             -> Map UserId (Map ClientId (Address "no-keys"))
          go m addr = Map.alter (go' addr) (addr ^. addrUser) m

          go' :: Address "no-keys"
              -> Maybe (Map ClientId (Address "no-keys"))
              -> Maybe (Map ClientId (Address "no-keys"))
          go' addr = Just . maybe newEntries (newEntries <>)
            where newEntries = Map.fromList [(addr ^. addrClient, addr)]

  _meWSReachable <- genPredicate . mconcat $ recipientToIds <$> _meRecipients
  _meNativeReachable <- genPredicate (snd <$> addrs)

  let env = MockEnv {..}
  validateMockEnv env & either error (const $ pure env)

shrinkMockEnv :: MockEnv -> [MockEnv]
shrinkMockEnv env = do
  rcps :: [Recipient] <- shrinkList (const []) (env ^. meRecipients)

  let env' :: MockEnv
      env' = env
        & meRecipients .~ rcps
        & meNativeAddress %~ naddrs
        & meWSReachable %~ wsrchbl
        & meNativeReachable %~ ntrchbl

      clients :: Set (UserId, ClientId)
      clients = Set.fromList . mconcat $ (\(Recipient uid _ cids) -> (uid,) <$> cids) <$> rcps

      naddrs  = Map.filterWithKey (\uid _ -> uid `Set.member` (fst `Set.map` clients))
              . fmap (Map.filterWithKey (\cid _ -> cid `Set.member` (snd `Set.map` clients)))
      wsrchbl = Set.filter (`Set.member` clients)
      ntrchbl = Set.filter (`Set.member` flatten (env' ^. meNativeAddress))
        where flatten = Set.fromList . mconcat . Map.elems . fmap Map.elems

  [env' | not $ null rcps]

validateMockEnv :: MockEnv -> Either String ()
validateMockEnv env = do
  forM_ (Map.toList $ env ^. meNativeAddress) $ \(uid, el) -> do
    forM_ (Map.toList el) $ \(cid, adr) -> do
      unless (uid == adr ^. addrUser && cid == adr ^. addrClient) $ do
        throwError (show (uid, cid, adr))

recipientToIds :: Recipient -> [(UserId, ClientId)]
recipientToIds (Recipient uid _ cids) = (uid,) <$> cids

genPredicate :: forall a. (Eq a, Ord a) => [a] -> Gen (Set a)
genPredicate xs = Set.fromList <$> do
  bools :: [Bool] <- vectorOf (length xs) arbitrary
  let reachables :: [a] = mconcat $ zipWith (\x yes -> [ x | yes ]) xs bools
  pure reachables

genRecipient :: HasCallStack => Gen Recipient
genRecipient = do
  uid  <- genId
  cids <- nub <$> listOf1 genClientId
  pure $ Recipient uid RouteAny cids

genId :: Gen (Id a)
genId = do
  gen <- mkStdGen <$> resize 100 arbitrary
  pure . Id . fst $ random gen

genClientId :: Gen ClientId
genClientId = newClientId <$> resize 50 arbitrary

fakePresences :: Recipient -> [Presence]
fakePresences (Recipient uid _ cids) = fakePresence uid <$> cids

fakePresence :: UserId -> ClientId -> Presence
fakePresence userId (Just -> clientId) = Presence {..}
  where
    connId    = fakeConnId $ fromJust clientId
    resource  = URI . fromJust $ URI.parseURI "http://example.com"
    createdAt = 0
    __field   = mempty

-- | TODO: explain why this keeps the distinction between client and connection intact.
fakeConnId :: ClientId -> ConnId
fakeConnId = ConnId . cs . client

clientIdFromConnId :: ConnId -> ClientId
clientIdFromConnId = ClientId . cs . fromConnId

genProtoAddress :: Gen (UserId -> ClientId -> Address "no-keys")
genProtoAddress = do
  _addrTransport <- QC.elements [minBound..]
  arnEpId <- arbitrary
  let _addrApp = "AppName"
      _addrToken = Token "tok"
      _addrEndpoint = mkFakeAddrEndpoint (arnEpId, _addrTransport, _addrApp)
  pure $ \_addrUser _addrClient -> let _addrConn = fakeConnId _addrClient in Address {..}

genPushes :: [Recipient] -> Gen [Push]
genPushes = listOf . genPush

-- | REFACTOR: What 'Route's are we still using, and what for?  This code is currently only testing
-- 'RouteAny'.
genPush :: [Recipient] -> Gen Push
genPush allrcps = do
  sender :: UserId <- (^. recipientId) <$> QC.elements allrcps
  rcps :: Range 1 1024 (Set Recipient) <- do
    numrcp <- choose (1, min 1024 (length allrcps))
    unsafeRange . Set.fromList . nubBy ((==) `on` (^. recipientId)) <$> vectorOf numrcp (QC.elements allrcps)
                  -- TODO: sometimes we only want to send to some clients?  currently we always send to all explicitly
  pload <- genPayload
  inclorigin <- arbitrary
  pure $ newPush sender rcps pload & pushNativeIncludeOrigin .~ inclorigin

shrinkPushes :: [Push] -> [[Push]]
shrinkPushes = shrinkList shrinkPush
  where
    shrinkPush :: Push -> [Push]
    shrinkPush psh = (\rcps -> psh & pushRecipients .~ rcps) <$> shrinkRecipients (psh ^. pushRecipients)

    shrinkRecipients :: Range 1 1024 (Set Recipient) -> [Range 1 1024 (Set Recipient)]
    shrinkRecipients = fmap unsafeRange . map Set.fromList . filter (not . null) . shrinkList shrinkRecipient . Set.toList . fromRange

    shrinkRecipient :: Recipient -> [Recipient]
    shrinkRecipient _ = []

genPayload :: Gen Payload
genPayload = do
  num :: Int <- arbitrary
  pure $ List1 (HashMap.singleton "val" (Aeson.toJSON num) NE.:| [])

instance Arbitrary Aeson.Value where
  -- (not currently in use; 'genPayload' is built to be more compact.)
  arbitrary = oneof
    [ Aeson.object <$> listOf ((Aeson..=) <$> arbitrary <*> (scale (`div` 3) (arbitrary @Aeson.Value)))
    , Aeson.Array . Vector.fromList <$> listOf (scale (`div` 3) (arbitrary @Aeson.Value))
    , Aeson.String <$> arbitrary
    , Aeson.Number <$> arbitrary
    , Aeson.Bool <$> QC.elements [minBound..]
    , pure Aeson.Null
    ]

genNotif :: Gen Notification
genNotif = Notification <$> genId <*> arbitrary <*> genPayload

genNotifs :: [Recipient] -> Gen [(Notification, [Presence])]
genNotifs (mconcat . fmap fakePresences -> allprcs) = fmap uniqNotifs . listOf $ do
  notif <- genNotif
  prcs <- nub <$> listOf (QC.elements allprcs)
  pure (notif, prcs)
  where
    uniqNotifs = nubBy ((==) `on` (ntfId . fst))

shrinkNotifs :: [(Notification, [Presence])] -> [[(Notification, [Presence])]]
shrinkNotifs = shrinkList (\(notif, prcs) -> (notif,) <$> shrinkList (const []) prcs)


----------------------------------------------------------------------
-- monad type and instances

newtype MockGundeck a = MockGundeck
  { fromMockGundeck :: ReaderT MockEnv (StateT MockState Identity) a }
  deriving (Functor, Applicative, Monad, MonadReader MockEnv, MonadState MockState)

runMockGundeck :: MockEnv -> MockGundeck a -> (a, MockState)
runMockGundeck env (MockGundeck m) = runIdentity $ runStateT (runReaderT m env) mempty

instance MonadThrow MockGundeck where
  throwM = error . show  -- (we are not expecting any interesting errors in these tests, so we might
                         -- as well crash badly here, as long as it doesn't go unnoticed...)

instance MonadPushAll MockGundeck where
  mpaNotificationTTL = pure $ NotificationTTL 300  -- (longer than we want any test to take.)
  mpaMkNotificationId = mockMkNotificationId
  mpaListAllPresences = mockListAllPresences
  mpaBulkPush = mockBulkPush
  mpaStreamAdd = mockStreamAdd
  mpaPushNative = mockPushNative
  mpaForkIO = id  -- just don't fork.  (this *may* cause deadlocks in principle, but as long as it
                  -- doesn't, this is good enough for testing).

instance MonadNativeTarget MockGundeck where
  mntgtLogErr _ = pure ()
  mntgtLookupAddress = mockLookupAddress
  mntgtMapAsync f xs = Right <$$> mapM f xs  -- (no concurrency)

instance MonadBulkPush MockGundeck where
  mbpBulkSend = mockBulkSend
  mbpDeleteAllPresences _ = pure ()  -- TODO: test presence deletion logic
  mbpPosixTime = pure $ Ms 1545045904275  -- (time is constant)
  mbpMapConcurrently = mapM  -- (no concurrency)
  mbpMonitorBadCannons _ = pure ()  -- (no monitoring)

instance Log.MonadLogger MockGundeck where
  log _ _ = pure ()  -- (no logging)


----------------------------------------------------------------------
-- monad implementation

-- | Expected behavior of 'Gundeck.Push.pushAll' (used in the property test).
mockPushAll
  :: (HasCallStack, m ~ MockGundeck)
  => [Push] -> m ()
mockPushAll pushes = do
  env <- ask
  modify $ (msWSQueue .~ expectWS env)
         . (msNativeQueue .~ expectNative env)
  where
    expectWS :: MockEnv -> NotifQueue
    expectWS env
      = foldl' (uncurry . deliver) mempty
      . filter reachable
      . reformat
      . mconcat . fmap removeSelf
      . mconcat . fmap insertAllClients
      $ rcps
      where
        reachable :: ((UserId, ClientId), payload) -> Bool
        reachable (ids, _) = ids `elem` (env ^. meWSReachable)

        removeSelf :: ((UserId, Maybe ClientId, any), (Recipient, Payload)) -> [(Recipient, Payload)]
        removeSelf ((_, Nothing, _), same) =
          [same]
        removeSelf ((_, Just sndcid, _), (Recipient rcpuid route cids, pay)) =
          [(Recipient rcpuid route $ filter (/= sndcid) cids, pay)]

        insertAllClients :: (any, (Recipient, Payload))
                         -> [(any, (Recipient, Payload))]
        -- if the recipient client list is empty, fill in all devices of that user
        insertAllClients (same, (Recipient uid route [], pay)) = [(same, (rcp', pay))]
          where
            rcp' = Recipient uid route defaults
            defaults = mconcat [ cids | Recipient uid' _ cids <- env ^. meRecipients, uid' == uid ]

        -- otherwise, no special hidden meaning.
        insertAllClients same@(_, (Recipient _ _ (_:_), _)) = [same]

    expectNative :: MockEnv -> NotifQueue
    expectNative env
      = foldl' (uncurry . deliver) mempty
      . filter reachable
      . reformat
      . mconcat . fmap removeSelf
      . mconcat . fmap insertAllClients
      $ rcps
      where
        reachable :: ((UserId, ClientId), payload) -> Bool
        reachable (ids, _) = reachableNative ids && not (reachableWS ids)
          where
            reachableWS :: (UserId, ClientId) -> Bool
            reachableWS = (`elem` (env ^. meWSReachable))

            reachableNative :: (UserId, ClientId) -> Bool
            reachableNative (uid, cid) = maybe False (`elem` (env ^. meNativeReachable)) adr
              where adr = (Map.lookup uid >=> Map.lookup cid) (env ^. meNativeAddress)

        removeSelf :: ((UserId, Maybe ClientId, Bool), (Recipient, Payload)) -> [(Recipient, Payload)]
        removeSelf ((snduid, _, False), same@(Recipient rcpuid _ _, _)) =
          -- if pushNativeIncludeOrigin is False, none of the originator's devices will receive
          -- native pushes.
          [same | snduid /= rcpuid]
        removeSelf ((_, Just sndcid, True), (Recipient rcpuid route cids, pay)) =
          -- if originating client is known and pushNativeIncludeOrigin is True, filter out just
          -- that client, and native-push to all other devices of the originator.
          [(Recipient rcpuid route $ filter (/= sndcid) cids, pay)]
        removeSelf ((_, Nothing, True), same) =
          -- if NO originating client is known and pushNativeIncludeOrigin is True, push to all
          -- originating devices.
          [same]

        insertAllClients :: (any, (Recipient, Payload))
                         -> [(any, (Recipient, Payload))]
        -- if the recipient client list is empty, fill in all devices of that user
        insertAllClients (same, (Recipient uid route [], pay)) = [(same, (rcp', pay))]
          where
            rcp' = Recipient uid route defaults
            defaults = maybe [] Map.keys . Map.lookup uid $ env ^. meNativeAddress
        -- otherwise, no special hidden meaning.
        insertAllClients same@(_, (Recipient _ _ (_:_), _)) = [same]

    reformat :: [(Recipient, Payload)] -> [((UserId, ClientId), Payload)]
    reformat = mconcat . fmap go
      where
        go (Recipient uid _ cids, pay) = (\cid -> ((uid, cid), pay)) <$> cids

    rcps :: [((UserId, Maybe ClientId, Bool), (Recipient, Payload))]
    rcps = mconcat $ go <$> filter (not . (^. pushTransient)) pushes
      where
        go :: Push -> [((UserId, Maybe ClientId, Bool), (Recipient, List1 Object))]
        go psh = do
          rcp <- Set.toList . fromRange $ psh ^. pushRecipients
          pure $
            ( ( psh ^. pushOrigin
              , clientIdFromConnId <$> psh ^. pushOriginConnection
              , psh ^. pushNativeIncludeOrigin
              )
            , (rcp, psh ^. pushPayload)
            )


-- | (There is certainly a fancier implementation using '<%=' or similar, but this one is easier to
-- reverse engineer later.)
--
-- TODO: we shouldn't even pretend to use randomness here, and just return increasing numbers.
mockMkNotificationId
  :: (HasCallStack, m ~ MockGundeck)
  => m NotificationId
mockMkNotificationId = Id <$> state go
  where
    go env = case random (env ^. msNonRandomGen) of
      (uuid, g') -> (uuid, env & msNonRandomGen .~ g')

mockListAllPresences
  :: (HasCallStack, m ~ MockGundeck)
  => [UserId] -> m [[Presence]]
mockListAllPresences uids = do
  allrecipients :: Map UserId Recipient
    <- asks (^. meRecipients) <&>
       Map.fromList . fmap (\rcp@(Recipient uid _ _) -> (uid, rcp))
  pure $ maybe [] fakePresences . (`Map.lookup` allrecipients) <$> uids

-- | Fake implementation of 'Web.bulkPush'.
mockBulkPush
  :: (HasCallStack, m ~ MockGundeck)
  => [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
mockBulkPush notifs = do
  env <- ask

  let delivered :: [(Notification, [Presence])]
      delivered =  [ (nid, prcs)
                   | (nid, filter (`elem` deliveredprcs) -> prcs) <- notifs
                   , not $ null prcs  -- (sic!) (this is what gundeck currently does)
                   ]

      deliveredprcs :: [Presence]
      deliveredprcs = filter isreachable . mconcat . fmap fakePresences $ env ^. meRecipients

      isreachable :: Presence -> Bool
      isreachable prc = (userId prc, fromJust $ clientId prc) `elem` (env ^. meWSReachable)

  forM_ delivered $ \(notif, prcs) -> do
    forM_ prcs $ \prc -> do
      modify $ msWSQueue %~ \queue -> deliver queue (userId prc, clientIdFromConnId $ connId prc) (ntfPayload notif)

  pure $ (_1 %~ ntfId) <$> delivered

-- | persisting notification is not needed for the tests at the moment, so we do nothing here.
mockStreamAdd
  :: (HasCallStack, m ~ MockGundeck)
  => NotificationId -> List1 NotificationTarget -> Payload -> NotificationTTL -> m ()
mockStreamAdd _ _ _ _ = pure ()

mockPushNative
  :: (HasCallStack, m ~ MockGundeck)
  => Notification -> Push -> [Address "no-keys"] -> m ()
mockPushNative _nid ((^. pushPayload) -> payload) addrs = do
  (flip elem -> isreachable) <- asks (^. meNativeReachable)
  forM_ addrs $ \addr -> do
    when (isreachable addr) . modify $ msNativeQueue %~ \queue -> deliver queue (addr ^. addrUser, addr ^. addrClient) payload

mockLookupAddress
  :: (HasCallStack, m ~ MockGundeck)
  => UserId -> m [Address "no-keys"]
mockLookupAddress uid = do
  getaddr <- asks (^. meNativeAddress)
  maybe (pure []) (pure . Map.elems) $ Map.lookup uid getaddr

mockBulkSend
  :: (HasCallStack, m ~ MockGundeck)
  => URI -> BulkPushRequest
  -> m (URI, Either SomeException BulkPushResponse)
mockBulkSend uri notifs = do
  reachables :: Set PushTarget
    <- Set.map (uncurry PushTarget . (_2 %~ fakeConnId)) <$> asks (^. meWSReachable)

  let getstatus trgt = if trgt `Set.member` reachables
                       then PushStatusOk
                       else PushStatusGone

      flat :: [(Notification, PushTarget)]
      flat = case notifs of
        (BulkPushRequest ntifs) ->
          mconcat $ (\(ntif, trgts) -> (ntif,) <$> trgts) <$> ntifs

  forM_ flat $ \(ntif, ptgt) -> do
    when (getstatus ptgt == PushStatusOk) $ do
      modify $ msWSQueue %~ \queue -> deliver queue (ptUserId ptgt, clientIdFromConnId $ ptConnId ptgt) (ntfPayload ntif)

  pure . (uri,) . Right $ BulkPushResponse
    [ (ntfId ntif, trgt, getstatus trgt) | (ntif, trgt) <- flat ]


----------------------------------------------------------------------
-- helpers

deliver :: NotifQueue -> (UserId, ClientId) -> Payload -> NotifQueue
deliver queue qkey qval = Map.alter (Just . tweak) qkey queue
  where
    tweak Nothing      = Set.singleton (payloadToInt qval)
    tweak (Just qvals) = Set.insert    (payloadToInt qval) qvals

payloadToInt :: Payload -> Int
payloadToInt (List1 (toList -> [toList -> [Number x]])) = round $ toRational (x * 100)
payloadToInt bad = error $ "unexpected Payload: " <> show bad
