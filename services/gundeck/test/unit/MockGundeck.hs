{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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

-- | This is 'MockGundeck', a re-implementation of the 'Gundeck' monad with mock effects.  This has
-- been developed to test 'Gundeck.Push.pushAll' and 'Gundeck.Push.Websocket.bulkPush', but can be
-- extended.
--
-- Besides 'MockGundeck' and its instances, there are two important types: 'MockEnv' and
-- 'MockState'.  'MockEnv' contains the system input to the test run; 'MockState' is manipulated by
-- the mock implementations and can be examined to make sure all the expected effects have been
-- caused.
--
-- This module is structured as follows: first, 'MockEnv' and 'MockState' are defined with their
-- 'Arbitrary' generators.  Then, 'MockGundeck' and its instances are defined; then the actual mock
-- effects are implemented.
--
-- There is a cascade of mockings: 'pushAll' can be run on the same input as 'mockPushAll', and
-- outputs and states can be tested to be equal.  'pushAll' calls 'bulkPush', but in the @instance
-- MonadPushAll MockGundeck@, you can either call the real thing or 'mockBulkPush'.
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
-- env

type Payload = List1 Aeson.Object

data MockEnv = MockEnv
  { _meRecipients      :: Map UserId Recipient
  , _meNativeAddress   :: Map UserId (Map ClientId (Address "no-keys"))
  , _meWSReachable     :: Set (UserId, ClientId)
  , _meNativeReachable :: Set (Address "no-keys")
  }
  deriving (Eq, Show)

data MockState = MockState
  { _msNonRandomGen    :: StdGen
  , _msWSQueue         :: NotifQueue
  , _msNativeQueue     :: NotifQueue
  , _msCassQueue       :: NotifQueue
  }

-- | Notification payload is aggregated as `Int`.  The same payload may be delivered in more than
-- one notification.  We do not keep track of numbers of deliveries (relying on QuickCheck to the
-- generate different payloads to get to other counter-examples for the same bugs).
type NotifQueue = Map (UserId, ClientId) (Set Int)

makeLenses ''MockEnv
makeLenses ''MockState

instance Eq MockState where
  (MockState _ w n c) == (MockState _ w' n' c') = w == w' && n == n' && c == c'

instance Show MockState where
  show (MockState _ w n c) = intercalate "\n"
    ["", "websocket: " <> show w, "native: " <> show n, "cassandra: " <> show c, ""]

instance Semigroup MockState where
  (MockState _ ws nat cass) <> (MockState gen ws' nat' cass') =
    MockState gen (Map.unionWith (<>) ws ws')
                  (Map.unionWith (<>) nat nat')
                  (Map.unionWith (<>) cass cass')

instance Monoid MockState where
  mempty = MockState (mkStdGen 0) mempty mempty mempty

-- (serializing test cases makes replay easier.)
instance ToJSON MockEnv where
  toJSON env = object
    [ "meRecipients"      Aeson..= Map.elems (env ^. meRecipients)
        -- (recipients are stored as list without keys for backwards compatibility)
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
    <$> (Map.fromList . fmap (\rcp -> (rcp ^. recipientId, rcp)) <$> (env Aeson..: "meRecipients"))
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
  recipientList :: [Recipient]
    <- nubBy ((==) `on` (^. recipientId)) <$> listOf1 genRecipient

  let _meRecipients :: Map UserId Recipient
      _meRecipients = Map.fromList $ (\rcp -> (rcp ^. recipientId, rcp)) <$> recipientList

  protoaddrs :: [UserId -> ClientId -> Address "no-keys"]
    <- do
      len <- let l = length _meRecipients in choose (l `div` 2, l)
      vectorOf len genProtoAddress

  let addrs :: [(Recipient, Address "no-keys")]
      addrs = mconcat $ zipWith go recipientList protoaddrs
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

  _meWSReachable <- genPredicate . mconcat $ recipientToIds <$> recipientList
  _meNativeReachable <- genPredicate (snd <$> addrs)

  let env = MockEnv {..}
  validateMockEnv env & either error (const $ pure env)

shrinkMockEnv :: MockEnv -> [MockEnv]
shrinkMockEnv env = do
  rcps :: [(UserId, Recipient)]
    <- shrinkList (const []) . Map.toList $ (env ^. meRecipients)

  let env' :: MockEnv
      env' = env
        & meRecipients .~ Map.fromList rcps
        & meNativeAddress %~ naddrs
        & meWSReachable %~ wsrchbl
        & meNativeReachable %~ ntrchbl

      clients :: Set (UserId, ClientId)
      clients = Set.fromList . mconcat $ (\(_, Recipient uid _ cids) -> (uid,) <$> cids) <$> rcps

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
  uid   <- genId
  cids  <- nub <$> listOf1 genClientId
  route <- QC.elements [RouteAny, RouteDirect]  -- REFACTOR: see 'Route' type about missing 'RouteNative'.
  pure $ Recipient uid route cids

genId :: Gen (Id a)
genId = do
  gen <- mkStdGen <$> resize 100 arbitrary
  pure . Id . fst $ random gen

genClientId :: Gen ClientId
genClientId = newClientId <$> resize 50 arbitrary

-- | See also: 'fakePresence'.
fakePresences :: Recipient -> [Presence]
fakePresences (Recipient uid _ cids) = fakePresence uid <$> cids

-- | Currently, we only create 'Presence's from 'Push' requests, which contains 'ClientId's, but no
-- 'ConnId's.  So in contrast to the production code where the two are generated independently, we
-- maintain identity (except for the type) between 'ClientId' and 'ConnId'.  (This makes switching
-- back between the two trivial without having to maintain a stateful mapping.)  Furthermore, we do
-- not cover the @isNothing (clientId prc)@ case.
fakePresence :: UserId -> ClientId -> Presence
fakePresence userId clientId_ = Presence {..}
  where
    clientId  = Just clientId_
    connId    = fakeConnId clientId_
    resource  = URI . fromJust $ URI.parseURI "http://127.0.0.1:8080"
    createdAt = 0
    __field   = mempty

-- | See also: 'fakePresence'.
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

genPushes :: MockEnv -> Gen [Push]
genPushes = listOf . genPush

genPush :: MockEnv -> Gen Push
genPush (Map.elems . (^. meRecipients) -> allrcps) = do
  sender :: UserId <- (^. recipientId) <$> QC.elements allrcps
  rcps :: Range 1 1024 (Set Recipient) <- do
    numrcp <- choose (1, min 1024 (length allrcps))
    rcps   <- vectorOf numrcp (QC.elements allrcps)
    unsafeRange . Set.fromList <$> dropSomeDevices `mapM` rcps
  pload <- genPayload
  inclorigin <- arbitrary
  pure $ newPush sender rcps pload
    -- TODO: & pushConnections .~ _
    -- TODO: & pushOriginConnection .~ _
    -- TODO: & pushTransient .~ _
    & pushNativeIncludeOrigin .~ inclorigin
    -- (not covered: pushNativeAps, pushNativePriority)

-- | Shuffle devices.  With probability 0.5, drop at least one device, but not all.  If number of
-- devices is @<2@, the input is returned.
dropSomeDevices :: Recipient -> Gen Recipient
dropSomeDevices (Recipient uid route cids) = do
  numdevs :: Int <- oneof [ pure $ length cids
                          , choose (1, length cids - 1)
                          ]
  Recipient uid route . take numdevs <$> QC.shuffle cids

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

genNotifs :: MockEnv -> Gen [(Notification, [Presence])]
genNotifs env = fmap uniqNotifs . listOf $ do
  let allprcs = mconcat . fmap fakePresences . Map.elems $ env ^. meRecipients
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

instance MonadPushAny MockGundeck where
  mpyPush = mockOldSimpleWebPush

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
  getrcp <- mkGetRecipient
  modify $ (msWSQueue     .~ expectWS env getrcp)
         . (msNativeQueue .~ expectNative env)
         . (msCassQueue   .~ expectCass)
  where
    expectWS :: MockEnv -> (UserId -> Recipient) -> NotifQueue
    expectWS env getrcp
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
            defaults = getrcp uid ^. recipientClients

        -- otherwise, no special hidden meaning.
        insertAllClients same@(_, (Recipient _ _ (_:_), _)) = [same]

    expectNative :: MockEnv -> NotifQueue
    expectNative env
      = foldl' (uncurry . deliver) mempty
      . filter reachable
      . reformat
      . mconcat . fmap removeSome
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

        removeSome :: ((UserId, Maybe ClientId, Bool), (Recipient, Payload)) -> [(Recipient, Payload)]
        removeSome ((_, _, _), (Recipient _ RouteDirect _, _)) =
          -- never native-push recipients marked 'RouteDirect'.
          []
        removeSome ((snduid, _, False), same@(Recipient rcpuid _ _, _)) =
          -- if pushNativeIncludeOrigin is False, none of the originator's devices will receive
          -- native pushes.
          [same | snduid /= rcpuid]
        removeSome ((_, Just sndcid, True), (Recipient rcpuid route cids, pay)) =
          -- if originating client is known and pushNativeIncludeOrigin is True, filter out just
          -- that client, and native-push to all other devices of the originator.
          [(Recipient rcpuid route $ filter (/= sndcid) cids, pay)]
        removeSome ((_, Nothing, True), same) =
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

    expectCass :: NotifQueue
    expectCass = foldl' (uncurry . deliver) mempty . mconcat $ go <$> pushes
      where
        go :: Push -> [((UserId, ClientId), Payload)]
        go psh =
          [ ((uid, cid), psh ^. pushPayload)
          | not $ psh ^. pushTransient
          , Recipient uid _ cids <- toList . fromRange $ psh ^. pushRecipients
          , cid <- cids
          ]

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
    <- asks (^. meRecipients)
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
      deliveredprcs = filter isreachable . mconcat . fmap fakePresences $ Map.elems (env ^. meRecipients)

      isreachable :: Presence -> Bool
      isreachable prc = (userId prc, fromJust $ clientId prc) `elem` (env ^. meWSReachable)

  forM_ delivered $ \(notif, prcs) -> do
    forM_ prcs $ \prc -> do
      modify $ msWSQueue %~ \queue ->
        deliver queue (userId prc, clientIdFromConnId $ connId prc) (ntfPayload notif)

  pure $ (_1 %~ ntfId) <$> delivered

-- | persisting notification is not needed for the tests at the moment, so we do nothing here.
mockStreamAdd
  :: (HasCallStack, m ~ MockGundeck)
  => NotificationId -> List1 NotificationTarget -> Payload -> NotificationTTL -> m ()
mockStreamAdd _ (toList -> targets) pay _ =
  forM_ targets $ \tgt ->
    forM_ (tgt ^. targetClients) $ \cid ->
      modify $ msCassQueue %~ \queue ->
        deliver queue (tgt ^. targetUser, cid) pay

mockPushNative
  :: (HasCallStack, m ~ MockGundeck)
  => Notification -> Push -> [Address "no-keys"] -> m ()
mockPushNative _nid ((^. pushPayload) -> payload) addrs = do
  (flip elem -> isreachable) <- asks (^. meNativeReachable)
  forM_ addrs $ \addr -> do
    when (isreachable addr) . modify $ msNativeQueue %~ \queue ->
      deliver queue (addr ^. addrUser, addr ^. addrClient) payload

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
  getstatus <- mkWSStatus
  let flat :: [(Notification, PushTarget)]
      flat = case notifs of
        (BulkPushRequest ntifs) ->
          mconcat $ (\(ntif, trgts) -> (ntif,) <$> trgts) <$> ntifs

  forM_ flat $ \(ntif, ptgt) -> do
    when (getstatus ptgt == PushStatusOk) $ do
      modify $ msWSQueue %~ \queue ->
        deliver queue (ptUserId ptgt, clientIdFromConnId $ ptConnId ptgt) (ntfPayload ntif)

  pure . (uri,) . Right $ BulkPushResponse
    [ (ntfId ntif, trgt, getstatus trgt) | (ntif, trgt) <- flat ]

mockOldSimpleWebPush
  :: (HasCallStack, m ~ MockGundeck)
  => Notification
  -> List1 NotificationTarget
  -> UserId
  -> Maybe ConnId
  -> Set ConnId
  -> m [Presence]
mockOldSimpleWebPush _ _ _ _ (Set.null -> False) =
  error "connection whitelists are not implemented in this test."
  -- (see 'genPush' above)

mockOldSimpleWebPush notif tgts _senderid mconnid _ = do
  getrecp   <- mkGetRecipient
  getstatus <- mkWSStatus

  let clients :: [(UserId, ClientId)]
      clients
        = -- reformat
          fmap (\(PushTarget uid connid) -> (uid, clientIdFromConnId connid))
          -- drop all broken web sockets
        . filter ((== PushStatusOk) . getstatus)
          -- do not push to sending device
        . filter ((/= mconnid) . Just . ptConnId)
          -- reformat
        . mconcat . fmap (\tgt ->
                            PushTarget (tgt ^. targetUser) . fakeConnId
                              <$> (tgt ^. targetClients))
        . fmap emptyMeansFullHack
        $ toList tgts

      emptyMeansFullHack :: NotificationTarget -> NotificationTarget
      emptyMeansFullHack tgt = tgt & targetClients %~ \case
        []   -> getrecp (tgt ^. targetUser) ^. recipientClients
        same@(_:_) -> same

  forM_ clients $ \(userid, clientid) -> do
    modify $ msWSQueue %~ \queue ->
      deliver queue (userid, clientid) (ntfPayload notif)

  pure $ uncurry fakePresence <$> clients


----------------------------------------------------------------------
-- helpers

-- | (it may be possible to drop this type in favor of more sophisticated use of quickcheck's
-- counterexample.)
newtype Pretty a = Pretty a
  deriving (Eq, Ord)

instance Aeson.ToJSON a => Show (Pretty a) where
  show (Pretty a) = cs $ Aeson.encodePretty a

shrinkPretty :: (a -> [a]) -> Pretty a -> [Pretty a]
shrinkPretty shrnk (Pretty xs) = Pretty <$> shrnk xs


deliver :: NotifQueue -> (UserId, ClientId) -> Payload -> NotifQueue
deliver queue qkey qval = Map.alter (Just . tweak) qkey queue
  where
    tweak Nothing      = Set.singleton (payloadToInt qval)
    tweak (Just qvals) = Set.insert    (payloadToInt qval) qvals

payloadToInt :: Payload -> Int
payloadToInt (List1 (toList -> [toList -> [Number x]])) = round $ toRational (x * 100)
payloadToInt bad = error $ "unexpected Payload: " <> show bad

mkWSStatus :: MockGundeck (PushTarget -> PushStatus)
mkWSStatus = do
  reachables :: Set PushTarget
    <- Set.map (uncurry PushTarget . (_2 %~ fakeConnId)) <$> asks (^. meWSReachable)
  pure $ \trgt -> if trgt `Set.member` reachables then PushStatusOk else PushStatusGone

-- | Throws an asynchronous error if 'MockEnv' does not contain the user (this should be ok for
-- testing).
mkGetRecipient :: MockGundeck (UserId -> Recipient)
mkGetRecipient = do
  allrcps <- asks (^. meRecipients)
  pure $ \uid -> fromMaybe (error "User missing in MockEnv") $ Map.lookup uid allrcps
