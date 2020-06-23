{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.HashMap.Lazy as HashMap
import Data.Id
import Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as MSet
import qualified Data.List.NonEmpty as NE
import Data.List1
import qualified Data.Map as Map
import Data.Misc (Milliseconds (Ms), (<$$>))
import Data.Range
import qualified Data.Scientific as Scientific
import qualified Data.Set as Set
import Data.String.Conversions
import Gundeck.Aws.Arn as Aws
import Gundeck.Options
import Gundeck.Push
import Gundeck.Push.Native as Native
import Gundeck.Push.Websocket as Web
import Gundeck.Types hiding (recipient)
import Gundeck.Types.BulkPush
import Imports
import qualified Network.URI as URI
import System.Logger.Class as Log hiding (trace)
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

----------------------------------------------------------------------
-- env

-- | We really don't care about the actual payloads anywhere in these tests, just that the right
-- ones arrive over the right connections.  So 'genPayload' is not very exhaustive, but only
-- generates small objects with one field containing a numeric value.  It would be nice to represent
-- this in the 'Payload' type, but the 'List1 Aeson.Object' structure is used in the production
-- code, so in the end it is more awkward than nice.
type Payload = List1 Aeson.Object

data ClientInfo = ClientInfo
  { _ciNativeAddress :: Maybe (Address, Bool {- reachable -}),
    _ciWSReachable :: Bool
  }
  deriving (Eq, Show)

newtype MockEnv = MockEnv
  { _meClientInfos :: Map UserId (Map ClientId ClientInfo)
  }
  deriving (Eq, Show)

data MockState = MockState
  { -- | A record of notifications that have been pushed via websockets.
    _msWSQueue :: NotifQueue,
    -- | A record of notifications that have been pushed via native push.
    _msNativeQueue :: NotifQueue,
    -- | Non-transient notifications that are stored in the database first thing before
    -- delivery (so clients can always come back and pick them up later until they expire).
    _msCassQueue :: NotifQueue
  }
  deriving (Eq)

-- | For each client we store the set of notifications they are scheduled to receive.  Notification
-- 'Payload's are converted into 'Int's for simplicity and to enable less verbose test errors (see
-- 'payloadToInt').
type NotifQueue = Map (UserId, ClientId) IntMultiSet

makeLenses ''ClientInfo

makeLenses ''MockEnv

makeLenses ''MockState

instance Show MockState where
  show (MockState w n c) =
    intercalate
      "\n"
      ["", "websocket: " <> show w, "native: " <> show n, "cassandra: " <> show c, ""]

emptyMockState :: MockState
emptyMockState = MockState mempty mempty mempty

-- these custom instances make for better error reports if tests fail.
instance ToJSON MockEnv where
  toJSON (MockEnv mp) =
    Aeson.object
      ["clientInfos" Aeson..= mp]

instance ToJSON ClientInfo where
  toJSON (ClientInfo native wsreach) =
    Aeson.object
      [ "native" Aeson..= native,
        "wsReachable" Aeson..= wsreach
      ]

instance ToJSON Address where
  toJSON adr =
    Aeson.object
      [ "user" Aeson..= (adr ^. addrUser),
        "transport" Aeson..= (adr ^. addrTransport),
        "app" Aeson..= (adr ^. addrApp),
        "token" Aeson..= (adr ^. addrToken),
        "endpoint" Aeson..= (serializeFakeAddrEndpoint $ adr ^. addrEndpoint),
        "conn" Aeson..= (adr ^. addrConn),
        "client" Aeson..= (adr ^. addrClient)
      ]

serializeFakeAddrEndpoint :: EndpointArn -> (Text, Transport, AppName)
serializeFakeAddrEndpoint ((^. snsTopic) -> eptopic) =
  ( case eptopic ^. endpointId of EndpointId txt -> txt,
    eptopic ^. endpointTransport,
    eptopic ^. endpointAppName
  )

instance FromJSON MockEnv where
  parseJSON = withObject "MockEnv" $ \env ->
    MockEnv
      <$> env Aeson..: "clientInfos"

instance FromJSON ClientInfo where
  parseJSON = withObject "ClientInfo" $ \cinfo ->
    ClientInfo
      <$> (cinfo Aeson..: "native")
      <*> (cinfo Aeson..: "wsReachable")

instance FromJSON Address where
  parseJSON = withObject "Address" $ \adr ->
    Address
      <$> (adr Aeson..: "user")
      <*> (mkFakeAddrEndpoint <$> adr Aeson..: "endpoint")
      <*> (adr Aeson..: "conn")
      <*> ( pushToken
              <$> (adr Aeson..: "transport")
              <*> (adr Aeson..: "app")
              <*> (adr Aeson..: "token")
              <*> (adr Aeson..: "client")
          )

mkFakeAddrEndpoint :: (Text, Transport, AppName) -> EndpointArn
mkFakeAddrEndpoint (epid, transport, app) = Aws.mkSnsArn Tokyo (Account "acc") eptopic
  where
    eptopic = mkEndpointTopic (ArnEnv "") transport app (EndpointId epid)

----------------------------------------------------------------------
-- env generators

-- | Generate an environment containing a mix of recipients with different
-- levels of brokenness:
--
-- 1. web socket delivery will work
-- 2. web socket delivery will NOT work, native push token registered, push will succeed
-- 3. web socket delivery will NOT work, native push token registered, push will fail
-- 4. web socket delivery will NOT work, no native push token registered
genMockEnv :: HasCallStack => Gen MockEnv
genMockEnv = do
  -- This function generates a 'ClientInfo' that corresponds to one of the
  -- four scenarios above
  let genClientInfo :: HasCallStack => UserId -> ClientId -> Gen ClientInfo
      genClientInfo uid cid = do
        _ciNativeAddress <-
          QC.oneof
            [ pure Nothing,
              do
                protoaddr <- genProtoAddress uid cid
                reachable <- arbitrary
                pure $ Just (protoaddr, reachable)
            ]
        _ciWSReachable <- arbitrary
        pure ClientInfo {..}
  -- Generate a list of users
  uids :: [UserId] <-
    nub <$> listOf1 genId
  -- For every user, generate several clients (preferring less clients)
  cidss :: [[ClientId]] <-
    let gencids _uid = do
          len <-
            QC.frequency
              [ (4, QC.choose (1, 3)),
                (1, QC.choose (4, 8))
              ]
          vectorOf len genClientId
        nubrec = upper mempty
          where
            upper _ [] = []
            upper visited (ys : xs) = case lower visited ys of
              ([], visited') -> upper visited' xs -- uids, cidss are not aligned!
              (ys', visited') -> ys' : upper visited' xs
            lower visited [] = ([], visited)
            lower visited (y : ys) =
              if y `elem` visited
                then lower visited ys
                else case lower (y `Set.insert` visited) ys of
                  (ys', visited') -> (y : ys', visited')
     in nubrec <$> forM uids gencids
  -- Build an 'MockEnv' containing a map with all those 'ClientInfo's, and
  -- check that it validates
  env <-
    MockEnv . Map.fromList . fmap (_2 %~ Map.fromList) <$> do
      forM (zip uids cidss) $ \(uid, cids) ->
        (uid,) <$> do
          forM cids $ \cid -> (cid,) <$> genClientInfo uid cid
  validateMockEnv env & either error (const $ pure env)

-- Try to shrink a 'MockEnv' by removing some users from '_meClientInfos'.
shrinkMockEnv :: HasCallStack => MockEnv -> [MockEnv]
shrinkMockEnv (MockEnv cis) =
  MockEnv . Map.fromList
    <$> filter (not . null) (shrinkList (const []) (Map.toList cis))

validateMockEnv :: forall m. MonadError String m => MockEnv -> m ()
validateMockEnv env = do
  checkIdsInNativeAddresses
  where
    -- (if you want to vaidate anything else, here is the place!)

    -- UserId and ClientId contained in Address must match the keys under which they are stored.
    checkIdsInNativeAddresses :: m ()
    checkIdsInNativeAddresses = do
      forM_ (Map.toList $ env ^. meClientInfos) $ \(uid, cinfos) -> do
        forM_ (Map.toList cinfos) $ \(cid, cinfo) -> do
          forM_ (cinfo ^. ciNativeAddress) $ \(adr, _) -> do
            unless (uid == adr ^. addrUser && cid == adr ^. addrClient) $ do
              throwError (show (uid, cid, adr))

genRecipients :: HasCallStack => Int -> MockEnv -> Gen [Recipient]
genRecipients numrcp env = do
  uids <- take numrcp <$> shuffle (allUsers env)
  genRecipient' env `mapM` uids

genRecipient :: HasCallStack => MockEnv -> Gen Recipient
genRecipient env = do
  uid <- QC.elements (allUsers env)
  genRecipient' env uid

genRecipient' :: HasCallStack => MockEnv -> UserId -> Gen Recipient
genRecipient' env uid = do
  route <- genRoute
  cids <-
    QC.frequency
      [ (1, pure RecipientClientsAll),
        (3, RecipientClientsSome <$> sublist1Of (clientIdsOfUser env uid))
      ]
  pure $ Recipient uid route cids

genRoute :: HasCallStack => Gen Route
genRoute = QC.elements [minBound ..]

genId :: Gen (Id a)
genId = do
  gen <- mkStdGen <$> arbitrary
  pure . Id . fst $ random gen

genClientId :: Gen ClientId
genClientId = newClientId <$> arbitrary

genProtoAddress :: HasCallStack => UserId -> ClientId -> Gen Address
genProtoAddress _addrUser _addrClient = do
  _addrTransport :: Transport <- QC.elements [minBound ..]
  arnEpId :: Text <- arbitrary
  let _addrApp = "AppName"
      _addrEndpoint = mkFakeAddrEndpoint (arnEpId, _addrTransport, _addrApp)
      _addrConn = fakeConnId _addrClient
      _addrPushToken = pushToken _addrTransport _addrApp (Token "tok") _addrClient
  pure Address {..}

genPushes :: MockEnv -> Gen [Push]
genPushes = listOf . genPush

genPush :: HasCallStack => MockEnv -> Gen Push
genPush env = do
  let alluids = allUsers env
  sender <- QC.elements alluids
  rcps :: Range 1 1024 (Set Recipient) <- do
    numrcp <- choose (1, min 1024 (length alluids))
    rcps <- genRecipients numrcp env
    unsafeRange . Set.fromList <$> dropSomeDevices `mapM` rcps
  pload <- genPayload
  inclorigin <- arbitrary
  transient <- arbitrary
  let connIdsByUser = fmap fakeConnId <$> Map.fromList (allRecipients env)
      allConnIds = mconcat $ Map.elems connIdsByUser
  onlyPushToConnections <- do
    -- from the list of all recipient connections, sometimes add some here.
    oneof
      [ pure mempty,
        fmap Set.fromList $ QC.sublistOf allConnIds
      ]
  originConnection <- do
    -- if one of the recipients is the sender, we may 'Just' pick one of the devices of that
    -- recipient here, or 'Nothing'.
    let genOriginConnId = case mconcat . fmap extract . toList . fromRange $ rcps of
          [] -> pure Nothing
          conns@(_ : _) -> Just <$> QC.elements conns
          where
            extract (Recipient uid _ _) | uid /= sender = []
            extract (Recipient _ _ (RecipientClientsSome cids)) = fakeConnId <$> toList cids
            extract (Recipient _ _ RecipientClientsAll) = lookupAll
              where
                lookupAll = fromMaybe [] $ Map.lookup sender connIdsByUser
    oneof
      [ pure Nothing,
        genOriginConnId
      ]
  pure $
    newPush sender rcps pload
      & pushConnections .~ onlyPushToConnections
      & pushOriginConnection .~ originConnection
      & pushTransient .~ transient
      & pushNativeIncludeOrigin .~ inclorigin

-- (not covered: pushNativeAps, pushNativePriority)

-- | Shuffle devices.  With probability 0.5, drop at least one device, but not all.  If number of
-- devices is @<2@ or if devices are set to 'RecipientClientsAll', the input is returned.
dropSomeDevices :: Recipient -> Gen Recipient
dropSomeDevices =
  recipientClients %%~ \case
    RecipientClientsAll -> pure RecipientClientsAll
    RecipientClientsSome cids -> do
      numdevs :: Int <-
        oneof
          [ pure $ length cids,
            choose (1, max 1 (length cids - 1))
          ]
      RecipientClientsSome . unsafeList1 . take numdevs
        <$> QC.shuffle (toList cids)

shrinkPushes :: HasCallStack => [Push] -> [[Push]]
shrinkPushes = shrinkList shrinkPush
  where
    shrinkPush :: HasCallStack => Push -> [Push]
    shrinkPush psh = (\rcps -> psh & pushRecipients .~ rcps) <$> shrinkRecipients (psh ^. pushRecipients)
    shrinkRecipients :: HasCallStack => Range 1 1024 (Set Recipient) -> [Range 1 1024 (Set Recipient)]
    shrinkRecipients = fmap unsafeRange . map Set.fromList . filter (not . null) . shrinkList shrinkRecipient . Set.toList . fromRange
    shrinkRecipient :: HasCallStack => Recipient -> [Recipient]
    shrinkRecipient _ = []

-- | See 'Payload'.
genPayload :: Gen Payload
genPayload = do
  num :: Int <- arbitrary
  pure $ List1 (HashMap.singleton "val" (Aeson.toJSON num) NE.:| [])

genNotif :: Gen Notification
genNotif = Notification <$> genId <*> arbitrary <*> genPayload

genNotifs :: MockEnv -> Gen [(Notification, [Presence])]
genNotifs env = fmap uniqNotifs . listOf $ do
  notif <- genNotif
  prcs <- nub . mconcat <$> listOf (fakePresences' env <$> genRecipient env)
  pure (notif, prcs)
  where
    uniqNotifs = nubBy ((==) `on` (ntfId . fst))

shrinkNotifs :: HasCallStack => [(Notification, [Presence])] -> [[(Notification, [Presence])]]
shrinkNotifs = shrinkList (\(notif, prcs) -> (notif,) <$> shrinkList (const []) prcs)

----------------------------------------------------------------------
-- monad type and instances

newtype MockGundeck a = MockGundeck
  {fromMockGundeck :: ReaderT MockEnv (StateT MockState (RandT StdGen Identity)) a}
  deriving (Functor, Applicative, Monad, MonadReader MockEnv, MonadState MockState, MonadRandom)

runMockGundeck :: MockEnv -> MockGundeck a -> (a, MockState)
runMockGundeck env (MockGundeck m) =
  runIdentity . (`evalRandT` mkStdGen 0) $ runStateT (runReaderT m env) emptyMockState

instance MonadThrow MockGundeck where
  throwM = error . show -- (we are not expecting any interesting errors in these tests, so we might
  -- as well crash badly here, as long as it doesn't go unnoticed...)

instance MonadPushAll MockGundeck where
  mpaNotificationTTL = pure $ NotificationTTL 300 -- (longer than we want any test to take.)
  mpaMkNotificationId = mockMkNotificationId
  mpaListAllPresences = mockListAllPresences
  mpaBulkPush = mockBulkPush
  mpaStreamAdd = mockStreamAdd
  mpaPushNative = mockPushNative
  mpaForkIO = id -- just don't fork.  (this *may* cause deadlocks in principle, but as long as it
  -- doesn't, this is good enough for testing).

  mpaRunWithBudget = \_ _ -> id -- no throttling needed as long as we don't overdo it in the tests...

instance MonadNativeTargets MockGundeck where
  mntgtLogErr _ = pure ()
  mntgtLookupAddresses = mockLookupAddresses

instance MonadMapAsync MockGundeck where
  mntgtPerPushConcurrency = pure Nothing -- (unbounded)
  mntgtMapAsync f xs = Right <$$> mapM f xs -- (no concurrency)

instance MonadPushAny MockGundeck where
  mpyPush = mockOldSimpleWebPush

instance MonadBulkPush MockGundeck where
  mbpBulkSend = mockBulkSend
  mbpDeleteAllPresences _ = pure () -- FUTUREWORK: test presence deletion logic
  mbpPosixTime = pure $ Ms 1545045904275 -- (time is constant)
  mbpMapConcurrently = mapM -- (no concurrency)
  mbpMonitorBadCannons _ = pure () -- (no monitoring)

instance Log.MonadLogger MockGundeck where
  log _ _ = pure () -- (no logging)

----------------------------------------------------------------------
-- monad implementation

-- | For a set of push notifications, compute the expected result of sending all of them.
-- This should match the result of doing 'Gundeck.Push.pushAll'.
--
-- Every push causes some notifications to be sent via websockets, sent via native transport,
-- and stored in Cassandra. The complete logic of handling a push is correspondingly specified
-- in 'handlePushWS', 'handlePushNative' and 'handlePushCass' respectively. Those parts are all
-- independent of each other.
mockPushAll ::
  (HasCallStack, m ~ MockGundeck) =>
  [Push] ->
  m ()
mockPushAll pushes = do
  forM_ pushes $ \psh -> do
    handlePushWS psh
    handlePushNative psh
    handlePushCass psh

-- | From a single 'Push', deliver only those notifications that real Gundeck would deliver via
-- websockets.
handlePushWS ::
  (HasCallStack, m ~ MockGundeck) =>
  Push ->
  m ()
handlePushWS Push {..} = do
  env <- ask
  forM_ (fromRange _pushRecipients) $ \(Recipient uid _ cids) -> do
    let cids' = case cids of
          RecipientClientsAll -> clientIdsOfUser env uid
          RecipientClientsSome cc -> toList cc
    forM_ cids' $ \cid -> do
      -- Condition 1: only devices with a working websocket connection will get the push.
      let isReachable = wsReachable env (uid, cid)
      -- Condition 2: we never deliver pushes to the originating device.
      let isOriginDevice = origin == (uid, Just cid)
      -- Condition 3: push to cid iff (a) listed in pushConnections or (b) pushConnections is empty.
      let isWhitelisted = null _pushConnections || fakeConnId cid `elem` _pushConnections
      when (isReachable && not isOriginDevice && isWhitelisted) $
        msWSQueue %= deliver (uid, cid) _pushPayload
  where
    origin = (_pushOrigin, clientIdFromConnId <$> _pushOriginConnection)

-- | From a single 'Push', deliver eligible 'Notification's via native transport.
handlePushNative ::
  (HasCallStack, m ~ MockGundeck) =>
  Push ->
  m ()
handlePushNative Push {..}
  -- Condition 1: transient pushes are not sent via native transport.
  | _pushTransient = pure ()
handlePushNative Push {..} = do
  env <- ask
  forM_ (fromRange _pushRecipients) $ \(Recipient uid route cids) -> do
    let cids' = case cids of
          RecipientClientsAll -> clientIdsOfUser env uid
          RecipientClientsSome cc -> toList cc
    forM_ cids' $ \cid -> do
      -- Condition 2: 'RouteDirect' pushes are not eligible for pushing via native transport.
      let isNative = route /= RouteDirect
      -- Condition 3: to get a native push, the device must be native-reachable but not
      -- websocket-reachable, as websockets take priority.
      let isReachable = nativeReachable env (uid, cid) && not (wsReachable env (uid, cid))
      -- Condition 4: the originating *user* can receive a native push only if
      -- 'pushNativeIncludeOrigin' is true. Even so, the originating *device* should never
      -- receive a push.
      let isOriginUser = uid == fst origin
          isOriginDevice = origin == (uid, Just cid)
          isAllowedPerOriginRules =
            not isOriginUser || (_pushNativeIncludeOrigin && not isOriginDevice)
      -- Condition 5: push to cid iff (a) listed in pushConnections or (b) pushConnections is empty.
      let isWhitelisted = null _pushConnections || fakeConnId cid `elem` _pushConnections
      when (isNative && isReachable && isAllowedPerOriginRules && isWhitelisted) $
        msNativeQueue %= deliver (uid, cid) _pushPayload
  where
    origin = (_pushOrigin, clientIdFromConnId <$> _pushOriginConnection)

-- | From a single 'Push', store only those notifications that real Gundeck would put into
-- Cassandra.
handlePushCass ::
  (HasCallStack, m ~ MockGundeck) =>
  Push ->
  m ()
handlePushCass Push {..}
  -- Condition 1: transient pushes are not put into Cassandra.
  | _pushTransient = pure ()
handlePushCass Push {..} = do
  forM_ (fromRange _pushRecipients) $ \(Recipient uid _ cids) -> do
    let cids' = case cids of
          RecipientClientsAll -> [ClientId mempty]
          -- clients are stored in cassandra as a list with a notification.  empty list is
          -- intepreted as "all clients" by 'Gundeck.Notification.Data.toNotif'.  (here, we just
          -- store a specific 'ClientId' that signifies "no client".)
          RecipientClientsSome cc -> toList cc
    forM_ cids' $ \cid ->
      msCassQueue %= deliver (uid, cid) _pushPayload

mockMkNotificationId ::
  (HasCallStack, m ~ MockGundeck) =>
  m NotificationId
mockMkNotificationId = Id <$> getRandom

mockListAllPresences ::
  (HasCallStack, m ~ MockGundeck) =>
  [UserId] ->
  m [[Presence]]
mockListAllPresences uids =
  asks $ fmap fakePresences . filter ((`elem` uids) . fst) . allRecipients

-- | Fake implementation of 'Web.bulkPush'.
mockBulkPush ::
  (HasCallStack, m ~ MockGundeck) =>
  [(Notification, [Presence])] ->
  m [(NotificationId, [Presence])]
mockBulkPush notifs = do
  env <- ask
  let delivered :: [(Notification, [Presence])]
      delivered =
        [ (nid, prcs)
          | (nid, filter (`elem` deliveredprcs) -> prcs) <- notifs,
            not $ null prcs -- (sic!) (this is what gundeck currently does)
        ]
      deliveredprcs :: [Presence]
      deliveredprcs = filter isreachable . mconcat . fmap fakePresences $ allRecipients env
      isreachable :: Presence -> Bool
      isreachable prc = wsReachable env (userId prc, fromJust $ clientId prc)
  forM_ delivered $ \(notif, prcs) -> do
    forM_ prcs $ \prc ->
      msWSQueue
        %= deliver (userId prc, clientIdFromConnId $ connId prc) (ntfPayload notif)
  pure $ (_1 %~ ntfId) <$> delivered

-- | persisting notification is not needed for the tests at the moment, so we do nothing here.
mockStreamAdd ::
  (HasCallStack, m ~ MockGundeck) =>
  NotificationId ->
  List1 NotificationTarget ->
  Payload ->
  NotificationTTL ->
  m ()
mockStreamAdd _ (toList -> targets) pay _ =
  forM_ targets $ \tgt -> case (tgt ^. targetClients) of
    clients@(_ : _) -> forM_ clients $ \cid ->
      msCassQueue %= deliver (tgt ^. targetUser, cid) pay
    [] ->
      msCassQueue %= deliver (tgt ^. targetUser, ClientId mempty) pay

mockPushNative ::
  (HasCallStack, m ~ MockGundeck) =>
  Notification ->
  Push ->
  [Address] ->
  m ()
mockPushNative _nid ((^. pushPayload) -> payload) addrs = do
  env <- ask
  forM_ addrs $ \addr -> do
    when (nativeReachableAddr env addr) $
      msNativeQueue
        %= deliver (addr ^. addrUser, addr ^. addrClient) payload

mockLookupAddresses ::
  (HasCallStack, m ~ MockGundeck) =>
  UserId ->
  m [Address]
mockLookupAddresses uid = do
  cinfos :: [ClientInfo] <-
    Map.elems
      . fromMaybe (error $ "mockLookupAddress: unknown UserId: " <> show uid)
      . Map.lookup uid
      <$> asks (^. meClientInfos)
  pure . catMaybes $ (^? ciNativeAddress . _Just . _1) <$> cinfos

mockBulkSend ::
  (HasCallStack, m ~ MockGundeck) =>
  URI ->
  BulkPushRequest ->
  m (URI, Either SomeException BulkPushResponse)
mockBulkSend uri notifs = do
  getstatus <- mkWSStatus
  let flat :: [(Notification, PushTarget)]
      flat = case notifs of
        (BulkPushRequest ntifs) ->
          mconcat $ (\(ntif, trgts) -> (ntif,) <$> trgts) <$> ntifs
  forM_ flat $ \(ntif, ptgt) -> do
    when (getstatus ptgt == PushStatusOk) $
      msWSQueue
        %= deliver (ptUserId ptgt, clientIdFromConnId $ ptConnId ptgt) (ntfPayload ntif)
  pure . (uri,) . Right $
    BulkPushResponse
      [(ntfId ntif, trgt, getstatus trgt) | (ntif, trgt) <- flat]

mockOldSimpleWebPush ::
  (HasCallStack, m ~ MockGundeck) =>
  Notification ->
  List1 NotificationTarget ->
  UserId ->
  Maybe ConnId ->
  Set ConnId ->
  m [Presence]
mockOldSimpleWebPush notif tgts _senderid mconnid connWhitelist = do
  env <- ask
  getstatus <- mkWSStatus
  let clients :: [(UserId, ClientId)]
      clients =
        -- reformat
        fmap (\(PushTarget uid connid) -> (uid, clientIdFromConnId connid))
          -- drop all broken web sockets
          . filter ((== PushStatusOk) . getstatus)
          -- do not push to sending device
          . filter ((/= mconnid) . Just . ptConnId)
          -- reformat
          . mconcat
          . fmap
            ( \tgt ->
                PushTarget (tgt ^. targetUser) . fakeConnId
                  <$> (tgt ^. targetClients)
            )
          -- apply filters
          . fmap (connWhitelistSieve . emptyMeansFullHack)
          $ toList tgts
      connWhitelistSieve :: NotificationTarget -> NotificationTarget
      connWhitelistSieve =
        if null connWhitelist
          then id
          else targetClients %~ filter ((`elem` connWhitelist) . fakeConnId)
      emptyMeansFullHack :: NotificationTarget -> NotificationTarget
      emptyMeansFullHack tgt =
        tgt & targetClients %~ \case
          [] -> clientIdsOfUser env (tgt ^. targetUser)
          same@(_ : _) -> same
  forM_ clients $ \(userid, clientid) -> do
    msWSQueue %= deliver (userid, clientid) (ntfPayload notif)
  pure $ uncurry fakePresence <$> clients

----------------------------------------------------------------------
-- helpers

-- | (it may be possible to drop this type in favor of more sophisticated use of quickcheck's
-- counterexample.)
newtype Pretty a = Pretty a
  deriving (Eq, Ord)

instance Aeson.ToJSON a => Show (Pretty a) where
  show (Pretty a) = cs $ Aeson.encodePretty a

shrinkPretty :: HasCallStack => (a -> [a]) -> Pretty a -> [Pretty a]
shrinkPretty shrnk (Pretty xs) = Pretty <$> shrnk xs

sublist1Of :: HasCallStack => [a] -> Gen (List1 a)
sublist1Of [] = error "sublist1Of: empty list"
sublist1Of xs =
  sublistOf xs >>= \case
    [] -> sublist1Of xs
    c : cc -> pure (list1 c cc)

unsafeList1 :: HasCallStack => [a] -> List1 a
unsafeList1 [] = error "unsafeList1: empty list"
unsafeList1 (x : xs) = list1 x xs

deliver :: (UserId, ClientId) -> Payload -> NotifQueue -> NotifQueue
deliver qkey qval queue = Map.alter (Just . tweak) qkey queue
  where
    tweak Nothing = MSet.singleton (payloadToInt qval)
    tweak (Just qvals) = MSet.insert (payloadToInt qval) qvals

-- | Get the number contained in the payload.
payloadToInt :: Payload -> Int
payloadToInt (List1 (toList -> [toList -> [Number x]]))
  | Just n <- Scientific.toBoundedInteger x = n
payloadToInt bad = error $ "unexpected Payload: " <> show bad

mkWSStatus :: MockGundeck (PushTarget -> PushStatus)
mkWSStatus = do
  env <- ask
  pure $ \trgt ->
    if wsReachable env (ptUserId trgt, clientIdFromConnId $ ptConnId trgt)
      then PushStatusOk
      else PushStatusGone

wsReachable :: MockEnv -> (UserId, ClientId) -> Bool
wsReachable (MockEnv mp) (uid, cid) =
  maybe False (^. ciWSReachable) $
    (Map.lookup uid >=> Map.lookup cid) mp

nativeReachable :: MockEnv -> (UserId, ClientId) -> Bool
nativeReachable (MockEnv mp) (uid, cid) =
  maybe False (^. _2) $
    (Map.lookup uid >=> Map.lookup cid >=> (^. ciNativeAddress)) mp

nativeReachableAddr :: MockEnv -> Address -> Bool
nativeReachableAddr env addr = nativeReachable env (addr ^. addrUser, addr ^. addrClient)

allUsers :: MockEnv -> [UserId]
allUsers = fmap fst . allRecipients

allRecipients :: MockEnv -> [(UserId, [ClientId])]
allRecipients (MockEnv mp) = (_2 %~ Map.keys) <$> Map.toList mp

clientIdsOfUser :: HasCallStack => MockEnv -> UserId -> [ClientId]
clientIdsOfUser (MockEnv mp) uid =
  maybe (error "unknown UserId") Map.keys $ Map.lookup uid mp

-- | See also: 'fakePresence'.
fakePresences :: (UserId, [ClientId]) -> [Presence]
fakePresences (uid, cids) = fakePresence uid <$> cids

-- | See also: 'fakePresence'.
fakePresences' :: MockEnv -> Recipient -> [Presence]
fakePresences' env (Recipient uid _ cids) =
  fakePresence uid <$> case cids of
    RecipientClientsAll -> clientIdsOfUser env uid
    RecipientClientsSome cc -> toList cc

-- | Currently, we only create 'Presence's from 'Push' requests, which contains 'ClientId's, but no
-- 'ConnId's.  So in contrast to the production code where the two are generated independently, we
-- maintain identity (except for the type) between 'ClientId' and 'ConnId'.  (This makes switching
-- back between the two trivial without having to maintain a stateful mapping.)  Furthermore, we do
-- not cover the @isNothing (clientId prc)@ case.
fakePresence :: UserId -> ClientId -> Presence
fakePresence userId clientId_ = Presence {..}
  where
    clientId = Just clientId_
    connId = fakeConnId clientId_
    resource = URI . fromJust $ URI.parseURI "http://127.0.0.1:8080"
    createdAt = 0
    __field = mempty

-- | See also: 'fakePresence'.
fakeConnId :: ClientId -> ConnId
fakeConnId = ConnId . cs . client

clientIdFromConnId :: ConnId -> ClientId
clientIdFromConnId = ClientId . cs . fromConnId
