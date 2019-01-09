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
import Control.Monad.Random
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

-- | We really don't care about the actual payloads anywhere in these tests, just that the right
-- ones arrive over the right connections.  So 'genPayload' is not very exhaustive, but only
-- generates small objects with one field containing a numeric value.  It would be nice to represent
-- this in the 'Payload' type, but the 'List1 Aeson.Object' structure is used in the production
-- code, so in the end it is more awkward than nice.
type Payload = List1 Aeson.Object

data ClientInfo = ClientInfo
    { _ciNativeAddress   :: Maybe (Address "no-keys", Bool{- reachable -})
    , _ciWSReachable     :: Bool
    }
  deriving (Eq, Show)

newtype MockEnv = MockEnv
    { _meClientInfos :: Map UserId (Map ClientId ClientInfo)
    }
  deriving (Eq, Show)

data MockState = MockState
  { _msWSQueue         :: NotifQueue
  , _msNativeQueue     :: NotifQueue
  , _msCassQueue       :: NotifQueue
  }
  deriving (Eq)

-- | For each client we store the set of notifications they are scheduled to receive.  Notification
-- 'Payload's are converted into 'Int's for simplicity and to enable less verbose test errors (see
-- 'payloadToInt').
--
-- In real world, the same payload may be delivered in more than one notification.  Here we do not
-- keep track of duplicate payloads, relying on QuickCheck to generate different payloads to get to
-- other counter-examples for the same bugs.
type NotifQueue = Map (UserId, ClientId) (Set Int)

makeLenses ''ClientInfo
makeLenses ''MockEnv
makeLenses ''MockState

instance Show MockState where
  show (MockState w n c) = intercalate "\n"
    ["", "websocket: " <> show w, "native: " <> show n, "cassandra: " <> show c, ""]

emptyMockState :: MockState
emptyMockState = MockState mempty mempty mempty

-- (serializing test cases makes replay easier.)
instance ToJSON MockEnv where
  toJSON (MockEnv mp) = Aeson.object
    [ "clientInfos" Aeson..= Map.toList (Map.toList <$> mp) ]

instance ToJSON ClientInfo where
  toJSON (ClientInfo native wsreach) = Aeson.object
    [ "native" Aeson..= native
    , "wsReachable" Aeson..= wsreach
    ]

instance ToJSON (Address s) where
  toJSON adr = Aeson.object
    [ "user"      Aeson..= (adr ^. addrUser)
    , "transport" Aeson..= (adr ^. addrTransport)
    , "app"       Aeson..= (adr ^. addrApp)
    , "token"     Aeson..= (adr ^. addrToken)
    , "endpoint"  Aeson..= (serializeFakeAddrEndpoint $ adr ^. addrEndpoint)
    , "conn"      Aeson..= (adr ^. addrConn)
    , "client"    Aeson..= (adr ^. addrClient)
    ]

serializeFakeAddrEndpoint :: EndpointArn -> (Text, Transport, AppName)
serializeFakeAddrEndpoint ((^. snsTopic) -> eptopic) =
  ( case eptopic ^. endpointId of EndpointId txt -> txt
  , eptopic ^. endpointTransport
  , eptopic ^. endpointAppName
  )

instance FromJSON MockEnv where
  parseJSON = withObject "MockEnv" $ \env -> MockEnv
    <$> (Map.fromList <$$> (Map.fromList <$> env Aeson..: "clientInfos"))

instance FromJSON ClientInfo where
  parseJSON = withObject "ClientInfo" $ \cinfo -> ClientInfo
    <$> (cinfo Aeson..: "native")
    <*> (cinfo Aeson..: "wsReachable")

instance FromJSON (Address s) where
  parseJSON = withObject "Address" $ \adr -> Address
    <$> (adr Aeson..: "user")
    <*> (adr Aeson..: "transport")
    <*> (adr Aeson..: "app")
    <*> (adr Aeson..: "token")
    <*> (mkFakeAddrEndpoint <$> adr Aeson..: "endpoint")
    <*> (adr Aeson..: "conn")
    <*> (adr Aeson..: "client")

mkFakeAddrEndpoint :: (Text, Transport, AppName) -> EndpointArn
mkFakeAddrEndpoint (epid, transport, app) = Aws.mkSnsArn Tokyo (Account "acc") eptopic
  where eptopic = mkEndpointTopic (ArnEnv "") transport app (EndpointId epid)


----------------------------------------------------------------------
-- env generators

-- | Generate an environment containing a mix of recipients with different
-- levels of brokenness:
--
-- 1. web socket delivery will work
-- 2. web socket delivery will NOT work, native push token registered, push will succeed
-- 3. web socket delivery will NOT work, native push token registered, push will fail
-- 4. web socket delivery will NOT work, no native push token registered
genMockEnv :: Gen MockEnv
genMockEnv = do
  -- This function generates a 'ClientInfo' that corresponds to one of the
  -- four scenarios above
  let genClientInfo :: UserId -> ClientId -> Gen ClientInfo
      genClientInfo uid cid = do
        _ciNativeAddress <- QC.oneof
          [ pure Nothing
          , do
              protoaddr <- genProtoAddress uid cid
              reachable <- arbitrary
              pure $ Just (protoaddr, reachable)
          ]
        _ciWSReachable <- arbitrary
        pure ClientInfo{..}

  -- Generate a list of users
  uids :: [UserId]
    <- nub <$> listOf1 genId

  -- For every user, generate several clients
  cidss :: [[ClientId]]
    <- let gencids = do
             len <- QC.choose (1, 8)
             vectorOf len genClientId
       in forM uids . const $ nub <$> gencids

  -- Build an 'MockEnv' containing a map with all those 'ClientInfo's, and
  -- check that it validates
  env <- MockEnv . Map.fromList . fmap (_2 %~ Map.fromList) <$> do
    forM (zip uids cidss) $ \(uid, cids) -> (uid,) <$> do
      forM cids $ \cid -> (cid,) <$> genClientInfo uid cid

  validateMockEnv env & either error (const $ pure env)

-- Try to shrink a 'MockEnv' by removing some users from '_meClientInfos'.
shrinkMockEnv :: MockEnv -> [MockEnv]
shrinkMockEnv (MockEnv cis) = MockEnv . Map.fromList <$> (shrinkList (const []) (Map.toList cis))

validateMockEnv :: forall m. MonadError String m => MockEnv -> m ()
validateMockEnv env = do
  checkIdsInNativeAddresses
  -- (if you want to vaidate anything else, here is the place!)
  where
    -- UserId and ClientId contained in Address must match the keys under which they are stored.
    checkIdsInNativeAddresses :: m ()
    checkIdsInNativeAddresses = do
      forM_ (Map.toList $ env ^. meClientInfos) $ \(uid, cinfos) -> do
        forM_ (Map.toList cinfos) $ \(cid, cinfo) -> do
          forM_ (cinfo ^. ciNativeAddress) $ \(adr, _) -> do
            unless (uid == adr ^. addrUser && cid == adr ^. addrClient) $ do
              throwError (show (uid, cid, adr))

genSubsetOf :: forall a. (Eq a, Ord a) => [a] -> Gen (Set a)
genSubsetOf = fmap Set.fromList . sublistOf

genRecipients :: HasCallStack => Int -> MockEnv -> Gen [Recipient]
genRecipients numrcp env = do
  uids  <- take numrcp <$> shuffle (allUsers env)
  genRecipient' env `mapM` uids

genRecipient :: HasCallStack => MockEnv -> Gen Recipient
genRecipient env = do
  uid   <- QC.elements (allUsers env)
  genRecipient' env uid

genRecipient' :: HasCallStack => MockEnv -> UserId -> Gen Recipient
genRecipient' env uid = do
  route <- genRoute
  cids  <- sublistOf (clientIdsOfUser env uid)
  pure $ Recipient uid route cids

-- REFACTOR: see 'Route' type about missing 'RouteNative'.
genRoute :: HasCallStack => Gen Route
genRoute = QC.elements [RouteAny, RouteDirect]

genId :: Gen (Id a)
genId = do
  gen <- mkStdGen <$> arbitrary
  pure . Id . fst $ random gen

genClientId :: Gen ClientId
genClientId = newClientId <$> arbitrary

genProtoAddress :: UserId -> ClientId -> Gen (Address "no-keys")
genProtoAddress _addrUser _addrClient = do
  _addrTransport :: Transport <- QC.elements [minBound..maxBound]
  arnEpId :: Text <- arbitrary
  let _addrApp = "AppName"
      _addrToken = Token "tok"
      _addrEndpoint = mkFakeAddrEndpoint (arnEpId, _addrTransport, _addrApp)
      _addrConn = fakeConnId _addrClient
  pure Address {..}

genPushes :: MockEnv -> Gen [Push]
genPushes = listOf . genPush

genPush :: MockEnv -> Gen Push
genPush env = do
  let alluids = allUsers env
  sender <- QC.elements alluids
  rcps :: Range 1 1024 (Set Recipient) <- do
    numrcp <- choose (1, min 1024 (length alluids))
    rcps   <- genRecipients numrcp env
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

-- | See 'Payload'.
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
  notif <- genNotif
  prcs <- nub . mconcat <$> listOf (fakePresences' <$> genRecipient env)
  pure (notif, prcs)
  where
    uniqNotifs = nubBy ((==) `on` (ntfId . fst))

shrinkNotifs :: [(Notification, [Presence])] -> [[(Notification, [Presence])]]
shrinkNotifs = shrinkList (\(notif, prcs) -> (notif,) <$> shrinkList (const []) prcs)


----------------------------------------------------------------------
-- monad type and instances

newtype MockGundeck a = MockGundeck
  { fromMockGundeck :: ReaderT MockEnv (StateT MockState (RandT StdGen Identity)) a }
  deriving (Functor, Applicative, Monad, MonadReader MockEnv, MonadState MockState, MonadRandom)

runMockGundeck :: MockEnv -> MockGundeck a -> (a, MockState)
runMockGundeck env (MockGundeck m) =
  runIdentity . (`evalRandT` mkStdGen 0) $ runStateT (runReaderT m env) emptyMockState

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

instance MonadNativeTargets MockGundeck where
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
  modify $ (msWSQueue     .~ expectWS env)
         . (msNativeQueue .~ expectNative env)
         . (msCassQueue   .~ expectCass)
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
        reachable (ids, _) = wsReachable env ids

        removeSelf :: ((UserId, Maybe ClientId, any), (Recipient, Payload)) -> [(Recipient, Payload)]
        removeSelf ((_, Nothing, _), same) =
          [same]
        removeSelf ((_, Just sndcid, _), (Recipient rcpuid route cids, pay)) =
          [(Recipient rcpuid route $ filter (/= sndcid) cids, pay)]

        insertAllClients :: (any, (Recipient, Payload))
                         -> [(any, (Recipient, Payload))]
        -- if the recipient client list is empty, fill in all devices of that user
        insertAllClients (same, (Recipient uid route [], pay)) = [(same, (rcp', pay))]
          where rcp' = Recipient uid route (clientIdsOfUser env uid)

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
        reachable (ids, _) = nativeReachable env ids && not (wsReachable env ids)

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
          where rcp' = Recipient uid route (clientIdsOfUser env uid)
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
mockMkNotificationId = Id <$> getRandom

mockListAllPresences
  :: (HasCallStack, m ~ MockGundeck)
  => [UserId] -> m [[Presence]]
mockListAllPresences uids =
  asks $ fmap fakePresences . filter ((`elem` uids) . fst) . allRecipients

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
      deliveredprcs = filter isreachable . mconcat . fmap fakePresences $ allRecipients env

      isreachable :: Presence -> Bool
      isreachable prc = wsReachable env (userId prc, fromJust $ clientId prc)

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
  env <- ask
  forM_ addrs $ \addr -> do
    when (nativeReachableAddr env addr) . modify $ msNativeQueue %~ \queue ->
      deliver queue (addr ^. addrUser, addr ^. addrClient) payload

mockLookupAddress
  :: (HasCallStack, m ~ MockGundeck)
  => UserId -> m [Address "no-keys"]
mockLookupAddress uid = do
  cinfos :: [ClientInfo]
    <- Map.elems .
       fromMaybe (error $ "mockLookupAddress: unknown UserId: " <> show uid) .
       Map.lookup uid <$>
       asks (^. meClientInfos)
  pure . catMaybes $ (^? ciNativeAddress . _Just . _1) <$> cinfos

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
  env <- ask
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
        []   -> clientIdsOfUser env (tgt ^. targetUser)
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

-- | Return the rounded number contained in the payload.  This is enough to detect message mixups,
-- but less noisy in the printed counter-examples.
payloadToInt :: Payload -> Int
payloadToInt (List1 (toList -> [toList -> [Number x]])) = round $ toRational (x * 100)
payloadToInt bad = error $ "unexpected Payload: " <> show bad

mkWSStatus :: MockGundeck (PushTarget -> PushStatus)
mkWSStatus = do
  env <- ask
  pure $ \trgt -> if wsReachable env (ptUserId trgt, clientIdFromConnId $ ptConnId trgt)
                  then PushStatusOk
                  else PushStatusGone


wsReachable :: MockEnv -> (UserId, ClientId) -> Bool
wsReachable (MockEnv mp) (uid, cid) = maybe False (^. ciWSReachable) $
  (Map.lookup uid >=> Map.lookup cid) mp

nativeReachable :: MockEnv -> (UserId, ClientId) -> Bool
nativeReachable (MockEnv mp) (uid, cid) = maybe False (^. _2) $
  (Map.lookup uid >=> Map.lookup cid >=> (^. ciNativeAddress)) mp

nativeReachableAddr :: MockEnv -> Address s -> Bool
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
fakePresences' :: Recipient -> [Presence]
fakePresences' (Recipient uid _ cids) = fakePresence uid <$> cids

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
