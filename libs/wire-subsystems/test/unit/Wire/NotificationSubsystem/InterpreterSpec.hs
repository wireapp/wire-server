module Wire.NotificationSubsystem.InterpreterSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.List1 qualified as List1
import Data.Range (fromRange, toRange)
import Data.Set qualified as Set
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Async (asyncToIOFinal)
import Polysemy.Input
import Polysemy.Writer (tell, writerToIOFinal)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Wire.GundeckAPIAccess
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter

spec :: Spec
spec = describe "NotificationSubsystem.Interpreter" do
  describe "pushImpl" do
    it "chunks and sends all notifications" do
      let mockConfig =
            NotificationSubsystemConfig
              { fanoutLimit = toRange $ Proxy @30,
                chunkSize = 12
              }

      connId2 <- generate arbitrary
      origin2 <- generate arbitrary
      (user1, user21, user22) <- generate arbitrary
      (payload1, payload2) <- generate $ resize 1 arbitrary
      clients1 <- generate $ resize 3 arbitrary
      lotOfRecipients <- generate $ resize 24 arbitrary
      let push1 =
            PushTo
              { _pushConn = Nothing,
                _pushTransient = True,
                _pushRoute = V2.RouteDirect,
                _pushNativePriority = Nothing,
                pushOrigin = Nothing,
                _pushRecipients = Recipient user1 (V2.RecipientClientsSome clients1) :| [],
                pushJson = payload1
              }
          push2 =
            PushTo
              { _pushConn = Just connId2,
                _pushTransient = True,
                _pushRoute = V2.RouteAny,
                _pushNativePriority = Just V2.LowPriority,
                pushOrigin = Just origin2,
                _pushRecipients =
                  Recipient user21 V2.RecipientClientsAll
                    :| [Recipient user22 V2.RecipientClientsAll],
                pushJson = payload2
              }
          duplicatePush = push2
          duplicatePushWithPush1Recipients = push2 {_pushRecipients = _pushRecipients push1}
          largePush = push2 {_pushRecipients = lotOfRecipients}
          pushes :: [PushToUser] =
            [ push1,
              push2,
              duplicatePush,
              duplicatePushWithPush1Recipients,
              largePush
            ]

      actualPushes <-
        runFinal
          . asyncToIOFinal
          . runGundeckAPIAccessMock
          . runInputConst mockConfig
          $ pushImpl pushes

      let expectedPushes =
            Set.fromList $
              map toV2Push
                <$>
                -- It's ok to use chunkPushes here because we're testing
                -- that separately
                chunkPushes mockConfig.chunkSize pushes
      Set.fromList actualPushes `shouldBe` expectedPushes

    it "respects maximum fanout limit" do
      let mockConfig =
            NotificationSubsystemConfig
              { fanoutLimit = toRange $ Proxy @30,
                chunkSize = 12
              }

      connId2 <- generate arbitrary
      origin2 <- generate arbitrary
      (user21, user22) <- generate arbitrary
      (payload1, payload2) <- generate $ resize 1 arbitrary
      lotOfRecipients <- fromList <$> replicateM 31 (generate arbitrary)
      let pushBiggerThanFanoutLimit =
            PushTo
              { _pushConn = Nothing,
                _pushTransient = True,
                _pushRoute = V2.RouteDirect,
                _pushNativePriority = Nothing,
                pushOrigin = Nothing,
                _pushRecipients = lotOfRecipients,
                pushJson = payload1
              }
          pushSmallerThanFanoutLimit =
            PushTo
              { _pushConn = Just connId2,
                _pushTransient = True,
                _pushRoute = V2.RouteAny,
                _pushNativePriority = Just V2.LowPriority,
                pushOrigin = Just origin2,
                _pushRecipients =
                  Recipient user21 V2.RecipientClientsAll
                    :| [Recipient user22 V2.RecipientClientsAll],
                pushJson = payload2
              }
          pushes :: [PushToUser] =
            [ pushBiggerThanFanoutLimit,
              pushSmallerThanFanoutLimit
            ]

      actualPushes <-
        runFinal
          . asyncToIOFinal
          . runGundeckAPIAccessMock
          . runInputConst mockConfig
          $ pushImpl pushes

      let expectedPushes =
            Set.fromList $
              map toV2Push
                <$>
                -- It's ok to use chunkPushes here because we're testing
                -- that separately
                chunkPushes mockConfig.chunkSize [pushSmallerThanFanoutLimit]
      Set.fromList actualPushes `shouldBe` expectedPushes

  describe "toV2Push" do
    it "does the transformation correctly" $ property \(pushToUser :: PushToUser) ->
      let v2Push = toV2Push pushToUser
       in -- Statically determined
          v2Push._pushConnections === mempty
            .&&. v2Push._pushNativeIncludeOrigin === True
            .&&. v2Push._pushNativeEncrypt === True
            .&&. v2Push._pushNativeAps === Nothing
            -- derived from pushToUser
            .&&. v2Push._pushOrigin === pushToUser.pushOrigin
            .&&. v2Push._pushOriginConnection === pushToUser._pushConn
            .&&. v2Push._pushTransient === pushToUser._pushTransient
            .&&. v2Push._pushNativePriority === fromMaybe V2.HighPriority pushToUser._pushNativePriority
            .&&. v2Push._pushPayload === List1.singleton (pushJson pushToUser)
            .&&. Set.map V2._recipientRoute (fromRange v2Push._pushRecipients) === Set.singleton pushToUser._pushRoute
            .&&. Set.map (\r -> Recipient r._recipientId r._recipientClients) (fromRange v2Push._pushRecipients)
              === Set.fromList (toList pushToUser._pushRecipients)

  describe "chunkPushes" do
    it "allows empty push" $ property \limit ->
      chunkPushes limit [] === ([] :: [[PushTo ()]])
    it "produces no empty chunks" $ property \limit (pushes :: [PushTo Int]) ->
      not (any null (chunkPushes limit pushes))
    it "allows concatenation if number was non-zero" $ property \(Positive limit) (pushes :: [PushTo Int]) ->
      (chunkPushes limit pushes >>= reverse >>= normalisePush)
        === (pushes >>= normalisePush)
    it "respects the chunkSize limit" $ property \limit (pushes :: [PushTo Int]) ->
      all ((<= limit) . sizeOfChunks) (chunkPushes limit pushes)

runGundeckAPIAccessMock :: Member (Final IO) r => Sem (GundeckAPIAccess : r) a -> Sem r [[V2.Push]]
runGundeckAPIAccessMock =
  fmap fst . writerToIOFinal . reinterpret \case
    PushV2 pushes -> tell [pushes]

normalisePush :: PushTo a -> [PushTo a]
normalisePush p =
  map
    (\r -> p {_pushRecipients = r :| []})
    (toList (_pushRecipients p))

sizeOfChunks :: [PushTo a] -> Natural
sizeOfChunks = fromIntegral . sum . map (length . _pushRecipients)
