module Wire.ClientSubsystem.InterpreterSpec (spec) where

import Data.Aeson qualified as A
import Data.Default
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import System.Logger.Message (Msg)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Property
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error (FederationError)
import Wire.API.Push.V2 qualified as V2
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.Team.LegalHold.Internal
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserEvent
import Wire.Arbitrary
import Wire.ClientStore hiding (claimPrekey)
import Wire.ClientSubsystem
import Wire.ClientSubsystem.Error
import Wire.ClientSubsystem.Interpreter
import Wire.DeleteQueue
import Wire.DeleteQueue.InMemory
import Wire.EmailSubsystem
import Wire.Events
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter
import Wire.GalleyAPIAccess hiding (newClient)
import Wire.InternalEvent
import Wire.MockInterpreters
import Wire.NotificationSubsystem
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.Sequential
import Wire.Sem.Logger
import Wire.Sem.Now (Now)
import Wire.StoredUser
import Wire.Util

data ClientSubsystemTestResult a = ClientSubsystemTestResult
  { authState :: MockAuthenticationState,
    result :: Either ClientError a,
    deletions :: [InternalNotification],
    events :: [MiniEvent],
    pushes :: [Push]
  }

type ClientSubsystemTestEffects =
  [ ClientStore,
    State ClientStoreState,
    Now,
    NotificationSubsystem,
    GalleyAPIAccess,
    Events,
    EmailSubsystem,
    DeleteQueue,
    Input ClientSubsystemConfig,
    Input (Local ()),
    FederationAPIAccess FederatorClient,
    Logger (Msg -> Msg),
    Concurrency 'Unsafe,
    Error ClientError,
    Error FederationError,
    State [Push],
    State [MiniEvent],
    State [InternalNotification],
    State MockAuthenticationState
  ]

runClientSubsystemTest ::
  forall a.
  [StoredUser] ->
  Sem (ClientSubsystem ': ClientSubsystemTestEffects) a ->
  ClientSubsystemTestResult a
runClientSubsystemTest users action =
  let interpreted :: Sem ClientSubsystemTestEffects a
      interpreted =
        runClientSubsystem
          mockAuthenticationSubsystemInterpreter
          (runInMemoryUserSubsytemInterpreter users mempty)
          action
      (authState, (deletions, (events, (pushes, result)))) =
        run
          . runState emptyMockAuthenticationState
          . runState @[InternalNotification] []
          . runState @[MiniEvent] []
          . runState @[Push] []
          . runError
          . runError
          . sequentiallyPerformConcurrency
          . noopLogger
          . noFederationAPIAccess @_ @FederatorClient
          . runInputConst (toLocalUnsafe testDomain ())
          . runInputConst (ClientSubsystemConfig 7 False)
          . inMemoryDeleteQueueInterpreter
          . noopEmailSubsystemInterpreter
          . miniEventInterpreter
          . miniGalleyAPIAccess mempty mempty def
          . inMemoryNotificationSubsystemInterpreter
          . interpretNowConst defaultTime
          . evalState emptyClientStoreState
          . runInMemoryClientStoreInterpreterWithState
          $ interpreted
   in ClientSubsystemTestResult {authState, result = fromRight (error "unexpected federation error") result, deletions, events, pushes}

expectRight ::
  (Show e, Show a) =>
  Either e a ->
  (a -> Property) ->
  Property
expectRight result' assertRight =
  counterexample ("unexpected result: " <> show result') $
    case result' of
      Left resultErr -> counterexample ("unexpected error: " <> show resultErr) False
      Right value -> assertRight value

assertSingle ::
  (Show a) =>
  String ->
  [a] ->
  (a -> Property) ->
  Property
assertSingle what xs assertItem = case xs of
  [] -> counterexample ("expected one " <> what <> ", but got none") False
  [x] -> assertItem x
  _ : _ -> counterexample ("expected one " <> what <> ", but got many: " <> show xs) False

assertClientEvent :: UserId -> Maybe ConnId -> Event -> MiniEvent -> Property
assertClientEvent uid mConn expected miniEvent =
  conjoin
    [ miniEvent.userId === uid,
      miniEvent.mConnId === mConn,
      miniEvent.event === expected
    ]

assertClientPush :: UserId -> Maybe ConnId -> EventType -> Push -> Property
assertClientPush uid mConn expected push =
  case A.fromJSON @Event (A.Object push.json) of
    A.Success actual ->
      conjoin
        [ push.origin === Just uid,
          push.conn === mConn,
          push.transient === False,
          push.route === V2.RouteAny,
          push.nativePriority === Nothing,
          push.recipients === [Recipient uid V2.RecipientClientsAll],
          push.apsData === Nothing,
          push.isCellsEvent === False,
          eventType actual === expected
        ]
    _ -> counterexample ("Failed to decode push: " <> show push) False

spec :: Spec
spec = describe "ClientSubsystem.Interpreter" do
  prop "adds and looks up a client" $ \user (FakeLastPrekey lpk) ->
    let luid = toLocalUnsafe testDomain user.id
        new = newClient PermanentClientType lpk
        clientId = clientIdFromPrekey (unpackLastPrekey lpk)
        expectedClient =
          Client
            { clientId,
              clientType = PermanentClientType,
              clientTime = toUTCTimeMillis defaultTime,
              clientClass = Nothing,
              clientLabel = Nothing,
              clientCookie = Nothing,
              clientModel = Nothing,
              clientCapabilities = mempty,
              clientMLSPublicKeys = mempty,
              clientLastActive = Nothing
            }
        testResult =
          runClientSubsystemTest [user] do
            added <- addClient luid Nothing new
            stored <- lookupLocalClients user.id
            pure (added, stored)
        auth = testResult.authState
     in expectRight testResult.result $ \value ->
          conjoin
            [ value === (expectedClient, [expectedClient]),
              auth.verificationCodeCalls === 1,
              auth.reAuthCalls === 0,
              auth.revokeCookiesCalls === 0,
              testResult.deletions === [],
              testResult.events === [],
              assertSingle "push" testResult.pushes (assertClientPush user.id Nothing EventTypeClientAdded)
            ]

  prop "removes client" $ \user conn (FakeLastPrekey lpk) ->
    let uid = user.id
        luid = toLocalUnsafe testDomain user.id
        new = newClient PermanentClientType lpk
        clientId = clientIdFromPrekey (unpackLastPrekey lpk)
        expectedClient =
          Client
            { clientId,
              clientType = PermanentClientType,
              clientTime = toUTCTimeMillis defaultTime,
              clientClass = Nothing,
              clientLabel = Nothing,
              clientCookie = Nothing,
              clientModel = Nothing,
              clientCapabilities = mempty,
              clientMLSPublicKeys = mempty,
              clientLastActive = Nothing
            }
        testResult =
          runClientSubsystemTest [user] do
            added <- addClient luid Nothing new
            removeClient uid conn clientId Nothing
            stored <- lookupLocalClients user.id
            pure (added, stored)
        auth = testResult.authState
     in expectRight testResult.result $ \(added, clients) ->
          conjoin
            [ added === expectedClient,
              clients === [],
              auth.verificationCodeCalls === 1,
              auth.reAuthCalls === 1,
              auth.revokeCookiesCalls === 0,
              testResult.deletions === [DeleteClient clientId uid (Just conn)],
              testResult.events === [],
              assertSingle "push" testResult.pushes (assertClientPush uid Nothing EventTypeClientAdded)
            ]

  prop "legal hold client cannot be removed" $ \user conn (FakeLastPrekey lpk) ->
    let uid = user.id
        luid = toLocalUnsafe testDomain user.id
        new = newClient LegalHoldClientType lpk
        clientId = clientIdFromPrekey (unpackLastPrekey lpk)
        testResult =
          runClientSubsystemTest [user] do
            void $ addClient luid Nothing new
            removeClient uid conn clientId Nothing
     in counterexample ("unexpected result: " <> show testResult.result) $
          case testResult.result of
            Left ClientLegalHoldCannotBeRemoved ->
              conjoin
                [ testResult.authState.verificationCodeCalls === 1,
                  testResult.authState.reAuthCalls === 0,
                  testResult.authState.revokeCookiesCalls === 0,
                  testResult.deletions === [],
                  assertSingle "event" testResult.events (assertClientEvent uid Nothing (UserEvent (UserLegalHoldEnabled uid))),
                  assertSingle "push" testResult.pushes (assertClientPush uid Nothing EventTypeClientAdded)
                ]
            Left clientErr ->
              counterexample ("unexpected ClientError: " <> show clientErr) False
            Right _ ->
              counterexample "legal hold client removal was expected to fail, but it succeeded" False

  prop "adds and removes legal hold client" $ \user (FakeLastPrekey lpk) ->
    let uid = user.id
        luid = toLocalUnsafe testDomain user.id
        new = newClient LegalHoldClientType lpk
        clientId = clientIdFromPrekey (unpackLastPrekey lpk)
        expectedClient =
          Client
            { clientId,
              clientType = LegalHoldClientType,
              clientTime = toUTCTimeMillis defaultTime,
              clientClass = Just LegalHoldClient,
              clientLabel = Nothing,
              clientCookie = Nothing,
              clientModel = Nothing,
              clientCapabilities = ClientCapabilityList (Set.singleton ClientSupportsLegalholdImplicitConsent),
              clientMLSPublicKeys = mempty,
              clientLastActive = Nothing
            }
        testResult =
          runClientSubsystemTest [user] do
            added <- addClient luid Nothing new
            removeLegalHoldClient uid
            stored <- lookupLocalClients user.id
            pure (added, stored)
        auth = testResult.authState
     in expectRight testResult.result $ \(added, clients) ->
          conjoin
            [ added === expectedClient,
              clients === [],
              auth.verificationCodeCalls === 1,
              auth.reAuthCalls === 0,
              auth.revokeCookiesCalls === 0,
              testResult.deletions === [DeleteClient clientId uid Nothing],
              testResult.events
                === [ MkMiniEvent uid Nothing (UserEvent (UserLegalHoldDisabled uid)),
                      MkMiniEvent uid Nothing (UserEvent (UserLegalHoldEnabled uid))
                    ],
              assertSingle "push" testResult.pushes (assertClientPush uid Nothing EventTypeClientAdded)
            ]

  prop "requests a legal hold client" $ \user (FakeLastPrekey lpk) ->
    let uid = user.id
        req = LegalHoldClientRequest uid lpk
        clientId = clientIdFromPrekey (unpackLastPrekey lpk)
        expectedEvent =
          UserEvent
            ( LegalHoldClientRequested
                (LegalHoldClientRequestedData uid lpk clientId)
            )
        testResult =
          runClientSubsystemTest [user] do
            publishLegalHoldClientRequested uid req
     in expectRight testResult.result $ \() ->
          conjoin
            [ testResult.deletions === [],
              assertSingle "event" testResult.events (assertClientEvent uid Nothing expectedEvent),
              testResult.pushes === []
            ]

  prop "update client" $ \user (FakeUpdateClient update) (FakeLastPrekey lpk) ->
    let uid = user.id
        luid = toLocalUnsafe testDomain uid
        new = newClient PermanentClientType lpk
        clientId = clientIdFromPrekey (unpackLastPrekey lpk)
        expectedClient =
          Client
            { clientId,
              clientType = PermanentClientType,
              clientTime = toUTCTimeMillis defaultTime,
              clientClass = Nothing,
              clientLabel = update.updateClientLabel,
              clientCookie = Nothing,
              clientModel = Nothing,
              clientCapabilities = fromMaybe mempty update.updateClientCapabilities,
              clientMLSPublicKeys = update.updateClientMLSPublicKeys,
              clientLastActive = Nothing
            }
        testResult =
          runClientSubsystemTest [user] do
            void $ addClient luid Nothing new
            updateClient uid clientId update
            stored <- lookupLocalClients user.id
            pure (head stored)
        auth = testResult.authState
     in expectRight testResult.result $ \value ->
          conjoin
            [ value === expectedClient,
              auth.verificationCodeCalls === 1,
              auth.reAuthCalls === 0,
              auth.revokeCookiesCalls === 0,
              testResult.deletions === [],
              testResult.events === [],
              assertSingle "push" testResult.pushes (assertClientPush uid Nothing EventTypeClientAdded)
            ]

  prop "claim prekey" $ \user (FakeLastPrekey lpk) ->
    let uid = user.id
        domain = testDomain
        luid = toLocalUnsafe domain uid
        new = newClient PermanentClientType lpk
        clientId = clientIdFromPrekey (unpackLastPrekey lpk)
        testResult =
          runClientSubsystemTest [user] do
            void $ addClient luid Nothing new
            claimPrekey (ProtectedUser uid) uid domain clientId
     in expectRight testResult.result $ \case
          Nothing -> counterexample "expected a client prekey, but got nothing" False
          Just pk -> pk.prekeyClient === clientId

  prop "claim local prekey" $ \user (FakeLastPrekey lpk) ->
    let uid = user.id
        domain = testDomain
        luid = toLocalUnsafe domain uid
        new = newClient PermanentClientType lpk
        clientId = clientIdFromPrekey (unpackLastPrekey lpk)
        testResult =
          runClientSubsystemTest [user] do
            void $ addClient luid Nothing new
            claimLocalPrekey (ProtectedUser uid) uid clientId
     in expectRight testResult.result $ \case
          Nothing -> counterexample "expected a client prekey, but got nothing" False
          Just pk -> pk.prekeyClient === clientId

  prop "claim prekey bundle" $ \user (FakeLastPrekey lpk1) (FakeLastPrekey lpk2) ->
    (lpk1 /= lpk2)
      ==> let uid = user.id
              domain = testDomain
              luid = toLocalUnsafe domain uid
              new1 = newClient PermanentClientType lpk1
              new2 = newClient PermanentClientType lpk2
              clientId1 = clientIdFromPrekey (unpackLastPrekey lpk1)
              clientId2 = clientIdFromPrekey (unpackLastPrekey lpk2)
              expectedClientIds = Set.fromList [clientId1, clientId2]
              testResult =
                runClientSubsystemTest [user] do
                  void $ addClient luid Nothing new1
                  void $ addClient luid Nothing new2
                  claimPrekeyBundle (ProtectedUser uid) domain uid
           in expectRight testResult.result $ \bundle ->
                (bundle.prekeyUser === uid)
                  .&&. (Set.fromList (fmap (.prekeyClient) bundle.prekeyClients) === expectedClientIds)

  prop "claim local prekey bundle" $ \user (FakeLastPrekey lpk1) (FakeLastPrekey lpk2) ->
    (lpk1 /= lpk2)
      ==> let uid = user.id
              domain = testDomain
              luid = toLocalUnsafe domain uid
              new1 = newClient PermanentClientType lpk1
              new2 = newClient PermanentClientType lpk2
              clientId1 = clientIdFromPrekey (unpackLastPrekey lpk1)
              clientId2 = clientIdFromPrekey (unpackLastPrekey lpk2)
              expectedClientIds = Set.fromList [clientId1, clientId2]
              testResult =
                runClientSubsystemTest [user] do
                  void $ addClient luid Nothing new1
                  void $ addClient luid Nothing new2
                  claimLocalPrekeyBundle (ProtectedUser uid) uid
           in expectRight testResult.result $ \bundle ->
                (bundle.prekeyUser === uid)
                  .&&. (Set.fromList (fmap (.prekeyClient) bundle.prekeyClients) === expectedClientIds)

  prop "claim multi prekey bundles v3" $ \protectee testData ->
    (unique testData)
      ==> let domain = testDomain
              testResult =
                runClientSubsystemTest (fmap fst testData) do
                  for_ testData $ \(user, (FakeLastPrekey lpk)) -> do
                    let uid = user.id
                        luid = toLocalUnsafe domain uid
                        new = newClient PermanentClientType lpk
                    addClient luid Nothing new
                  let qUserClients = QualifiedUserClients $ Map.fromList [(domain, Map.fromList (fmap toUserClients testData))]
                  claimMultiPrekeyBundlesV3 (ProtectedUser protectee) qUserClients
           in expectRight testResult.result $ \m ->
                let qClientMap = m.getQualifiedUserClientPrekeyMap.qualifiedUserClientMap
                    userMap = fromMaybe mempty $ Map.lookup domain qClientMap
                 in Map.size qClientMap === 1 .&&. Map.size userMap === length testData

  prop "claim multi prekey bundles" $ \protectee testData ->
    (unique testData)
      ==> let domain = testDomain
              testResult =
                runClientSubsystemTest (fmap fst testData) do
                  for_ testData $ \(user, (FakeLastPrekey lpk)) -> do
                    let uid = user.id
                        luid = toLocalUnsafe domain uid
                        new = newClient PermanentClientType lpk
                    addClient luid Nothing new
                  let qUserClients = QualifiedUserClients $ Map.fromList [(domain, Map.fromList (fmap toUserClients testData))]
                  claimMultiPrekeyBundles (ProtectedUser protectee) qUserClients
           in expectRight testResult.result $ \m ->
                let qClientMap = m.qualifiedUserClientPrekeys.qualifiedUserClientMap
                    userMap = fromMaybe mempty $ Map.lookup domain qClientMap
                 in Map.size qClientMap === 1 .&&. Map.size userMap === length testData

  prop "claim local multi prekey bundles" $ \protectee testData ->
    (unique testData)
      ==> let domain = testDomain
              testResult =
                runClientSubsystemTest (fmap fst testData) do
                  for_ testData $ \(user, (FakeLastPrekey lpk)) -> do
                    let uid = user.id
                        luid = toLocalUnsafe domain uid
                        new = newClient PermanentClientType lpk
                    addClient luid Nothing new
                  let userClients = UserClients $ Map.fromList (fmap toUserClients testData)
                  claimLocalMultiPrekeyBundles (ProtectedUser protectee) userClients
           in expectRight testResult.result $ \m ->
                let clientMap = m.getUserClientPrekeyMap.userClientMap
                 in Map.size clientMap === length testData
  where
    toUserClients (user, FakeLastPrekey lpk) = (user.id, Set.fromList [clientIdFromPrekey (unpackLastPrekey lpk)])

    unique testData =
      length testData == length (Set.fromList (fmap ((.id) . fst) testData))
        && length testData == length (Set.fromList ((fmap snd) testData))

newtype FakeUpdateClient = FakeUpdateClient {unFakeUpdateClient :: UpdateClient}
  deriving (Show, Eq, Generic)

instance Arbitrary FakeUpdateClient where
  arbitrary = do
    update <- arbitrary
    (FakeLastPrekey lpk) <- arbitrary
    keys <- QC.sublistOf somePrekeys
    pure $
      FakeUpdateClient $
        update
          { updateClientLastKey = lpk <$ update.updateClientLastKey,
            updateClientPrekeys = keys
          }
