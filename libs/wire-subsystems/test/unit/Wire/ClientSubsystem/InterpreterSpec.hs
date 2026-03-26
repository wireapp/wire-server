module Wire.ClientSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Json.Util (toUTCTimeMillis)
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import System.Logger.Message (Msg)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.ClientStore
import Wire.ClientSubsystem
import Wire.ClientSubsystem.Error
import Wire.ClientSubsystem.Interpreter
import Wire.DeleteQueue
import Wire.DeleteQueue.InMemory
import Wire.EmailSubsystem
import Wire.Events
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter
import Wire.GalleyAPIAccess
import Wire.InternalEvent
import Wire.MockInterpreters
import Wire.NotificationSubsystem
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.Sequential
import Wire.Sem.Logger
import Wire.Sem.Now (Now)
import Wire.StoredUser

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
          . sequentiallyPerformConcurrency
          . noopLogger
          . noFederationAPIAccess @_ @FederatorClient
          . runInputConst (toLocalUnsafe testDomain ())
          . runInputConst (ClientSubsystemConfig 7 False)
          . inMemoryDeleteQueueInterpreter
          . noopEmailSubsystemInterpreter
          . miniEventInterpreter
          . miniGalleyAPIAccess mempty def
          . inMemoryNotificationSubsystemInterpreter
          . interpretNowConst defaultTime
          . evalState emptyClientStoreState
          . runInMemoryClientStoreInterpreterWithState
          $ interpreted
   in ClientSubsystemTestResult {authState, result, deletions, events, pushes}

spec :: Spec
spec = focus $ describe "ClientSubsystem.Interpreter" do
  prop "adds and looksup a client" $ \user ->
    let luid = toLocalUnsafe testDomain user.id
        new = Wire.API.User.Client.newClient PermanentClientType validLastPrekey
        clientId = clientIdFromPrekey (unpackLastPrekey validLastPrekey)
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
     in case testResult.result of
          Left clientErr -> expectationFailure ("unexpected ClientError: " <> show clientErr)
          Right value -> value `shouldBe` (expectedClient, [expectedClient])
          .&&. (testResult.authState.verificationCodeCalls `shouldBe` 1)
          .&&. (testResult.authState.reAuthCalls `shouldBe` 0)
          .&&. (testResult.authState.revokeCookiesCalls `shouldBe` 0)
          .&&. (length testResult.deletions `shouldBe` 0)
          .&&. (length testResult.events `shouldBe` 0)
          .&&. (length testResult.pushes `shouldBe` 1)

validLastPrekey :: LastPrekey
validLastPrekey =
  lastPrekey "pQABARn//wKhAFggnCcZIK1pbtlJf4wRQ44h4w7/sfSgj5oWXMQaUGYAJ/sDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
