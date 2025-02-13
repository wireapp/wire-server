module Wire.EnterpriseLoginSubsystem.InterpreterSpec where

import Data.Default
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Test.Hspec
import Wire.DomainRegistrationStore
import Wire.DomainVerificationChallengeStore
import Wire.EmailSending
import Wire.EnterpriseLoginSubsystem.Error
import Wire.EnterpriseLoginSubsystem.Interpreter
import Wire.GalleyAPIAccess
import Wire.MockInterpreters.DomainRegistrationStore
import Wire.MockInterpreters.DomainVerificationChallengeStore
import Wire.MockInterpreters.EmailSending
import Wire.MockInterpreters.Error
import Wire.MockInterpreters.GalleyAPIAccess
import Wire.MockInterpreters.Random
import Wire.MockInterpreters.SparAPIAccess
import Wire.MockInterpreters.UserKeyStore
import Wire.MockInterpreters.UserSubsystem
import Wire.ParseException
import Wire.Rpc
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Random
import Wire.SparAPIAccess
import Wire.UserKeyStore
import Wire.UserSubsystem

runDependencies ::
  Sem
    '[ DomainRegistrationStore,
       DomainVerificationChallengeStore,
       (Error EnterpriseLoginSubsystemError),
       (Error ParseException),
       GalleyAPIAccess,
       SparAPIAccess,
       TinyLog,
       (Input EnterpriseLoginSubsystemConfig),
       EmailSending,
       Random,
       Rpc,
       UserKeyStore,
       UserSubsystem
     ]
    a ->
  Either EnterpriseLoginSubsystemError a
runDependencies =
  run
    . userSubsystemTestInterpreter []
    . (evalState mempty . inMemoryUserKeyStoreInterpreter . raiseUnder)
    . fakeRpc
    . runRandomPure
    . noopEmailSendingInterpreter
    . runInputConst
      ( EnterpriseLoginSubsystemConfig
          Nothing
          (error "undefined wire-server-enterprise endpoint")
      )
    . discardTinyLogs
    . emptySparAPIAccess
    . miniGalleyAPIAccess Nothing def
    . runErrorUnsafe
    . runError
    . (evalState mempty . inMemoryDomainVerificationChallengeStoreInterpreter . raiseUnder)
    . (evalState mempty . inMemoryDomainRegistrationStoreInterpreter . raiseUnder)

fakeRpc :: InterpreterFor Rpc r
fakeRpc = interpret $ \case
  Rpc {} -> error "Rpc not implemented"
  RpcWithRetries {} -> error "RpcWithRetries not implemented"

spec :: Spec
spec = describe "EnterpriseLoginSubsystem" $ do
  it "LockDomain" pending
  it "UnlockDomain" pending
  it "PreAuthorizeDomain" pending
  it "UnAuthorizeDomain" pending
  it "UpdateDomainRegistration" pending
  it "DeleteDomain" pending
