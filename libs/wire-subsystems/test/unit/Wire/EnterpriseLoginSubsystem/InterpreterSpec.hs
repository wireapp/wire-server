-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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
    . miniGalleyAPIAccess mempty def
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
