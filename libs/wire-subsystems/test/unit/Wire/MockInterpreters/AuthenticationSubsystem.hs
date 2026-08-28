-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.AuthenticationSubsystem where

import Imports
import Polysemy
import Polysemy.State
import Wire.AuthenticationSubsystem

data MockAuthenticationState = MockAuthenticationState
  { verificationCodeCalls :: Int,
    reAuthCalls :: Int,
    revokeCookiesCalls :: Int
  }
  deriving stock (Eq, Show)

emptyMockAuthenticationState :: MockAuthenticationState
emptyMockAuthenticationState =
  MockAuthenticationState
    { verificationCodeCalls = 0,
      reAuthCalls = 0,
      revokeCookiesCalls = 0
    }

mockAuthenticationSubsystemInterpreter ::
  (Member (State MockAuthenticationState) r) =>
  InterpreterFor AuthenticationSubsystem r
mockAuthenticationSubsystemInterpreter = interpret \case
  ReauthenticateEither {} -> do
    modify \st -> st {reAuthCalls = st.reAuthCalls + 1}
    pure $ Right ()
  RevokeCookies {} ->
    modify \st -> st {revokeCookiesCalls = st.revokeCookiesCalls + 1}
  EnforceVerificationCodeEither {} -> do
    modify \st -> st {verificationCodeCalls = st.verificationCodeCalls + 1}
    pure $ pure ()
  _ -> error "mockAuthenticationSubsystemInterpreter: implement on demand"
