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

module API.User.PasswordReset
  ( tests,
  )
where

import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.User.Auth hiding (user)
import Data.Misc (PlainTextPassword (..))
import Imports
import Test.Tasty hiding (Timeout)
import Util

tests :: ConnectionLimit -> Opt.Timeout -> Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at _conf p b _c _g =
  testGroup
    "password-reset"
    [ test p "post /password-reset[/complete] - 201[/200]" $ testPasswordReset b,
      test p "post /password-reset & put /self/email - 400" $ testPasswordResetAfterEmailUpdate b
    ]

testPasswordReset :: Brig -> Http ()
testPasswordReset brig = do
  u <- randomUser brig
  let Just email = userEmail u
  let uid = userId u
  -- initiate reset
  let newpw = PlainTextPassword "newsecret"
  do
    initiatePasswordReset brig email !!! const 201 === statusCode
    passwordResetData <- preparePasswordReset brig email uid newpw
    completePasswordReset brig passwordResetData !!! const 200 === statusCode
  -- try login
  login brig (defEmailLogin email) PersistentCookie
    !!! const 403 === statusCode
  login brig (PasswordLogin (LoginByEmail email) newpw Nothing) PersistentCookie
    !!! const 200 === statusCode
  -- reset password again to the same new password, get 400 "must be different"
  do
    initiatePasswordReset brig email !!! const 201 === statusCode
    passwordResetData <- preparePasswordReset brig email uid newpw
    completePasswordReset brig passwordResetData !!! const 409 === statusCode

testPasswordResetAfterEmailUpdate :: Brig -> Http ()
testPasswordResetAfterEmailUpdate brig = do
  u <- randomUser brig
  let uid = userId u
  let Just email = userEmail u
  eml <- randomEmail
  initiateEmailUpdate brig eml uid !!! const 202 === statusCode
  initiatePasswordReset brig email !!! const 201 === statusCode
  passwordResetData <- preparePasswordReset brig email uid (PlainTextPassword "newsecret")
  -- activate new email
  activateEmail brig eml
  checkEmail brig uid eml
  -- attempting to complete password reset should fail
  completePasswordReset brig passwordResetData !!! const 400 === statusCode
