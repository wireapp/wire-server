{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Brig.Options qualified as Opt
import Cassandra qualified as DB
import Data.Aeson as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Misc
import Imports
import Test.Tasty hiding (Timeout)
import Util
import Util.Timeout
import Wire.API.User
import Wire.API.User.Auth

tests ::
  DB.ClientState ->
  ConnectionLimit ->
  Timeout ->
  Opt.Opts ->
  Manager ->
  Brig ->
  Cannon ->
  Galley ->
  TestTree
tests _cs _cl _at _conf p b _c _g =
  testGroup
    "password-reset"
    [ test p "post /password-reset[/complete] - 201[/200]" $ testPasswordReset b,
      test p "post /password-reset after put /access/self/email - 400" $ testPasswordResetAfterEmailUpdate b,
      test p "post /password-reset/complete - password too short - 400" $ testPasswordResetInvalidPasswordLength b
    ]

testPasswordReset :: Brig -> Http ()
testPasswordReset brig = do
  u <- randomUser brig
  let Just email = userEmail u
  let uid = userId u
  -- initiate reset
  let newpw = plainTextPassword8Unsafe "newsecret"
  do
    initiatePasswordReset brig email !!! const 201 === statusCode
    -- even though a password reset is now in progress
    -- we expect a successful response from a subsequent request to not leak any information
    -- about the requested email
    initiatePasswordReset brig email !!! const 201 === statusCode

    passwordResetData <- preparePasswordReset brig email uid newpw
    completePasswordReset brig passwordResetData !!! const 200 === statusCode
  -- try login
  login brig (defEmailLogin email) PersistentCookie
    !!! const 403 === statusCode
  login
    brig
    (MkLogin (LoginByEmail email) (plainTextPassword8To6 newpw) Nothing Nothing)
    PersistentCookie
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
  initiateEmailUpdateLogin brig eml (emailLogin email defPassword Nothing) uid !!! const 202 === statusCode
  initiatePasswordReset brig email !!! const 201 === statusCode
  passwordResetData <- preparePasswordReset brig email uid (plainTextPassword8Unsafe "newsecret")
  -- activate new email
  activateEmail brig eml
  checkEmail brig uid eml
  -- attempting to complete password reset should fail
  completePasswordReset brig passwordResetData !!! const 400 === statusCode

testPasswordResetInvalidPasswordLength :: Brig -> Http ()
testPasswordResetInvalidPasswordLength brig = do
  u <- randomUser brig
  let Just email = userEmail u
  let uid = userId u
  -- for convenience, we create a valid password first that we replace with an invalid one in the JSON later
  let newpw = plainTextPassword8Unsafe "newsecret"
  initiatePasswordReset brig email !!! const 201 === statusCode
  passwordResetData <- preparePasswordReset brig email uid newpw
  let shortPassword = String "123456"
  let reqBody = toJSON passwordResetData & addJsonKey "password" shortPassword
  postCompletePasswordReset reqBody !!! const 400 === statusCode
  where
    addJsonKey :: Key -> Value -> Value -> Object
    addJsonKey key val (Object xs) = KeyMap.insert key val xs
    addJsonKey _ _ _ = error "invalid JSON object"

    postCompletePasswordReset :: Object -> (MonadHttp m) => m ResponseLBS
    postCompletePasswordReset bdy =
      post
        ( brig
            . path "/password-reset/complete"
            . contentJson
            . body (RequestBodyLBS (encode bdy))
        )
