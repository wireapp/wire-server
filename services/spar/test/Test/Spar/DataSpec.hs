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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Spar.DataSpec where

import Data.Time
import Imports
import SAML2.WebSSO (Time (Time), addTime)
import Spar.Data
import Test.Hspec
import Wire.API.User.Saml

spec :: Spec
spec = do
  describe "mkTTL" $ do
    check 1 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:15Z" (Right 15)
    check 2 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:40Z" (Left (TTLTooLong "TTL:authresp:40" "TTL:authresp:30"))
    check 3 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:00Z" (Left (TTLNegative "TTL:authresp:0"))
    check 4 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T08:30:00Z" (Left (TTLNegative "TTL:authresp:-1800"))
  describe "ttlToNominalDiffTime" $ do
    it "" $ do
      addTime (ttlToNominalDiffTime $ TTL 3) (Time $ parsetm "1924-07-14T08:30:00Z")
        `shouldBe` Time (parsetm "1924-07-14T08:30:03Z")

check :: (HasCallStack) => Int -> Env -> String -> Either TTLError (TTL "authresp") -> Spec
check testnumber env (parsetm -> endOfLife) expectttl =
  it (show testnumber) $ mkTTLAssertions env endOfLife `shouldBe` expectttl

parsetm :: (HasCallStack) => String -> UTCTime
parsetm = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

{-# HLINT ignore "Eta reduce" #-}
-- For clarity
mkDataEnv :: (HasCallStack) => String -> TTL "authresp" -> Env
mkDataEnv now maxttl =
  Env
    (parsetm now)
    0 -- will not be looked at
    maxttl -- this one will
