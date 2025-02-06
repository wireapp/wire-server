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

module Test.Wire.API.Roundtrip.Misc (tests) where

import Data.Default
import Data.Domain
import Imports
import Test.Tasty qualified as T
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (===))
import Wire.API.EnterpriseLogin

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (6_000_000) "6s") . T.testGroup "MISC roundtrip tests" $
    [ T.testGroup "DomainRegistration{,Row}" $
        [ testCase "default values match" $ do
            let Right dom = mkDomain "example.com"
            Right (def dom :: DomainRegistration)
              @?= domainRegistrationFromRow (def dom)
            (def dom :: DomainRegistrationRow)
              @?= domainRegistrationToRow (def dom),
          testProperty "to, from row" $ \new -> do
            let row = domainRegistrationToRow new
                reRow = domainRegistrationToRow (domainRegistrationFromRow row)
            domainRegistrationFromRow row === Right new
            row === reRow
        ]
    ]
