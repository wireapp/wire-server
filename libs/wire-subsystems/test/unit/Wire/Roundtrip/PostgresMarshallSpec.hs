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

module Wire.Roundtrip.PostgresMarshallSpec (spec) where

import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.PostgresMarshall
import Wire.Arbitrary qualified as Arbitrary ()
import Wire.CodeStore.Scope

spec :: Spec
spec = do
  describe "Wire.API.PostgresMarshall roundtrip tests" $ do
    prop "Scope" (testRoundTrip @Int32 @Scope)

testRoundTrip ::
  forall db domain.
  ( PostgresMarshall db domain,
    PostgresUnmarshall db domain,
    Eq domain,
    Show domain
  ) =>
  domain -> Property
testRoundTrip value =
  let actual = postgresUnmarshall @db @domain . postgresMarshall @db @domain $ value
      expected = Right value
   in actual === expected
