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

module Test.Wire.API.User.Search where

import Data.Aeson (encode, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Imports
import Test.Tasty qualified as T
import Test.Tasty.QuickCheck (counterexample, testProperty)
import Wire.API.User.Search (Contact)

tests :: T.TestTree
tests = T.testGroup "Search" [searchResultBackwardsCompatibility]

searchResultBackwardsCompatibility :: T.TestTree
searchResultBackwardsCompatibility =
  testProperty
    "Contact always contains id"
    $ \(c :: Contact) ->
      let prop =
            case toJSON c of
              Aeson.Object o -> "id" `elem` KeyMap.keys o
              _ -> False
       in counterexample ("This json doesn't contain 'id': \n" <> cs (encode c)) prop
