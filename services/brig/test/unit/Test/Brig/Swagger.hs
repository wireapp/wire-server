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

module Test.Brig.Swagger (tests) where

import Brig.API.Public (DocsAPI, docsAPI)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Imports
import Servant.Client (client, runClientM)
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection (typeRep)

tests :: TestTree
tests =
  localOption (Timeout (60 * 1000000) "60s") . testGroup "Swagger" $
    [ testCase "no internal routing ID duplicates" $ do
        pure ()
    ]

{- TODO:

- write clients for all swagger end-points
- use hspec-wai to run the swagger end-points so we can call them in a unit test
- call all the swagger.json ones and string-concatenate the responses
- grep responses for internal route IDs, and return a list
- assert (list == nub list)
- it's slightly more complicated: V1 and V2 often contain the same end-point with the same route ID.  just check for every version separately?

-}

position :<|> hello :<|> marketing = client (Proxy @DocsAPI)
