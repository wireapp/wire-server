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

module Federation.User where

import Bilge (Manager)
import qualified Brig.Options as BrigOpts
-- import Control.Lens ((.~), (^.))
-- import qualified Data.Text as Text
-- import qualified Data.Text.Encoding as Text
import Imports
-- import qualified Network.HTTP.Client as HTTP
-- import qualified System.Logger.Class as Log
-- import System.Random as Random
import Test.Tasty
import Test.Tasty.HUnit
-- import URI.ByteString
import Util (Brig)
import Util.Options (Endpoint)

spec :: BrigOpts.Opts -> Manager -> Brig -> Endpoint -> IO TestTree
spec brigOpts mg brig federator =
  pure $
    testGroup
      "brig-federation-user"
      [ testCase "lookup user by qualified handle on remote backend" $ testHandleLookup brigOpts mg brig federator
      ]

testHandleLookup :: BrigOpts.Opts -> Manager -> Brig -> Endpoint -> Assertion
testHandleLookup _brigOpts _mg _brig _federator = do
  undefined
