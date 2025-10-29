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

module Test.Brig.InternalNotification where

import Data.Aeson qualified as A
import Data.ByteString.Lazy as BSL
import Data.Id (clientToText)
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.InternalEvent (InternalNotification (..))

tests :: TestTree
tests =
  testGroup
    "InternalNotification"
    [testCase "Golden test for old format of DeleteClient" checkGolden]

checkGolden :: IO ()
checkGolden = do
  -- This file was generated from ToJSON of the format prior to 67993ab1
  ns <- BSL.readFile "../../libs/wire-subsystems/test/resources/internal-notification.json"
  let eith = A.eitherDecode @InternalNotification ns
  case eith of
    Left err -> assertFailure ("Could not parse InternalNotification: " <> show err)
    Right n ->
      case n of
        DeleteClient cid _uid _mcon ->
          clientToText cid @?= "1"
        _ -> assertFailure "Unexpected value"
