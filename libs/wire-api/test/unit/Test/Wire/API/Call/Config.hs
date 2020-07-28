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

module Test.Wire.API.Call.Config where

import Data.Aeson
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck hiding (total)
import Wire.API.Arbitrary ()
import Wire.API.Call.Config (TurnURI, isTcp, isTls, isUdp, limitServers)

tests :: TestTree
tests =
  testGroup
    "TURN"
    [ testProperty "TurnURI: decode . encode = id" turnURIid,
      testProperty "limitServers/lengthMax" lengthMaxProp,
      testProperty "limitServers/fairness udp" (fairnessProp isUdp),
      testProperty "limitServers/fairness tls" (fairnessProp isTls),
      testProperty "limitServers/fairness tcp" (fairnessProp isTcp),
      testProperty "limitServers/udpPriority" udpPriority
    ]

turnURIid :: TurnURI -> Property
turnURIid t = Just t === (decode . encode) t

lengthMaxProp :: (ZeroToTen, [TurnURI]) -> Property
lengthMaxProp (ZeroToTen len, uris) = length (limitServers uris len) === min len (length uris)

-- example input:
--  predicate = isTls
--  limit = 4
--  uris  = [udp/tcp/tcp/tcp/tls]
-- => ensure at least one TLS in output
fairnessProp :: (TurnURI -> Bool) -> (ZeroToTen, [TurnURI]) -> Bool
fairnessProp predicate (ZeroToTen len, uris) = do
  let total = length (filter predicate uris)
      returned = length (filter predicate (limitServers uris len))
      expected_min = len `div` 3 -- 3 possible predicates
  if total >= expected_min
    then returned >= expected_min
    else True

udpPriority :: [TurnURI] -> Bool
udpPriority uris = do
  let totalUdp = length (filter isUdp uris)
      returnedUdp = length (filter isUdp (limitServers uris 4))
  if totalUdp >= 2
    then returnedUdp >= 2
    else True

newtype ZeroToTen = ZeroToTen Int
  deriving (Eq, Show)

instance Arbitrary ZeroToTen where
  arbitrary = ZeroToTen <$> chooseInt (0, 10)
