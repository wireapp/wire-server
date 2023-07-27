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

module Test.Wire.API.Call.Config where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck hiding (total)
import Wire.API.Call.Config (RTCConfiguration, TurnURI, isTcp, isTls, isUdp, limitServers)
import Wire.Arbitrary ()

tests :: TestTree
tests =
  testGroup
    "TURN"
    [ testProperty "TurnURI: decode . encode = id" turnURIid,
      testProperty "limitServers/lengthMax" lengthMaxProp,
      testProperty "limitServers/fairness udp" (fairnessProp isUdp),
      testProperty "limitServers/fairness tls" (fairnessProp isTls),
      testProperty "limitServers/fairness tcp" (fairnessProp isTcp),
      testProperty "limitServers/udpPriority" udpPriority,
      testProperty "RTCConfiguration/toJson: sftServersAreNeverNull" sftServersAreNeverNull
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
  total < expected_min || returned >= expected_min

udpPriority :: [TurnURI] -> Bool
udpPriority uris = do
  let totalUdp = length (filter isUdp uris)
      returnedUdp = length (filter isUdp (limitServers uris 4))
  totalUdp < 2 || returnedUdp >= 2

sftServersAreNeverNull :: RTCConfiguration -> Bool
sftServersAreNeverNull cfg = case toJSON cfg of
  Object o -> KeyMap.lookup "sft_servers" o /= Just Null
  v -> error . show $ "type mismatch, expected RTCConfiguration to be Object, but got: " <> encode v

newtype ZeroToTen = ZeroToTen Int
  deriving (Eq, Show)

instance Arbitrary ZeroToTen where
  arbitrary = ZeroToTen <$> chooseInt (0, 10)
