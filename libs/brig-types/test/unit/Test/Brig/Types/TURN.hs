{-# LANGUAGE OverloadedStrings #-}

module Test.Brig.Types.TURN where

import Imports
import Brig.Types.TURN hiding (turnURI)
import Data.Aeson
import Brig.Types.Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding (total)

tests :: TestTree
tests = testGroup "TURN"
    [ testProperty "TurnURI: decode . encode = id" turnURIid
    , testProperty "limitServers/lengthMax" lengthMaxProp
    , testProperty "limitServers/fairness udp" (fairnessProp isUdp)
    , testProperty "limitServers/fairness tls" (fairnessProp isTls)
    , testProperty "limitServers/fairness tcp" (fairnessProp isTcp)
    , testProperty "limitServers/udpPriority" udpPriority
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
    if total >= expected_min then
        returned >= expected_min
    else
        True

udpPriority :: [TurnURI] -> Bool
udpPriority uris = do
    let totalUdp = length (filter isUdp uris)
        returnedUdp = length (filter isUdp (limitServers uris 4))
    if totalUdp >= 2 then
        returnedUdp >= 2
    else
        True

newtype ZeroToTen = ZeroToTen Int
  deriving (Eq, Show)

instance Arbitrary ZeroToTen where
  arbitrary = ZeroToTen <$> choose (0, 10)
