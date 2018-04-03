{-# LANGUAGE OverloadedStrings #-}

module Test.Brig.Types.TURN where

import Brig.Types.TURN
import Data.Aeson
import Data.List (sort)
import Data.Misc
import System.Random.Shuffle
import Test.Brig.Types.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "TURN"
    [ testProperty "TurnURI: decode . encode = id" turnURIid
    , testGroup "Order"
        [ testCase "Turn order example" turnURIOrder
        ]
    ]

turnURIid :: TurnURI -> Property
turnURIid t = Just t === (decode . encode) t

turnURIOrder :: IO ()
turnURIOrder = go (100 :: Int)
  where
    go 0 = return ()
    go n = do
        shuffled <- shuffleM ordered
        assertEqual "Unexpected shuffle & sorting" (sort shuffled) ordered
        go (n - 1)

    -- Prefer servers with `SchemeTURN` and then `TransportUDP`
    ordered = [ turnURI SchemeTurn  host port Nothing
              , turnURI SchemeTurn  host port (Just TransportUDP)
              , turnURI SchemeTurn  host port (Just TransportTCP)
              , turnURI SchemeTurns host port Nothing
              , turnURI SchemeTurns host port (Just TransportUDP)
              , turnURI SchemeTurns host port (Just TransportTCP)
              ]

    host = TurnHost $ IpAddr $ read "127.0.0.1"
    port = Port 12345
