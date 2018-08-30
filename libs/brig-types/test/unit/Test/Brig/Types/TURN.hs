{-# LANGUAGE OverloadedStrings #-}

module Test.Brig.Types.TURN where

import Brig.Types.TURN hiding (turnURI)
import Data.Aeson
import Test.Brig.Types.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "TURN"
    [ testProperty "TurnURI: decode . encode = id" turnURIid
    , testProperty "limit/lengthMax" lengthMaxProp
    , testProperty "limit/udpPriority" udpPriority
    ]

turnURIid :: TurnURI -> Property
turnURIid t = Just t === (decode . encode) t

lengthMaxProp :: (ZeroToTen, [TurnURI]) -> Property
lengthMaxProp (ZeroToTen len, uris) = length (limitServers uris len) === min len (length uris)

udpPriority :: [TurnURI] -> Bool
udpPriority uris = do
    let len = length (filter isUdp uris)
    if len > 1 then
        length (filter isUdp (limitServers uris 4)) >= 2
    else
        True


newtype ZeroToTen = ZeroToTen Int
  deriving (Eq, Show)

instance Arbitrary ZeroToTen where
  arbitrary = ZeroToTen <$> choose (0, 10)
