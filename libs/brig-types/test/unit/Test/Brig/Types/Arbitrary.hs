{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Brig.Types.Arbitrary where

import Brig.Types.TURN
import Control.Applicative
import Data.IP
import Data.Misc
import Data.Word
import Test.QuickCheck

newtype Octet = Octet { octet :: Word16 }
    deriving (Eq, Show)

instance Arbitrary Octet where
    arbitrary = Octet <$> arbitrary `suchThat` (<256)

instance Arbitrary Scheme where
    arbitrary = elements [ SchemeTurn
                         , SchemeTurns
                         ]

-- TODO: Add an arbitrary instance for IPv6
instance Arbitrary IpAddr where
    arbitrary = ipV4Arbitrary -- <|> ipV4Arbitrary
      where
        ipV4Arbitrary :: Gen IpAddr
        ipV4Arbitrary = do
            a <- ipV4Part
            b <- ipV4Part
            c <- ipV4Part
            d <- ipV4Part
            let adr = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
            IpAddr . IPv4 <$> return (read adr)

        ipV4Part = octet <$> arbitrary

instance Arbitrary TurnHost where
    arbitrary = TurnHost <$> arbitrary

instance Arbitrary Port where
    arbitrary = Port <$> arbitrary

instance Arbitrary Transport where
    arbitrary = elements [ TransportUDP
                         , TransportTCP
                         ]

instance Arbitrary TurnURI where
    arbitrary = turnURI <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
