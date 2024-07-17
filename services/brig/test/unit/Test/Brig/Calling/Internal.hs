{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module Test.Brig.Calling.Internal where

import Brig.Calling.Internal
import Data.Misc (mkHttpsUrl)
import Data.Text qualified as T
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import URI.ByteString.QQ as URI
import Wire.API.Call.Config (sftServer)
import Wire.Network.DNS.SRV (SrvTarget (SrvTarget))

tests :: TestTree
tests =
  testGroup "Calling.API" $
    [ testGroup "sftServerFromSrvTarget" $
        [ testCase "when srvTarget ends with a dot" $ do
            let Right expectedServer = sftServer <$> mkHttpsUrl [URI.uri|https://sft1.env.example.com:9364|]
            assertEqual
              "the dot should be stripped from sft server"
              expectedServer
              (sftServerFromSrvTarget $ SrvTarget "sft1.env.example.com." 9364),
          testCase "when srvTarget doesn't end with a dot" $ do
            let Right expectedServer = sftServer <$> mkHttpsUrl [URI.uri|https://sft2.env.example.com:443|]
            assertEqual
              "the dot should be stripped from sft server"
              expectedServer
              (sftServerFromSrvTarget $ SrvTarget "sft2.env.example.com" 443)
        ],
      testCase "base26" $ do
        "a" @=? base26 0
        "ba" @=? base26 26
        "cfox" @=? base26 38919,
      testProperty "base26 . unbase26 === id" $ \(Base26 s) -> base26 (unbase26 s) === s,
      testProperty "unbase26 . base26 === id" $ \(NonNegative n) -> unbase26 (base26 n) === n
    ]

newtype Base26 = Base26 Text
  deriving (Eq, Show)

mkBase26 :: String -> Base26
mkBase26 s = Base26 $ case dropWhile (== 'a') s of
  "" -> "a"
  str -> T.pack str

instance Arbitrary Base26 where
  arbitrary =
    mkBase26 <$> listOf1 (fmap chr (chooseInt (ord 'a', ord 'z')))

unbase26 :: Text -> Integer
unbase26 = foldl' (\v c -> fromIntegral (ord c - ord 'a') + v * 26) 0 . T.unpack
