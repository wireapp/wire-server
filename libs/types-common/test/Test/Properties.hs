{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Test.Properties
  ( tests,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.ByteString.Lazy as L
import Data.Handle (Handle)
import Data.Id
import qualified Data.Json.Util as Util
import Data.ProtocolBuffers.Internal
import Data.Serialize
import Data.Text.Ascii
import qualified Data.Text.Ascii as Ascii
import Data.Time
import Data.Time.Clock.POSIX
import Data.UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)

tests :: TestTree
tests =
  testGroup
    "Properties"
    [ testGroup
        "Ascii"
        [ testProperty "validate (toText x) == Right x" $
            \(a :: Ascii) -> Ascii.validate (Ascii.toText a) == Right a,
          testProperty "unsafeFromByteString (toByteString x) == x" $
            \(encodeBase64 -> a) -> Ascii.unsafeFromByteString (toByteString' a) == a,
          -- (unsafeFromByteString occasionally fails on Ascii strings)
          testProperty "validate (toText x <> \"𝄞\") /= Right x" $
            \(a :: Ascii) -> Ascii.validate (Ascii.toText a <> "𝄞") /= Right a
        ],
      testGroup
        "Ascii (Printable)"
        [ testProperty "contains Printable c ==> contains Standard c" $
            \(c :: Char) -> Ascii.contains Ascii.Printable c ==> Ascii.contains Ascii.Standard c
        ],
      testGroup
        "Ascii (Base16)"
        [ testProperty "validate (toText (encode x)) == Right (encode x)" $
            \(s :: String) ->
              let a = Ascii.encodeBase16 (C8.pack s)
               in Ascii.validate (Ascii.toText a) == Right a,
          testProperty "decode . encode = id" $
            \(s :: String) ->
              let bs = C8.pack s
               in Ascii.decodeBase16 (Ascii.encodeBase16 bs) == Just bs,
          testProperty "contains Base16 c ==> contains Standard c" $
            \(c :: Char) -> Ascii.contains Ascii.Base16 c ==> Ascii.contains Ascii.Standard c
        ],
      testGroup
        "Ascii (Base64)"
        [ testProperty "validate (toText (encode x)) == Right (encode x)" $
            \(s :: String) ->
              let a = Ascii.encodeBase64 (C8.pack s)
               in Ascii.validate (Ascii.toText a) == Right a,
          testProperty "decode . encode = id" $
            \(s :: String) ->
              let bs = C8.pack s
               in Ascii.decodeBase64 (Ascii.encodeBase64 bs) == Just bs,
          testProperty "contains Base64 c ==> contains Standard c" $
            \(c :: Char) -> Ascii.contains Ascii.Base64 c ==> Ascii.contains Ascii.Standard c
        ],
      testGroup
        "Ascii (Base64Url)"
        [ testProperty "validate (toText (encode x)) == Right (encode x)" $
            \(s :: String) ->
              let a = Ascii.encodeBase64Url (C8.pack s)
               in Ascii.validate (Ascii.toText a) == Right a,
          testProperty "decode . encode = id" $
            \(s :: String) ->
              let bs = C8.pack s
               in Ascii.decodeBase64Url (Ascii.encodeBase64Url bs) == Just bs,
          testProperty "contains Base64Url c ==> contains Standard c" $
            \(c :: Char) -> Ascii.contains Ascii.Base64Url c ==> Ascii.contains Ascii.Standard c
        ],
      testGroup
        "Base64ByteString"
        [ testProperty "validate (Aeson.decode . Aeson.encode) == pure . id" $
            \(Util.Base64ByteString . L.pack -> s) ->
              (Aeson.eitherDecode . Aeson.encode) s == Right s,
          -- the property only considers valid 'String's, and it does not document the encoding very
          -- well, so here are some unit tests (see
          -- http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt for more).
          testCase "examples" $ do
            let go :: Util.Base64ByteString -> L.ByteString -> Assertion
                go b uu = do
                  Aeson.encode b @=? uu
                  (Aeson.eitherDecode . Aeson.encode) b @=? Right b
            go "" "\"\""
            go "foo" "\"Zm9v\""
        ],
      testGroup
        "UTCTimeMillis"
        [ testProperty "validate (Aeson.decode . Aeson.encode) == pure . id" $
            \(t :: Util.UTCTimeMillis) ->
              (Aeson.eitherDecode . Aeson.encode) t == Right t,
          -- (we could test @show x == show y ==> x == y@, but that kind of follows from the above.)

          let toUTCTimeMillisSlow :: HasCallStack => UTCTime -> Maybe UTCTime
              toUTCTimeMillisSlow t = parseExact formatRounded
                where
                  parseExact = parseTimeM True defaultTimeLocale "%FT%T%QZ"
                  formatRounded = formatTime defaultTimeLocale format t
                  format = "%FT%T." ++ formatMillis t ++ "Z"
                  formatMillis = Imports.take 3 . formatTime defaultTimeLocale "%q"
           in testProperty "toUTCTimeMillis" $
                \(t :: UTCTime) ->
                  Just (Util.fromUTCTimeMillis $ Util.toUTCTimeMillis t) == toUTCTimeMillisSlow t,
          let testcase (t1, t2) = testCase (show (t1, t2)) $ make t1 @=? make t2
              make = Util.readUTCTimeMillis
           in testGroup "validate Eq" $
                testcase
                  <$> [ ("1918-04-14T09:58:58.457Z", "1918-04-14T09:58:58.457Z"),
                        ("1918-04-14T09:58:58.4574Z", "1918-04-14T09:58:58.457Z"),
                        ("1918-04-14T09:58:58.4579Z", "1918-04-14T09:58:58.457Z")
                      ],
          let testcase (t1, t2) = testCase (show (t1, t2)) $ process t1 @=? Just t2
              process = fmap show . Util.readUTCTimeMillis
           in testGroup "validate Eq" $
                testcase
                  <$> [ ("1918-04-14T09:58:58.457Z", "1918-04-14T09:58:58.457Z"),
                        ("1918-04-14T09:58:58.4574Z", "1918-04-14T09:58:58.457Z"),
                        ("1918-04-14T09:58:58.4579Z", "1918-04-14T09:58:58.457Z"),
                        -- client parsers require *exactly* three digits:
                        ("1918-04-14T09:58:58Z", "1918-04-14T09:58:58.000Z"),
                        ("1918-04-14T09:58:58.1Z", "1918-04-14T09:58:58.100Z"),
                        ("1918-04-14T09:58:58.12Z", "1918-04-14T09:58:58.120Z")
                      ]
        ],
      testGroup
        "Handle"
        [ jsonRoundtrip @Handle
        ],
      testGroup
        "UUID"
        [ testProperty "decode . encode = id" $
            \t (x :: UUID) -> roundtrip t x === Right x
        ],
      testGroup
        "ClientId"
        [ testProperty "decode . encode = id" $
            \t (x :: ClientId) -> roundtrip t x === Right x
        ],
      testGroup
        "AssetId"
        [ testProperty "decode . encode = id" $
            \t (x :: AssetId) -> roundtrip t x === Right x
        ],
      testGroup
        "ConvId"
        [ testProperty "decode . encode = id" $
            \t (x :: ConvId) -> roundtrip t x === Right x
        ],
      testGroup
        "InvitationId"
        [ testProperty "decode . encode = id" $
            \t (x :: InvitationId) -> roundtrip t x === Right x
        ],
      testGroup
        "UserId"
        [ testProperty "decode . encode = id" $
            \t (x :: UserId) -> roundtrip t x === Right x
        ],
      testGroup
        "Id NoId"
        [ testProperty "decode . encode = id" $
            \t (x :: Id NoId) -> roundtrip t x === Right x
        ]
    ]

roundtrip :: (EncodeWire a, DecodeWire a) => Tag' -> a -> Either String a
roundtrip (Tag' t) = runGet (getWireField >>= decodeWire) . runPut . encodeWire t

jsonRoundtrip ::
  forall a.
  (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
  TestTree
jsonRoundtrip = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (Aeson.parseEither parseJSON . toJSON) v

newtype Tag' = Tag' Tag
  deriving (Eq, Show)

instance Arbitrary Tag' where
  arbitrary = Tag' <$> choose (0, 536870912)

instance Arbitrary Util.UTCTimeMillis where
  arbitrary = Util.toUTCTimeMillis . posixSecondsToUTCTime . fromInteger <$> arbitrary
