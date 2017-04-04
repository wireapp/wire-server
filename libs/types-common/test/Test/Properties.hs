{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Properties (tests) where

import Data.ByteString.Conversion
import Data.Monoid
import Data.Text.Ascii
import Data.Id
import Data.ProtocolBuffers.Internal
import Data.Serialize
import Data.UUID
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Ascii       as Ascii

tests :: TestTree
tests = testGroup "Properties"
    [ testGroup "Ascii"
        [ testProperty "validate (toText x) == Right x" $
            \(a :: Ascii) -> Ascii.validate (Ascii.toText a) == Right a
        , testProperty "unsafeFromByteString (toByteString x) == x" $
            \(a :: Ascii) -> Ascii.unsafeFromByteString (toByteString' a) == a
        , testProperty "validate (toText x <> \"𝄞\") /= Right x" $
            \(a :: Ascii) -> Ascii.validate (Ascii.toText a <> "𝄞") /= Right a
        ]

    , testGroup "Ascii (Printable)"
        [ testProperty "contains Printable c ==> contains Standard c" $
            \(c :: Char) -> Ascii.contains Ascii.Printable c ==> Ascii.contains Ascii.Standard c
        ]

    , testGroup "Ascii (Base16)"
        [ testProperty "validate (toText (encode x)) == Right (encode x)" $
            \(s :: String) ->
                let a = Ascii.encodeBase16 (C8.pack s)
                in Ascii.validate (Ascii.toText a) == Right a
        , testProperty "decode . encode = id" $
            \(s :: String) ->
                let bs = C8.pack s
                in Ascii.decodeBase16 (Ascii.encodeBase16 bs) == Just bs
        , testProperty "contains Base16 c ==> contains Standard c" $
            \(c :: Char) -> Ascii.contains Ascii.Base16 c ==> Ascii.contains Ascii.Standard c
        ]

    , testGroup "Ascii (Base64)"
        [ testProperty "validate (toText (encode x)) == Right (encode x)" $
            \(s :: String) ->
                let a = Ascii.encodeBase64 (C8.pack s)
                in Ascii.validate (Ascii.toText a) == Right a
        , testProperty "decode . encode = id" $
            \(s :: String) ->
                let bs = C8.pack s
                in Ascii.decodeBase64 (Ascii.encodeBase64 bs) == Just bs
        , testProperty "contains Base64 c ==> contains Standard c" $
            \(c :: Char) -> Ascii.contains Ascii.Base64 c ==> Ascii.contains Ascii.Standard c
        ]

    , testGroup "Ascii (Base64Url)"
        [ testProperty "validate (toText (encode x)) == Right (encode x)" $
            \(s :: String) ->
                let a = Ascii.encodeBase64Url (C8.pack s)
                in Ascii.validate (Ascii.toText a) == Right a
        , testProperty "decode . encode = id" $
            \(s :: String) ->
                let bs = C8.pack s
                in Ascii.decodeBase64Url (Ascii.encodeBase64Url bs) == Just bs
        , testProperty "contains Base64Url c ==> contains Standard c" $
            \(c :: Char) -> Ascii.contains Ascii.Base64Url c ==> Ascii.contains Ascii.Standard c
        ]

    , testGroup "UUID"
        [ testProperty "decode . encode = id" $
              \t (x :: UUID) -> roundtrip t x === Right x
        ]
    , testGroup "ClientId"
        [ testProperty "decode . encode = id" $
              \t (x :: ClientId) -> roundtrip t x === Right x
        ]
    , testGroup "AssetId"
        [ testProperty "decode . encode = id" $
              \t (x :: AssetId) -> roundtrip t x === Right x
        ]
    , testGroup "ConvId"
        [ testProperty "decode . encode = id" $
              \t (x :: ConvId) -> roundtrip t x === Right x
        ]
    , testGroup "InvitationId"
        [ testProperty "decode . encode = id" $
              \t (x :: InvitationId) -> roundtrip t x === Right x
        ]
    , testGroup "UserId"
        [ testProperty "decode . encode = id" $
              \t (x :: UserId) -> roundtrip t x === Right x
        ]
    , testGroup "Id NoId"
        [ testProperty "decode . encode = id" $
              \t (x :: Id NoId) -> roundtrip t x === Right x
        ]
    ]

roundtrip :: (EncodeWire a, DecodeWire a) => Tag' -> a -> Either String a
roundtrip (Tag' t) = runGet (getWireField >>= decodeWire) . runPut . encodeWire t

newtype Tag' = Tag' Tag
    deriving (Eq, Show)

instance Arbitrary Tag' where
    arbitrary = Tag' <$> choose (0, 536870912)
