{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Native where

import Imports

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Aeson
import Data.Id (UserId, randomId, ConnId (..), ClientId (..))
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Gundeck.Push.Native.Crypto
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types
import Network.AWS (Region (Ireland))
import OpenSSL.Cipher

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Char8   as C
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.List1              as List1
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified OpenSSL.EVP.Cipher      as EVP
import qualified OpenSSL.EVP.Digest      as EVP

tests :: TestTree
tests = testGroup "Native"
    [ testProperty "serialise/ok" $
        -- this may fail sporadically, but that's not a production issue.
        -- see <https://github.com/wireapp/wire-server/issues/341>.
        forAll genTransport serialiseOkProp
    , testProperty "serialise/size-limit" $
        forAll genTransport sizeLimitProp
    , testProperty "crypto/block-size-16" $
        forAll ((,) <$> genKeys <*> genPlaintext) blockSizeProp
    , testProperty "crypto/aes-decrypt" $
        forAll ((,) <$> genKeys <*> genPlaintext) decryptProp
    , testProperty "crypto/encrypt-then-mac" $
        forAll ((,) <$> genKeys <*> genPlaintext) macProp
    ]

serialiseOkProp :: Transport -> Property
serialiseOkProp t = ioProperty $ do
    a <- mkAddress t
    n <- randNotif (0, 1280)
    c <- aes256
    d <- sha256
    let m = randMessage n
    r <- serialise m a
    let sn = either (const Nothing) Just r >>= decode' . LT.encodeUtf8
    let equalTransport = fmap snsNotifTransport sn == Just t
    equalNotif <- case snsNotifBundle <$> sn of
        Nothing                -> return False
        Just (NoticeBundle n') -> return $ ntfId n == n'
        Just (PlainBundle   p) -> return $ n == plainNotif p
        Just (CipherBundle  p) -> do
            let (iv, dat') = BS.splitAt 16 (cipherData p)
            let mac = EVP.hmacBS d (macKeyBytes macKey) (cipherData p)
            plain <- EVP.cipherBS c (encKeyBytes encKey) iv EVP.Decrypt dat'
            let n' = plainNotif <$> decodeStrict' plain
            return $ n' == Just n && mac == cipherMac p
    let debugInfo = (t, a, n, {- c, d, m, -} r, sn, equalTransport, equalNotif)
    return . counterexample (show debugInfo) $ equalTransport && equalNotif

sizeLimitProp :: Transport -> Property
sizeLimitProp t = ioProperty $ do
    a <- mkAddress t
    let lim = fromIntegral (maxPayloadSize t)
    n <- randNotif (lim + 1, lim + 10000)
    let m = randMessage n
    isRight <$> serialise m a

blockSizeProp :: (TestKeys, ByteString) -> Property
blockSizeProp (TestKeys keys, input) = ioProperty $ do
    sha <- sha256
    aes <- aes256
    cipher <- encrypt input keys aes sha
    return $ BS.length (cipherData cipher) `mod` 16 == 0

decryptProp :: (TestKeys, ByteString) -> Property
decryptProp (TestKeys keys, input) = ioProperty $ do
    sha <- sha256
    aes <- aes256
    cipher <- encrypt input keys aes sha
    let (iv, ciphertext) = BS.splitAt 16 (cipherData cipher)
    ctx    <- newAESCtx Decrypt (encKeyBytes (sigEncKey keys)) iv
    padded <- aesCBC ctx ciphertext
    let (plain, pad) = BS.splitAt (fromIntegral (BS.length input)) padded
    let expected     = BS.replicate (BS.length pad) (fromIntegral (BS.length pad))
    return $ conjoin
        [ counterexample ("Actual padding: " ++ C.unpack pad) $
          counterexample ("Expected padding: " ++ C.unpack expected) $
            pad == expected
        , counterexample ("Actual plaintext: " ++ C.unpack plain) $
            plain == input
        ]

macProp :: (TestKeys, ByteString) -> Property
macProp (TestKeys keys, input) = ioProperty $ do
    sha    <- sha256
    aes    <- aes256
    cipher <- encrypt input keys aes sha
    let mac = EVP.hmacBS sha (macKeyBytes (sigMacKey keys)) (cipherData cipher)
    return $ mac == cipherMac cipher

-----------------------------------------------------------------------------
-- Types

data SnsNotification = SnsNotification
    { snsNotifTransport :: !Transport
    , snsNotifData      :: !SnsData
    } deriving (Eq, Show)

instance FromJSON SnsNotification where
    parseJSON = withObject "SnsNotification" $ \o ->
        case HashMap.toList o of
            [("GCM",               String n)] -> parseGcm n
            [("APNS",              String n)] -> parseApns APNS n
            [("APNS_SANDBOX",      String n)] -> parseApns APNSSandbox n
            [("APNS_VOIP",         String n)] -> parseApns APNSVoIP n
            [("APNS_VOIP_SANDBOX", String n)] -> parseApns APNSVoIPSandbox n
            _                                 -> mempty
      where
        parseApns t n = let apn = decodeStrict' (T.encodeUtf8 n) in
            maybe mempty (pure . SnsNotification t . SnsApnsData) apn

        parseGcm n = let gcm = decodeStrict' (T.encodeUtf8 n) in
            maybe mempty (pure . SnsNotification GCM . SnsGcmData) gcm

data SnsData
    = SnsGcmData  !GcmData
    | SnsApnsData !ApnsData
    deriving (Eq, Show)

snsNotifBundle :: SnsNotification -> Bundle
snsNotifBundle n = case snsNotifData n of
    SnsGcmData  d -> gcmBundle  d
    SnsApnsData d -> apnsBundle d

data GcmData = GcmData
    { gcmPriority :: !Text
    , gcmBundle   :: !Bundle
    } deriving (Eq, Show)

instance FromJSON GcmData where
    parseJSON = withObject "GcmData" $ \o ->
        GcmData <$> o .: "priority"
                <*> o .: "data"

data ApnsData = ApnsData
    { apnsMeta   :: !Object
    , apnsBundle :: !Bundle
    } deriving (Eq, Show)

instance FromJSON ApnsData where
    parseJSON = withObject "ApnsData" $ \o ->
        ApnsData <$> o .: "aps"
                 <*> o .: "data"

data Bundle
    = PlainBundle  !PlainData
    | CipherBundle !CipherData
    | NoticeBundle !NotificationId
    deriving (Eq, Show)

instance FromJSON Bundle where
    parseJSON = withObject "Bundle" $ \o ->
        case HashMap.lookup "type" o of
            Just (String  "plain") -> PlainBundle <$> parseJSON (Object o)
            Just (String "cipher") -> do
                mac <- B64.decodeLenient . T.encodeUtf8 <$> o .: "mac"
                dat <- B64.decodeLenient . T.encodeUtf8 <$> o .: "data"
                return $! CipherBundle $! CipherData mac dat
            Just (String "notice") -> case HashMap.lookup "data" o of
                Just (Object o') -> NoticeBundle <$> o' .: "id"
                _                -> mempty
            _ -> mempty

data PlainData = PlainData
    { plainNotif :: !Notification
    , plainUser  :: !(Maybe UserId)
    } deriving (Eq, Show)

instance FromJSON PlainData where
    parseJSON = withObject "PlainData" $ \o ->
        PlainData <$> o .: "data" <*> o .:? "user"

-----------------------------------------------------------------------------
-- Randomness

genTransport :: Gen Transport
genTransport = elements [minBound..]

randNotif :: (Int, Int) -> IO Notification
randNotif size = do
    i <- randomId
    generate $ do
        l <- choose size
        v <- T.pack <$> vectorOf l (elements ['a'..'z'])
        let pload = List1.singleton (HashMap.fromList ["data" .= v])
        Notification i <$> arbitrary <*> pure pload

randMessage :: Notification -> Message "keys"
randMessage n = Notice (ntfId n) HighPriority Nothing

genKeys :: Gen TestKeys
genKeys = TestKeys <$> (SignalingKeys <$> (EncKey <$> genKey) <*> (MacKey <$> genKey))

genPlaintext :: Gen ByteString
genPlaintext = BS.pack <$> arbitrary

genKey :: Gen BS.ByteString
genKey = BS.pack . take 32 <$> arbitrary `suchThat` ((>= 32) . length)

newtype TestKeys = TestKeys SignalingKeys

instance Show TestKeys where
    show (TestKeys (SignalingKeys (EncKey ek) (MacKey mk))) =
        show ek ++ ":" ++ show mk

-----------------------------------------------------------------------------
-- Utilities

mkAddress :: Transport -> IO (Address "keys")
mkAddress t = Address
    <$> randomId
    <*> pure t
    <*> pure (AppName "test")
    <*> pure (Token "test")
    <*> pure (mkEndpoint t (AppName "test"))
    <*> pure (ConnId "conn")
    <*> pure (ClientId "client")
    <*> pure (Just (SignalingKeys encKey macKey))
    <*> pure Nothing

mkEndpoint :: Transport -> AppName -> EndpointArn
mkEndpoint t a = mkSnsArn Ireland (Account "test") topic
  where
    topic = mkEndpointTopic (ArnEnv "test") t a (EndpointId "test")

encKey :: EncKey
encKey = EncKey (BS.replicate 32 0)

macKey :: MacKey
macKey = MacKey (BS.replicate 32 0)

sha256 :: IO EVP.Digest
sha256 = maybe (error "SHA256 not found") return
     =<< EVP.getDigestByName "SHA256"

aes256 :: IO EVP.Cipher
aes256 = maybe (error "AES256 not found") return
     =<< EVP.getCipherByName "AES-256-CBC"
