{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Bot.Crypto
    ( Plaintext
    , Ciphertext

      -- * Session Basics (Double Ratchet)
    , botInitSession
    , clientInitSession
    , initSessionFromMsg
    , encrypt
    , decrypt

      -- * High-Level Operations (Double Ratchet)
    , encryptMessage
    , decryptMessage

      -- * Auxiliary Symmetric Encryption
    , SymmetricKeys
    , randomSymmetricKeys
    , encryptSymmetric
    , decryptSymmetric

      -- * Utilities
    , randomBytes
    ) where

import Imports
import Control.Monad.Catch
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error
import Crypto.Hash (digestFromByteString)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.ByteArray (convert)
import Data.ByteString.Conversion
import Data.Id
import Data.Serialize
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Wire.Bot.Clients
import Network.Wire.Bot.Crypto.Glue (unwrap, randomBytes)
import Network.Wire.Bot.Monad
import Network.Wire.Client.API.Client
import Network.Wire.Client.API.Conversation
import Network.Wire.Client.API.Push

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64
import qualified Data.Map.Strict            as Map
import qualified System.CryptoBox           as CBox

type Plaintext  = ByteString
type Ciphertext = ByteString

-----------------------------------------------------------------------------
-- Session Basics

-- | Initialise OTR sessions between all clients of the 'Bot' running
-- the 'BotSession' and all clients of the given user through fetching
-- prekeys.
-- initSessionsFromPreKeys?
botInitSession :: UserId -> BotSession ()
botInitSession uid = do
    clients <- getBotClients =<< getBot
    forM_ clients $ \cl -> clientInitSession cl uid

-- | Initialise OTR sessions between the given 'BotClient' and all clients
-- of the given user through fetching prekeys.
clientInitSession :: BotClient -> UserId -> BotSession ()
clientInitSession cl uid = do
    s <- mapM (newSession (botClientBox cl)) . prekeyClients =<< getUserPrekeys uid
    addSession (botClientSessions cl) uid (Map.fromList s)
  where
    newSession b c = do
        k <- decodePrekey c
        let i = mkSID uid (prekeyClient c)
        s <- liftIO $ unwrap =<< CBox.sessionFromPrekey b i k
        return (prekeyClient c, s)

-- | Initialise an OTR session between the given 'BotClient' and the sender of
-- the given OTR message.
initSessionFromMsg :: BotClient -> ConvEvent OtrMessage -> BotSession ByteString
initSessionFromMsg cl m = do
    let u = convEvtFrom m
    let o = convEvtData m
    let b = botClientBox cl
    let i = mkSID u (otrSender o)
    bytes  <- decodeBase64 (otrCiphertext o)
    (s, x) <- liftIO $ unwrap =<< CBox.sessionFromMessage b i bytes
    addSession (botClientSessions cl) u (Map.singleton (otrSender o) s)
    liftIO $ CBox.copyBytes x

-- | Encrypt an OTR message for all other clients in a given conversation.
encrypt :: BotClient -> ConvId -> ByteString -> BotSession OtrRecipients
encrypt cl cnv val = fmap (OtrRecipients . UserClientMap) .
    foldSessions (botClientSessions cl) cnv Map.empty $ \u c s rcps ->
        if botClientId cl == c then return rcps
        else liftIO $ do
            ciphertext <- do
                bs <- CBox.encrypt s val >>= unwrap >>= CBox.copyBytes
                return $! decodeUtf8 $! B64.encode bs
            return $ Map.insertWith Map.union u (Map.singleton c ciphertext) rcps

-- | Decrypt an OTR message received from a given user and client.
decrypt :: BotClient -> UserId -> ClientId -> ByteString -> BotSession ByteString
decrypt clt usr sid msg = do
    s  <- flip requireMaybe "Missing client session" =<< lookupSession (botClientSessions clt) usr sid
    liftIO $ CBox.copyBytes =<< unwrap =<< CBox.decrypt s msg

-----------------------------------------------------------------------------
-- High-Level Operations

encryptMessage :: BotClient -> ConvId -> Plaintext -> BotSession NewOtrMessage
encryptMessage clt cnv msg = NewOtrMessage (botClientId clt)
    <$> encrypt clt cnv msg
    <*> pure False   -- Native push?
    <*> pure False   -- Transient?
    <*> pure Nothing -- Priority
    <*> pure Nothing -- Extra data distributed to all recipients

decryptMessage :: BotClient -> ConvEvent OtrMessage -> BotSession ByteString
decryptMessage clt e = do
    let o = convEvtData e
    ciph <- decodeBase64 (otrCiphertext o)
    decrypt clt (convEvtFrom e) (otrSender o) ciph

-----------------------------------------------------------------------------
-- Auxiliary Symmetric Encryption

data SymmetricKeys = SymmetricKeys
    { symmetricEncKey :: !ByteString
    , symmetricMacKey :: !ByteString
    } deriving (Eq, Show)

instance Serialize SymmetricKeys where
    put k = putByteString (symmetricEncKey k) >> putByteString (symmetricMacKey k)
    get   = SymmetricKeys <$> getByteString 32 <*> getByteString 32

randomSymmetricKeys :: MonadIO m => BotClient -> m SymmetricKeys
randomSymmetricKeys clt = SymmetricKeys <$> randomBytes (botClientBox clt) 32
                                        <*> randomBytes (botClientBox clt) 32

encryptSymmetric :: MonadIO m => BotClient -> SymmetricKeys -> Plaintext -> m Ciphertext
encryptSymmetric clt (SymmetricKeys ekey mkey) msg = liftIO $ do
    aes <- initAES256 ekey
    iv  <- randomBytes (botClientBox clt) 16
    let ciphertext = iv <> cbcEncrypt aes (aesIV iv) (padPKCS7 msg)
    let mac = hmac (toByteString' mkey) ciphertext :: HMAC SHA256
    return $ convert mac <> ciphertext

decryptSymmetric :: MonadIO m => BotClient -> SymmetricKeys -> Ciphertext -> m Plaintext
decryptSymmetric _ (SymmetricKeys ekey mkey) msg = liftIO $ do
    aes <- initAES256 ekey
    let (dgst, ciphertext) = BS.splitAt 32 msg
    sha256 <- requireMaybe (digestFromByteString dgst) "Bad MAC"
    let mac = hmac (toByteString' mkey) ciphertext :: HMAC SHA256
    unless (HMAC sha256 == mac) $
        throwM $ RequirementFailed "Bad MAC"
    let (iv, dat) = BS.splitAt 16 ciphertext
    return $ unpadPKCS7 $ cbcDecrypt aes (aesIV iv) dat

-----------------------------------------------------------------------------
-- Helpers

initAES256 :: ByteString -> IO AES256
initAES256 = throwCryptoErrorIO . cipherInit

aesIV :: ByteString -> IV AES256
aesIV = fromMaybe (error "Network.Wire.Bot.Crypto: Bad IV") . makeIV

padPKCS7 :: Plaintext -> Plaintext
padPKCS7 = pad (PKCS7 16)

unpadPKCS7 :: Plaintext -> Plaintext
unpadPKCS7 = fromMaybe (error "Network.Wire.Bot.Crypto: Bad padding") . unpad (PKCS7 16)

mkSID :: UserId -> ClientId -> CBox.SID
mkSID u c = CBox.SID $ toByteString' u <> "." <> toByteString' (client c)

decodePrekey :: ClientPrekey -> BotSession ByteString
decodePrekey = decodeBase64 . prekeyKey . prekeyData

decodeBase64 :: Text -> BotSession ByteString
decodeBase64 = requireRight . B64.decode . encodeUtf8
