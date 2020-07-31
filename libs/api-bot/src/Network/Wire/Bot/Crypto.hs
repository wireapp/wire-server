{-# LANGUAGE OverloadedStrings #-}

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

module Network.Wire.Bot.Crypto
  ( Plaintext,
    Ciphertext,

    -- * Session Basics (Double Ratchet)
    botInitSession,
    clientInitSession,
    initSessionFromMsg,
    encrypt,
    decrypt,

    -- * High-Level Operations (Double Ratchet)
    encryptMessage,
    decryptMessage,

    -- * Auxiliary Symmetric Encryption
    SymmetricKeys,
    randomSymmetricKeys,
    encryptSymmetric,
    decryptSymmetric,

    -- * Utilities
    randomBytes,
  )
where

import Control.Monad.Catch
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error
import Crypto.Hash (digestFromByteString)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Conversion
import Data.Id
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Imports
import Network.Wire.Bot.Clients
import Network.Wire.Bot.Crypto.Glue (randomBytes, unwrap)
import Network.Wire.Bot.Monad
import Network.Wire.Client.API.Client
import Network.Wire.Client.API.Conversation
import Network.Wire.Client.API.Push
import qualified System.CryptoBox as CBox

type Plaintext = ByteString

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
  bytes <- decodeBase64 (otrCiphertext o)
  (s, x) <- liftIO $ unwrap =<< CBox.sessionFromMessage b i bytes
  addSession (botClientSessions cl) u (Map.singleton (otrSender o) s)
  liftIO $ CBox.copyBytes x

-- | Encrypt an OTR message for all other clients in a given conversation.
encrypt :: BotClient -> ConvId -> ByteString -> BotSession OtrRecipients
encrypt cl cnv val = fmap (OtrRecipients . UserClientMap)
  . foldSessions (botClientSessions cl) cnv Map.empty
  $ \u c s rcps ->
    if botClientId cl == c
      then return rcps
      else liftIO $ do
        ciphertext <- do
          bs <- CBox.encrypt s val >>= unwrap >>= CBox.copyBytes
          return $! decodeUtf8 $! B64.encode bs
        let userId = makeIdOpaque u
        return $ Map.insertWith Map.union userId (Map.singleton c ciphertext) rcps

-- | Decrypt an OTR message received from a given user and client.
decrypt :: BotClient -> UserId -> ClientId -> ByteString -> BotSession ByteString
decrypt clt usr sid msg = do
  s <- flip requireMaybe "Missing client session" =<< lookupSession (botClientSessions clt) usr sid
  liftIO $ CBox.copyBytes =<< unwrap =<< CBox.decrypt s msg

-----------------------------------------------------------------------------
-- High-Level Operations

encryptMessage :: BotClient -> ConvId -> Plaintext -> BotSession NewOtrMessage
encryptMessage clt cnv msg =
  NewOtrMessage (botClientId clt)
    <$> encrypt clt cnv msg
    <*> pure False -- Native push?
    <*> pure False -- Transient?
    <*> pure Nothing -- Priority
    <*> pure Nothing -- Extra data distributed to all recipients
    <*> pure Nothing

decryptMessage :: BotClient -> ConvEvent OtrMessage -> BotSession ByteString
decryptMessage clt e = do
  let o = convEvtData e
  ciph <- decodeBase64 (otrCiphertext o)
  decrypt clt (convEvtFrom e) (otrSender o) ciph

-----------------------------------------------------------------------------
-- Auxiliary Symmetric Encryption

data SymmetricKeys = SymmetricKeys
  { symmetricEncKey :: !ByteString,
    symmetricMacKey :: !ByteString
  }
  deriving (Eq, Show)

instance Serialize SymmetricKeys where
  put k = putByteString (symmetricEncKey k) >> putByteString (symmetricMacKey k)
  get = SymmetricKeys <$> getByteString 32 <*> getByteString 32

randomSymmetricKeys :: MonadIO m => BotClient -> m SymmetricKeys
randomSymmetricKeys clt =
  SymmetricKeys <$> randomBytes (botClientBox clt) 32
    <*> randomBytes (botClientBox clt) 32

encryptSymmetric :: MonadIO m => BotClient -> SymmetricKeys -> Plaintext -> m Ciphertext
encryptSymmetric clt (SymmetricKeys ekey mkey) msg = liftIO $ do
  aes <- initAES256 ekey
  iv <- randomBytes (botClientBox clt) 16
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
    throwM $
      RequirementFailed "Bad MAC"
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
