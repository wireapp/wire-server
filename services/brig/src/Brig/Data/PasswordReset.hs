{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Persistent storage for password reset codes.
-- TODO: Use Brig.Data.Codes
module Brig.Data.PasswordReset
    ( createPasswordResetCode
    , verifyPasswordResetCode
    , lookupPasswordResetCode
    , deletePasswordResetCode
    , mkPasswordResetKey
    ) where

import Brig.App (AppIO, currentTime)
import Brig.Data.Instances ()
import Brig.Types
import Cassandra
import Control.Lens (view)
import Control.Monad.IO.Class
import Data.ByteString.Conversion
import Data.Functor.Identity
import Data.Id
import Data.Int (Int32)
import Data.Text (pack)
import Data.Time.Clock
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.EVP.Digest (getDigestByName, digestBS)
import OpenSSL.Random (randBytes)
import Text.Printf (printf)

import qualified Data.Text.Ascii as Ascii

maxAttempts :: Int32
maxAttempts = 3

ttl :: NominalDiffTime
ttl = 3600 -- 60 minutes

createPasswordResetCode :: UserId -> Either Email Phone -> AppIO PasswordResetPair
createPasswordResetCode u target = do
    key  <- liftIO $ mkPasswordResetKey u
    now  <- liftIO =<< view currentTime
    code <- liftIO $ either (const genEmailCode) (const genPhoneCode) target
    retry x5 . write codeInsert $ params Quorum (key, code, u, maxAttempts, ttl `addUTCTime` now, round ttl)
    return (key, code)
  where
    genEmailCode = PasswordResetCode . Ascii.encodeBase64Url <$> randBytes 24
    genPhoneCode = PasswordResetCode . Ascii.unsafeFromText . pack . printf "%06d"
                <$> randIntegerZeroToNMinusOne 1000000

lookupPasswordResetCode :: UserId -> AppIO (Maybe PasswordResetCode)
lookupPasswordResetCode u = do
    key <- liftIO $ mkPasswordResetKey u
    now <- liftIO =<< view currentTime
    validate now =<< retry x1 (query1 codeSelect (params Quorum (Identity key)))
  where
    validate now (Just (c, _, _, Just t)) | t > now = return $ Just c
    validate _   _                                  = return Nothing

verifyPasswordResetCode :: PasswordResetPair -> AppIO (Maybe UserId)
verifyPasswordResetCode (k, c) = do
    now <- liftIO =<< view currentTime
    code <- retry x1 (query1 codeSelect (params Quorum (Identity k)))
    case code of
        Just (c', u,      _, Just t) | c == c' && t >= now -> return (Just u)
        Just (c', u, Just n, Just t) | n > 1   && t >  now -> do
            countdown (k, c', u, n-1, t, round ttl)
            return Nothing
        Just (_, _,  _,      _) -> deletePasswordResetCode k >> return Nothing
        Nothing                 -> return Nothing
  where
    countdown = retry x5 . write codeInsert . params Quorum

deletePasswordResetCode :: PasswordResetKey -> AppIO ()
deletePasswordResetCode k = retry x5 . write codeDelete $ params Quorum (Identity k)

mkPasswordResetKey :: (MonadIO m) => UserId -> m PasswordResetKey
mkPasswordResetKey u = do
    d <- liftIO $ getDigestByName "SHA256" >>= maybe (error "SHA256 not found") return
    return . PasswordResetKey . Ascii.encodeBase64Url . digestBS d $ toByteString' u

-- Queries

codeInsert :: PrepQuery W (PasswordResetKey, PasswordResetCode, UserId, Int32, UTCTime, Int32) ()
codeInsert = "INSERT INTO password_reset (key, code, user, retries, timeout) VALUES (?, ?, ?, ?, ?) USING TTL ?"

codeSelect :: PrepQuery R (Identity PasswordResetKey) (PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime)
codeSelect = "SELECT code, user, retries, timeout FROM password_reset WHERE key = ?"

codeDelete :: PrepQuery W (Identity PasswordResetKey) ()
codeDelete = "DELETE FROM password_reset WHERE key = ?"
