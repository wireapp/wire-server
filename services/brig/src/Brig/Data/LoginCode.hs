{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Persistent storage for login codes.
-- TODO: Use Brig.Data.Codes
-- TODO: Move to Brig.User.Auth.DB.LoginCode
module Brig.Data.LoginCode
    ( LoginCode
    , createLoginCode
    , verifyLoginCode
    , lookupLoginCode
    ) where

import Imports
import Brig.App (AppIO, currentTime)
import Brig.User.Auth.DB.Instances ()
import Brig.Data.Instances ()
import Brig.Types.User.Auth
import Brig.Types.Code (Timeout (..))
import Cassandra
import Control.Lens (view)
import Data.Id
import Data.Time.Clock
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import Text.Printf (printf)

import qualified Data.Text as T

-- | Max. number of verification attempts per code.
maxAttempts :: Int32
maxAttempts = 3

-- | Timeout of individual codes.
ttl :: NominalDiffTime
ttl = 600

createLoginCode :: UserId -> AppIO PendingLoginCode
createLoginCode u = do
    now  <- liftIO =<< view currentTime
    code <- liftIO genCode
    insertLoginCode u code maxAttempts (ttl `addUTCTime` now)
    return $! PendingLoginCode code (Timeout ttl)
  where
    genCode = LoginCode . T.pack . printf "%06d" <$> randIntegerZeroToNMinusOne 1000000

verifyLoginCode :: UserId -> LoginCode -> AppIO Bool
verifyLoginCode u c = do
    code <- retry x1 (query1 codeSelect (params Quorum (Identity u)))
    now  <- liftIO =<< view currentTime
    case code of
        Just (c', _, t) | c == c' && t >= now -> deleteLoginCode u >> return True
        Just (c', n, t) | n > 1   && t >  now -> insertLoginCode u c' (n - 1) t >> return False
        Just (_ , _, _)                       -> deleteLoginCode u >> return False
        Nothing                               -> return False

lookupLoginCode :: UserId -> AppIO (Maybe PendingLoginCode)
lookupLoginCode u = do
    now <- liftIO =<< view currentTime
    validate now =<< retry x1 (query1 codeSelect (params Quorum (Identity u)))
  where
    validate now (Just (c, _, t)) | now < t = return (Just (pending c now t))
    validate _   _                          = return Nothing

    pending c now t = PendingLoginCode c (timeout now t)
    timeout   now t = Timeout (t `diffUTCTime` now)

deleteLoginCode :: UserId -> AppIO ()
deleteLoginCode u = retry x5 . write codeDelete $ params Quorum (Identity u)

insertLoginCode :: UserId -> LoginCode -> Int32 -> UTCTime -> AppIO ()
insertLoginCode u c n t = retry x5 . write codeInsert $ params Quorum (u, c, n, t, round ttl)

-- Queries

codeInsert :: PrepQuery W (UserId, LoginCode, Int32, UTCTime, Int32) ()
codeInsert = "INSERT INTO login_codes (user, code, retries, timeout) VALUES (?, ?, ?, ?) USING TTL ?"

codeSelect :: PrepQuery R (Identity UserId) (LoginCode, Int32, UTCTime)
codeSelect = "SELECT code, retries, timeout FROM login_codes WHERE user = ?"

codeDelete :: PrepQuery W (Identity UserId) ()
codeDelete = "DELETE FROM login_codes WHERE user = ?"
