{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Main
  ( main,
  )
where

import Control.Error
import Control.Lens
import Data.ByteString.Base64.URL
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy.Char8 as L
import Data.UUID (UUID, fromASCIIBytes)
import Data.ZAuth.Creation as C
import Data.ZAuth.Token
import Data.ZAuth.Validation as V
import Imports
import Options.Applicative hiding (header)
import qualified Options.Applicative as O
import Options.Applicative.Types
import Sodium.Crypto.Sign
import System.Exit

data Mode
  = CreateUser
  | CreateSession
  | CreateAccess
  | CreateBot
  | CreateProvider
  | VerifyUser
  | VerifyAccess
  | VerifyBot
  | VerifyProvider
  | GenKeyPair
  deriving (Eq, Show, Enum)

data ZOpts = ZOpts
  { _dur :: !Integer,
    _skey :: !ByteString,
    _idx :: !Int,
    _mode :: !Mode,
    _dat :: [ByteString]
  }
  deriving (Eq, Show)

makeLenses ''ZOpts

main :: IO ()
main = do
  o <- execParser (info (helper <*> options) desc)
  go (o ^. mode) o
  where
    desc = O.header "Create/Validate access tokens." <> fullDesc

go :: Mode -> ZOpts -> IO ()
go VerifyUser o = check' (o ^. skey) (tkn (o ^. dat) fromByteString :: Token User)
go VerifyAccess o = check' (o ^. skey) (tkn (o ^. dat) fromByteString :: Token Access)
go VerifyBot o = check' (o ^. skey) (tkn (o ^. dat) fromByteString :: Token Bot)
go VerifyProvider o = check' (o ^. skey) (tkn (o ^. dat) fromByteString :: Token Provider)
go CreateSession o = do
  when (length (o ^. dat) /= 2) $
    error "invalid --data, must have 2 elements"
  let u = uuid . head $ o ^. dat
  case fromByteString ((o ^. dat) !! 1) of
    Nothing -> error "invalid random int"
    Just rn -> runCreate' o $ toByteString <$> sessionToken (o ^. dur) u rn
go CreateUser o = do
  when (length (o ^. dat) /= 2) $
    error "invalid --data, must have 2 elements"
  let u = uuid . head $ o ^. dat
  case fromByteString ((o ^. dat) !! 1) of
    Nothing -> error "invalid random int"
    Just rn -> runCreate' o $ toByteString <$> userToken (o ^. dur) u rn
go CreateAccess o = do
  when (null (o ^. dat)) $
    error "invalid --data, must have 1 or 2 elements"
  let u = uuid . head $ o ^. dat
  case length (o ^. dat) of
    1 -> runCreate' o $ toByteString <$> accessToken1 (o ^. dur) u
    2 -> case fromByteString ((o ^. dat) !! 1) of
      Nothing -> error "invalid connection"
      Just c -> runCreate' o $ toByteString <$> accessToken (o ^. dur) u c
    _ -> error "invalid --data, must have 1 or 2 elements"
go CreateBot o = do
  when (length (o ^. dat) /= 3) $
    error "invalid --data, must have 3 elements"
  let p = uuid $ (o ^. dat) !! 0
      b = uuid $ (o ^. dat) !! 1
      c = uuid $ (o ^. dat) !! 2
  runCreate' o $ toByteString <$> botToken p b c
go CreateProvider o = do
  when (length (o ^. dat) /= 1) $
    error "missing --data"
  let p = uuid $ (o ^. dat) !! 0
  runCreate' o $ toByteString <$> providerToken (o ^. dur) p
go GenKeyPair _ = do
  (p, s) <- newKeyPair
  putStrLn $ "public: " <> show p
  putStrLn $ "secret: " <> show s

tkn :: [ByteString] -> (ByteString -> Maybe (Token a)) -> Token a
tkn xs f = fromMaybe (error "Failed to read token") . f $ headDef "missing token data" xs

uuid :: ByteString -> UUID
uuid s = fromMaybe (error $ "Invalid UUID: " ++ show s) $ fromASCIIBytes s

check' :: ToByteString a => ByteString -> Token a -> IO ()
check' k t = exceptT (\e -> putStrLn e >> exitFailure) (const $ return ()) $ do
  p <- hoistEither $ PublicKey <$> decode k
  e <- liftIO $ runValidate (V.mkEnv p (replicate (t ^. header . key) p)) (check t)
  hoistEither $ fmapL show e

runCreate' :: ZOpts -> Create LByteString -> IO ()
runCreate' o m = exceptT putStrLn L.putStrLn $ do
  s <- hoistEither $ SecretKey <$> decode (o ^. skey)
  z <- lift $ C.mkEnv s (replicate (o ^. idx) s)
  lift $ runCreate z (o ^. idx) m

options :: Parser ZOpts
options =
  ZOpts
    <$> optDuration
    <*> optKey
    <*> optIdx
    <*> optMode
    <*> optData
  where
    optDuration =
      fmap read . strOption $
        long "duration"
          <> short 'd'
          <> metavar "STRING"
          <> value "3600"
          <> showDefault
          <> help "token validity duration in seconds"
    optKey =
      fmap fromString . strOption $
        long "key"
          <> short 'k'
          <> value ""
          <> metavar "STRING"
          <> help "public or private key"
    optIdx =
      fmap read . strOption $
        long "index"
          <> short 'i'
          <> metavar "INT"
          <> help "key index"
    optMode =
      option toMode $
        long "mode"
          <> short 'm'
          <> metavar "STRING"
          <> value CreateAccess
          <> showDefaultWith (const "create-access")
          <> help
            "create-user | create-access | create-session | create-bot | create-provider \
            \ verify-user | verify-access | verify-bot | verify-provider | gen-keypair"
    optData =
      many <$> fmap fromString . strOption $
        long "data"
          <> metavar "STRING"
          <> help "token data"
    toMode =
      readerAsk >>= \s -> case s of
        "create-user" -> return CreateUser
        "create-session" -> return CreateSession
        "create-access" -> return CreateAccess
        "create-bot" -> return CreateBot
        "create-provider" -> return CreateProvider
        "verify-user" -> return VerifyUser
        "verify-access" -> return VerifyAccess
        "verify-bot" -> return VerifyBot
        "verify-provider" -> return VerifyProvider
        "gen-keypair" -> return GenKeyPair
        other -> readerError $ "invalid mode: " <> other
