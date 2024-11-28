{-# LANGUAGE RecordWildCards #-}

module Wire.HashPassword.Scrypt where

import Crypto.KDF.Scrypt (Parameters (..), generate)
import Data.ByteArray
import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Wire.API.Password
import Wire.API.Password.Scrypt
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

mkSafePasswordScrypt :: (Member Random r) => PlainTextPassword' t -> Sem r Password
mkSafePasswordScrypt =
  fmap ScryptPassword . hashPasswordScrypt . Text.encodeUtf8 . fromPlainTextPassword

hashPasswordScrypt :: (Member Random r) => ByteString -> Sem r ScryptHashedPassword
hashPasswordScrypt password = do
  salt <- Random.bytes $ fromIntegral defaultScryptParams.saltLength
  let params = defaultScryptParams
  let hashedKey = hashPasswordWithParams params password salt
  pure $! ScryptHashedPassword {..}

defaultScryptParams :: ScryptParameters
defaultScryptParams =
  ScryptParameters
    { saltLength = 32,
      rounds = 14,
      blockSize = 8,
      parallelism = 1,
      outputLength = 64
    }

fromScrypt :: ScryptParameters -> Parameters
fromScrypt scryptParams =
  Parameters
    { n = 2 ^ scryptParams.rounds,
      r = fromIntegral scryptParams.blockSize,
      p = fromIntegral scryptParams.parallelism,
      outputLength = 64
    }

hashPasswordWithParams ::
  ( ByteArrayAccess password,
    ByteArrayAccess salt
  ) =>
  ScryptParameters ->
  password ->
  salt ->
  ByteString
hashPasswordWithParams parameters password salt =
  convert (generate (fromScrypt parameters) password salt :: Bytes)
