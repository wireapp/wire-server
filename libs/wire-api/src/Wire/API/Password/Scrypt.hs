{-# LANGUAGE RecordWildCards #-}

module Wire.API.Password.Scrypt where

import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as C8
import Data.Misc
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports

data ScryptHashedPassword = ScryptHashedPassword
  { params :: ScryptParameters,
    salt :: ByteString,
    hashedKey :: ByteString
  }

-------------------------------------------------------------------------------

data ScryptParameters = ScryptParameters
  { -- | Bytes to randomly generate as a unique salt, default is __32__
    saltLength :: Word32,
    -- | log2(N) rounds to hash, default is __14__ (i.e. 2^14 rounds)
    rounds :: Word32,
    -- | Block size, default is __8__
    --
    -- Limits are min: @1@, and max: @blockSize * scryptParallelism < 2 ^ 30@
    blockSize :: Word32,
    -- | Parallelism factor, default is __1__
    --
    -- Limits are min: @0@, and max: @blockSize * scryptParallelism < 2 ^ 30@
    parallelism :: Word32,
    -- | Output key length in bytes, default is __64__
    outputLength :: Word32
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------

encodeScryptPassword :: ScryptHashedPassword -> Text
encodeScryptPassword ScryptHashedPassword {..} =
  Text.intercalate
    "|"
    [ showT params.rounds,
      showT params.blockSize,
      showT params.parallelism,
      Text.decodeUtf8 . B64.encode $ salt,
      Text.decodeUtf8 . B64.encode $ hashedKey
    ]

parseScryptPasswordHashParams :: ByteString -> Either String ScryptHashedPassword
parseScryptPasswordHashParams passwordHash = do
  let paramList = Text.split (== '|') . Text.decodeUtf8 $ passwordHash
  case paramList of
    [roundsStr, blockSizeStr, parallelismStr, salt64, hashedKey64] -> do
      rounds <- eitherFromMaybe "rounds" $ readT roundsStr
      blockSize <- eitherFromMaybe "blockSize" $ readT blockSizeStr
      parallelism <- eitherFromMaybe "parellelism" $ readT parallelismStr
      salt <- from64 salt64
      hashedKey <- from64 hashedKey64
      let outputLength = fromIntegral $ C8.length hashedKey
          saltLength = fromIntegral $ C8.length salt
      pure $ ScryptHashedPassword {params = ScryptParameters {..}, ..}
    _ -> Left $ "failed to parse ScryptHashedPassword: expected exactly 5 params"
  where
    eitherFromMaybe :: String -> Maybe a -> Either String a
    eitherFromMaybe paramName = maybe (Left $ "failed to parse scrypt parameter: " <> paramName) Right
