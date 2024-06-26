module Wire.MockInterpreters.HashPassword where

import Crypto.KDF.Argon2 as Argon2
import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Wire.API.Password
import Wire.HashPassword

staticHashPasswordInterpreter :: InterpreterFor HashPassword r
staticHashPasswordInterpreter = interpret $ \case
  HashPasswordArgon2id password -> go (hashPasswordArgon2idWithOptions fastArgon2IdOptions) "9bytesalt" password
  where
    go alg salt password = do
      let passwordBS = Text.encodeUtf8 (fromPlainTextPassword password)
      pure $ unsafeMkPassword $ alg salt passwordBS

fastArgon2IdOptions :: Argon2.Options
fastArgon2IdOptions =
  let hashParallelism = 4
   in defaultOptions
        { iterations = 1,
          parallelism = hashParallelism,
          -- This needs to be min 8 * hashParallelism, otherewise we get an
          -- unsafe error
          memory = 8 * hashParallelism
        }
