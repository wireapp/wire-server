module Wire.MockInterpreters.HashPassword where

import Crypto.KDF.Argon2 as Argon2
import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Wire.API.Password as Password
import Wire.HashPassword

staticHashPasswordInterpreter :: InterpreterFor HashPassword r
staticHashPasswordInterpreter = interpret $ \case
  HashPassword6 password -> hashPassword password
  HashPassword8 password -> hashPassword password

hashPassword :: (Monad m) => PlainTextPassword' t -> m Password
hashPassword password =
  pure . Argon2Password $
    hashPasswordArgon2idWithSalt
      fastArgon2IdOptions
      "9bytesalt"
      (Text.encodeUtf8 (fromPlainTextPassword password))

fastArgon2IdOptions :: Argon2.Options
fastArgon2IdOptions =
  let hashParallelism = 4
   in Password.defaultOptions
        { iterations = 1,
          parallelism = hashParallelism,
          -- This needs to be min 8 * hashParallelism, otherewise we get an
          -- unsafe error
          memory = 8 * hashParallelism
        }
