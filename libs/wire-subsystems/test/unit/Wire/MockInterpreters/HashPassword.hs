module Wire.MockInterpreters.HashPassword where

import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Wire.API.Password
import Wire.HashPassword

-- TODO: argond2id is really slow, we should perhaps move verify password here,
-- and do something fast by either messing with the options of the argon2id
-- algorithm or just not hashing things.
staticHashPasswordInterpreter :: InterpreterFor HashPassword r
staticHashPasswordInterpreter = interpret $ \case
  HashPasswordScrypt password -> go hashPasswordScryptWithSalt "salt" password
  HashPasswordArgon2id password -> go hashPasswordArgon2idWithSalt "9bytesalt" password
  where
    go alg salt password = do
      let passwordBS = Text.encodeUtf8 (fromPlainTextPassword password)
      pure $ unsafeMkPassword $ alg salt passwordBS
