module Wire.MockInterpreters.HashPassword where

import Data.Misc
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Wire.API.Password
import Wire.HashPassword

staticHashPasswordInterpreter :: InterpreterFor HashPassword r
staticHashPasswordInterpreter = interpret $ \case
  HashPasswordScrypt password -> go hashPasswordScryptWithSalt "salt" password
  HashPasswordArgon2id password -> go hashPasswordArgon2idWithSalt "9bytesalt" password
  where
    go alg salt password = do
      let passwordBS = Text.encodeUtf8 (fromPlainTextPassword password)
      pure $ unsafeMkPassword $ alg salt passwordBS
