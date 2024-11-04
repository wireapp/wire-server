module Wire.MockInterpreters.ActivationCodeStore where

import Data.Id
import Data.Map
import Data.Text (pack)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as T
import Imports
import Polysemy
import Polysemy.State
import Text.Printf (printf)
import Wire.API.User.Activation
import Wire.ActivationCodeStore (ActivationCodeStore (..))
import Wire.UserKeyStore

inMemoryActivationCodeStoreInterpreter ::
  ( Member (State (Map EmailKey (Maybe UserId, ActivationCode))) r
  ) =>
  InterpreterFor ActivationCodeStore r
inMemoryActivationCodeStoreInterpreter = interpret \case
  LookupActivationCode ek -> gets (!? ek)
  NewActivationCode ek _ uid -> do
    let key =
          ActivationKey
            . Ascii.encodeBase64Url
            . T.encodeUtf8
            . emailKeyUniq
            $ ek
        code =
          ActivationCode
            . Ascii.unsafeFromText
            . pack
            . printf "%06d"
            . length
            . show
            $ ek
    modify (insert ek (uid, code)) $> Activation key code
