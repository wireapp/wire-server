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

emailKeyToCode :: EmailKey -> ActivationCode
emailKeyToCode =
  ActivationCode
    . Ascii.unsafeFromText
    . pack
    . printf "%06d"
    . length
    . show

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
        c = emailKeyToCode ek
    modify (insert ek (uid, c)) $> Activation key c
