module Wire.MockInterpreters.PasswordResetCodeStore where

import Data.Map qualified as Map
import Data.Text.Ascii
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User.Password
import Wire.PasswordResetCodeStore

inMemoryPasswordResetCodeStore ::
  forall r.
  (Member (State (Map PasswordResetKey (PRQueryData Identity))) r) =>
  InterpreterFor PasswordResetCodeStore r
inMemoryPasswordResetCodeStore =
  interpret
    \case
      GenerateEmailCode ->
        pure . PasswordResetCode . encodeBase64Url $ "email-code"
      GeneratePhoneCode -> (error "deprecated")
      CodeSelect resetKey -> do
        gets $
          fmap (mapPRQueryData (Just . runIdentity))
            . Map.lookup resetKey
      CodeInsert resetKey queryData _ttl -> do
        modify $ Map.insert resetKey queryData
      CodeDelete resetKey -> do
        modify $ Map.delete resetKey
