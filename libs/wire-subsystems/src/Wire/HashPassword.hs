{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.HashPassword where

import Data.Misc
import Imports
import Polysemy
import Util.Options
import Wire.API.Password
import Wire.API.Password qualified as Password

data HashPassword m a where
  HashPassword6 :: PlainTextPassword6 -> HashPassword m Password
  HashPassword8 :: PlainTextPassword8 -> HashPassword m Password
  VerifyPasswordWithStatus :: PlainTextPassword' t -> Password -> HashPassword m (Bool, PasswordStatus)

makeSem ''HashPassword

runHashPassword ::
  forall r.
  ( Member (Embed IO) r
  ) =>
  PasswordHashingOptions ->
  InterpreterFor HashPassword r
runHashPassword opts =
  interpret $
    \case
      HashPassword6 pw6 -> hashFunction pw6
      HashPassword8 pw8 -> hashFunction pw8
      VerifyPasswordWithStatus plain pwd -> pure $ verifyPasswordWithStatusInternal plain pwd
  where
    hashFunction :: PlainTextPassword' t -> Sem r Password
    hashFunction = case opts of
      PasswordHashingArgon2id o -> Password.mkSafePassword (argon2OptsFromHashingOpts o)
      PasswordHashingScrypt -> Password.mkSafePasswordScrypt

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword :: (Member HashPassword r) => PlainTextPassword' t -> Password -> Sem r Bool
verifyPassword plain pwd = fst <$> verifyPasswordWithStatus plain pwd
