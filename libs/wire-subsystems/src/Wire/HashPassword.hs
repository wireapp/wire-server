{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.HashPassword where

import Crypto.KDF.Argon2 qualified as Argon2
import Data.Misc
import Imports
import Polysemy
import Wire.API.Password (Password)
import Wire.API.Password qualified as Password

data HashPassword m a where
  HashPassword6 :: PlainTextPassword6 -> HashPassword m Password
  HashPassword8 :: PlainTextPassword8 -> HashPassword m Password

makeSem ''HashPassword

runHashPassword ::
  ( Member (Embed IO) r
  ) =>
  Argon2.Options ->
  InterpreterFor HashPassword r
runHashPassword opts =
  interpret $
    \case
      HashPassword6 pw6 -> hashPasswordImpl opts pw6
      HashPassword8 pw8 -> hashPasswordImpl opts pw8

hashPasswordImpl ::
  (Member (Embed IO) r) =>
  Argon2.Options ->
  PlainTextPassword' t ->
  Sem r Password
hashPasswordImpl opts pwd = do
  liftIO $ Password.mkSafePassword opts pwd
