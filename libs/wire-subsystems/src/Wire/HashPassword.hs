{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.HashPassword where

import Crypto.KDF.Argon2 qualified as Argon2
import Data.Misc
import Imports
import Polysemy
import Polysemy.Input
import Util.Options (PasswordHashingOptions (..))
import Wire.API.Password (Password)
import Wire.API.Password qualified as Password

data HashPassword m a where
  HashPassword6 :: PlainTextPassword6 -> HashPassword m Password
  HashPassword8 :: PlainTextPassword8 -> HashPassword m Password

makeSem ''HashPassword

runHashPassword ::
  ( Member (Embed IO) r,
    Member (Input (Maybe PasswordHashingOptions)) r
  ) =>
  InterpreterFor HashPassword r
runHashPassword = interpret $ \case
  HashPassword6 pw6 -> hashPasswordImpl pw6
  HashPassword8 pw8 -> hashPasswordImpl pw8

hashPasswordImpl ::
  ( Member (Input (Maybe PasswordHashingOptions)) r,
    Member (Embed IO) r
  ) =>
  PlainTextPassword' t ->
  Sem r Password
hashPasswordImpl pwd = do
  hashingOptsM <- input
  let argonOpts = case hashingOptsM of
        Just (PasswordHashingOptions {..}) ->
          Argon2.Options
            { variant = Argon2.Argon2id,
              version = Argon2.Version13,
              iterations = fromIntegral iterations,
              memory = fromIntegral memory,
              parallelism = fromIntegral parallelism
            }
        Nothing -> Password.defaultOptions

  liftIO $ Password.mkSafePassword (Just argonOpts) pwd
