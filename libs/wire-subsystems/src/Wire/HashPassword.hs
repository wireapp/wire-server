{-# LANGUAGE TemplateHaskell #-}

module Wire.HashPassword where

import Data.Misc
import Imports
import Polysemy
import Wire.API.Password (Password)
import Wire.API.Password qualified as Password

data HashPassword m a where
  HashPasswordArgon2id :: PlainTextPassword8 -> HashPassword m Password
  HashPasswordScrypt :: PlainTextPassword8 -> HashPassword m Password

makeSem ''HashPassword

runHashPassword :: (Member (Embed IO) r) => InterpreterFor HashPassword r
runHashPassword = interpret $ \case
  HashPasswordArgon2id pw -> liftIO $ Password.mkSafePasswordArgon2id pw
  HashPasswordScrypt pw -> liftIO $ Password.mkSafePasswordScrypt pw
