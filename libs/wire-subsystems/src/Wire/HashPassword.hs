{-# LANGUAGE TemplateHaskell #-}

module Wire.HashPassword where

import Data.Misc
import Imports
import Polysemy
import Wire.API.Password (Password)
import Wire.API.Password qualified as Password

data HashPassword m a where
  HashPassword :: PlainTextPassword8 -> HashPassword m Password

makeSem ''HashPassword

runHashPassword :: (Member (Embed IO) r) => InterpreterFor HashPassword r
runHashPassword = interpret $ \case
  HashPassword pw -> liftIO $ Password.mkSafePassword pw
