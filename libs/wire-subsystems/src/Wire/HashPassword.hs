{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.HashPassword where

import Data.Misc
import Imports
import Polysemy
import Wire.API.Password

data PasswordStatus
  = PasswordStatusOk
  | PasswordStatusNeedsUpdate
  deriving (Show, Eq)

data HashPassword m a where
  HashPassword6 :: PlainTextPassword6 -> HashPassword m Password
  HashPassword8 :: PlainTextPassword8 -> HashPassword m Password
  VerifyPasswordWithStatus :: PlainTextPassword' t -> Password -> HashPassword m (Bool, PasswordStatus)

makeSem ''HashPassword

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword :: (Member HashPassword r) => PlainTextPassword' t -> Password -> Sem r Bool
verifyPassword plain pwd = fst <$> verifyPasswordWithStatus plain pwd
