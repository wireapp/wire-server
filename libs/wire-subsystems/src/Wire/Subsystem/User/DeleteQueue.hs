{-# LANGUAGE TemplateHaskell #-}

module Wire.DeleteQueue where

import Data.Id
import Imports
import Polysemy

data DeleteQueue m a where
  EnqueueUserDeletion :: UserId -> DeleteQueue m ()
  EnqueueClientDeletion :: ClientId -> UserId -> Maybe ConnId -> DeleteQueue m ()
  EnqueueServiceDeletion :: ProviderId -> ServiceId -> DeleteQueue m ()

makeSem ''DeleteQueue
