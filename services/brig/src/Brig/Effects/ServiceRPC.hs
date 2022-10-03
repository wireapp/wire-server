{-# LANGUAGE TemplateHaskell #-}

module Brig.Sem.ServiceRPC where

import Bilge
import qualified Data.ByteString.Lazy as BL
import Imports
import Network.HTTP.Types.Method
import Polysemy

data Service
  = Galley

data ServiceRPC (service :: Service) m a where
  Request ::
    StdMethod ->
    (Request -> Request) ->
    ServiceRPC service m (Response (Maybe BL.ByteString))

makeSem ''ServiceRPC
