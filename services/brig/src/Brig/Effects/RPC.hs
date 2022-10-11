{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.RPC where

import Bilge
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as LT
import Imports
import Network.HTTP.Types.Method
import Polysemy

data RPC m a where
  ServiceRequest ::
    LT.Text ->
    Request ->
    StdMethod ->
    (Request -> Request) ->
    RPC m (Response (Maybe BL.ByteString))

makeSem ''RPC
