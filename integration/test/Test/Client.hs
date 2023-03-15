{-# LANGUAGE LambdaCase #-}

module Test.Client where

import API
import App
import Data.Default
import Imports
import qualified Network.HTTP.Types as HTTP
import Response
import SetupHelpers

-- | Cannot delete a legalhold client
--
-- More comments
testCantDeleteLHClient :: App ()
testCantDeleteLHClient = do
  user <- randomUser def
  bindResponse (addClient user def {ctype = "legalhold", internal = True}) $ \resp -> do
    (@?=) resp.status HTTP.status201

testOtherWithoutComments :: App ()
testOtherWithoutComments = pure ()
