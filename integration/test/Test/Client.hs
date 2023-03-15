{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Test.Client where

import API
import App
import Data.Default
import Imports
import Response
import SetupHelpers

-- | Cannot delete a legalhold client
--
-- More comments
testCantDeleteLHClient :: HasCallStack => App ()
testCantDeleteLHClient = do
  user <- randomUser def
  bindResponse (addClient user def {ctype = "legalhold", internal = True}) $ \resp -> do
    (@?=) resp.status 200

testOtherWithoutComments :: App ()
testOtherWithoutComments = pure ()
