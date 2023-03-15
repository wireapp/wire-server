{-# LANGUAGE LambdaCase #-}

module Test.Client where

import App
import Data.Default
import Imports
import SetupHelpers

-- | Cannot delete a legalhold client
--
-- More comments
testCantDeleteLHClient :: App ()
testCantDeleteLHClient = do
  user <- randomUser def
  print user

testOtherWithoutComments :: App ()
testOtherWithoutComments = pure ()
