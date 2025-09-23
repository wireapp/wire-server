-- This file is part of the Wire Server implementation.

module Main (main) where

import Imports
import qualified Test.CargoHold.API.AuditLogTest as AuditLog
import Test.Tasty

main :: IO ()
main = defaultMain (testGroup "Cargohold Unit" [AuditLog.tests])
