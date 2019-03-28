module Main (main) where

import Imports
import Cassandra.Util (defInitCassandra)
import Test.Tasty(defaultMain, testGroup)

import qualified System.Logger         as Logger

import qualified Operations.Identity   as OI

main :: IO ()
main = do
  -- For command line arguments for the configPaths and tasty parser not to interfere,
  -- split arguments into configArgs and otherArgs

  let casHost="127.0.0.1"
  let casPort=9042 
  let casKey="castest"

  lg <- Logger.new Logger.defSettings  -- TODO: use mkLogger'?
  db <- defInitCassandra casKey casHost casPort lg
  cassandraApi <- OI.tests db

  defaultMain $ testGroup "Cassandra-Util API Integration"
    [
      cassandraApi
    ]

