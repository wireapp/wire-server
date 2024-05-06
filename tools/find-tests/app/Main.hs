{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Main
  ( main,
  )
where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 as BS
import Data.Map qualified as Map
import FindTests.CliInput
import FindTests.Load
import FindTests.Parse
import FindTests.StolenFromIntegration
import GHC
import GHC.Driver.Flags
import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Hs
import GHC.Hs.Dump
import GHC.Paths (libdir)
import GHC.Utils.Logger
import Imports as I
import System.FilePath
import System.FilePath (takeBaseName)

-- usage:
--
-- >>> $ make -C ~/src/wire-server c package=find-tests
-- >>> $ cd ~/src/wire-server && ./dist/find-tests ./tools/find-tests/test-data/prod-run.json

main :: IO ()
main = do
  [cliInputFile] <- getArgs
  wireServerRoot <- lookupEnv "WIRE_SERVER_ROOT" >>= maybe (error "*** $WIRE_SERVER_ROOT is not defined") pure

  CliInput cliInput <-
    either (error . show . ((cliInputFile <> ": ") <>)) pure
      =<< (eitherDecode <$> BS.readFile cliInputFile)

  let flatten :: forall e v. Show e => [[[Either e v]]] -> [v]
      flatten nestedInput = if I.null bad then good else error $ errmsg <> show bad
        where
          (bad, good) = I.partitionEithers . join . join $ nestedInput
          -- TODO: lookup failure msg: "if you think the function should be found, this could be
          -- because the parser is ignoring the function due to some peculiarity in the syntax of
          -- the target function.  please open a ticket."
          errmsg = ""

  runApp $ \dflags -> do
    cliOutput :: [FoundTestCase] <- fmap flatten . forM (Map.toList cliInput) $
      \(packageName, moduleMapping) -> forM (Map.toList moduleMapping) $
        \(relTargetFile, testCaseNames) -> do
          let absTargetFile = wireServerRoot <> packageName <> relTargetFile
          parsed <- loadHsModule absTargetFile

          let mapping :: Map String FoundTestCase
              mapping = parseTestCases dflags absTargetFile parsed

          -- for debugging:
          -- I.putStrLn . showSDoc dflags . showAstDataFull $ parsed
          -- liftIO $ BS.putStrLn $ encode mapping

          --  . fmap fromJust $ flip Map.lookup mapping <$> testCaseNames
          pure (undefined :: [Either () FoundTestCase])

    error . BS.unpack . encodePretty $ cliOutput
