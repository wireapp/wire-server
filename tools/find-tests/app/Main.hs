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
-- import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 as BS
import Data.Map qualified as Map
import FindTests.CliInput
-- import FindTests.Load
-- import FindTests.Parse
import FindTests.StolenFromIntegration
import Imports as I

-- import System.FilePath
-- import System.FilePath (takeBaseName)

-- usage:
--
-- >>> $ make -C ~/src/wire-server c package=find-tests
-- >>> $ cd ~/src/wire-server && ./dist/find-tests ./tools/find-tests/test-data/prod-run.json

main :: IO ()
main = do
  [cliInputFile] <- getArgs

  CliInput cliInput <-
    either (error . show . ((cliInputFile <> ": ") <>)) pure
      =<< (eitherDecode <$> BS.readFile cliInputFile)

  let flatten :: forall e v. (e ~ (), v ~ (), Show e) => [[[Either e v]]] -> [v]
      flatten nestedInput = if I.null bad then good else error $ errmsg <> show bad
        where
          (bad, good) = I.partitionEithers . join . join $ nestedInput
          -- TODO: lookup failure msg: "if you think the function should be found, this could be
          -- because the parser is ignoring the function due to some peculiarity in the syntax of
          -- the target function.  please open a ticket."
          errmsg = ""

  _cliOutput :: [()] <- fmap flatten . forM (Map.toList cliInput) $
    \(packageName, moduleMapping) -> do
      parsed <- collectTestsInModule packageName `mapM` (Map.keys moduleMapping)

      error $ show parsed

  {-
            let mapping :: Map String FoundTestCase
                mapping = parseTestCases absTargetFile parsed

            -- for debugging:
            -- I.putStrLn . showSDoc dflags . showAstDataFull $ parsed
            -- liftIO $ BS.putStrLn $ encode mapping

            --  . fmap fromJust $ flip Map.lookup mapping <$> testCaseNames

            error . BS.unpack . encodePretty $ cliOutput
  -}
  pure undefined
