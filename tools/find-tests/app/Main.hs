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
import Data.ByteString.Lazy.Char8 as BS
import Data.Map qualified as Map
import FindTests.Load
import FindTests.Parse
import GHC
import GHC.Driver.Flags
import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Hs.Dump
import GHC.Paths (libdir)
import GHC.Utils.Logger
import Imports as I
import System.FilePath (takeBaseName)

-- usage:
--
-- >>> $ make -C ~/src/wire-server c package=find-tests
-- >>> $ cd ~/src/wire-server && echo 'module Tests where\n\nsub :: Int\nsub = 3\n\nmain :: IO ()\nmain = print "yeay"\n > /tmp/Tests.hs
-- >>> $ cd ~/src/wire-server && ./dist/find-tests /tmp/Tests.hs test1

main :: IO ()
main = do
  [targetFile, targetFunction] <- getArgs

  runApp $ \dflags -> do
    parsed <- loadHsModule targetFile
    I.putStrLn . showSDoc dflags . showAstDataFull $ parsed
    let mapping = parseTestCases dflags parsed

    -- TODO: lookup failure msg: "if you think the function should be found, this could be
    -- because the parser is ignoring the function due to some peculiarity in the syntax of
    -- the target function.  please open a ticket."

    liftIO $ BS.putStrLn $ encode mapping
    print (Map.lookup targetFunction mapping)
