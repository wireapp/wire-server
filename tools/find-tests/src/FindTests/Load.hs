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

module FindTests.Load
  ( runApp,
    loadHsModule,
  )
where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS
import Data.Map qualified as Map
import FindTests.Parse
import GHC
import GHC.Driver.Flags
import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Hs.Dump
import GHC.Parser qualified as P
import GHC.Parser.Lexer qualified as P
import GHC.Paths (libdir)
import GHC.Utils.Logger
import Imports as I
import System.FilePath (takeBaseName)

runApp :: (DynFlags -> Ghc a) -> IO a
runApp action = do
  logger <- initLogger
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    prettyPrintGhcErrors logger $ do
      runGhc (Just libdir) $ do
        dflags <- wireGhcFlags
        action dflags

-- | Load both token stream (needed for comment processing) and AST (needed for fun
-- declaration processing).
--
-- NB: if we need to read comments, maybe the easiest way is to parse the token stream, filter
-- for comment tokens, and find the latest one before a declaration.  but it's probably much
-- better to forbid comments *above* type signatures and require to put them in the function
-- body, then they will be included without extra effort.
loadHsModule :: FilePath -> Ghc ParsedSource
loadHsModule targetFile = do
  let targetModule = takeBaseName targetFile
  target <- guessTarget targetFile Nothing Nothing
  setTargets [target]
  _ <- load LoadAllTargets
  modSum <- getModSummary $ mkModuleName targetModule
  parsedSource <$> parseModule modSum

----------------------------------------------------------------------
-- internal

wireGhcFlags :: Ghc DynFlags
wireGhcFlags = do
  dflags <- getSessionDynFlags
  let dflags' =
        dflags
          `gopt_set` Opt_KeepRawTokenStream
          `dopt_set` Opt_D_dump_debug
  setSessionDynFlags dflags'
  pure dflags'
