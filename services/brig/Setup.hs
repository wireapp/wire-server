{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

import Control.Exception
import Control.Monad
import Data.Char
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.PackageDescription
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = \desc info hooks flags -> do
          generate desc info
          buildHook simpleUserHooks desc info hooks flags,
        replHook = \desc info hooks flags args -> do
          generate desc info
          replHook simpleUserHooks desc info hooks flags args
      }

generate :: PackageDescription -> LocalBuildInfo -> IO ()
generate desc info = withLibLBI desc info $ \_ lib -> do
  let base = autogenComponentModulesDir info lib </> "Brig" </> "Docs"
  generateDocs base "swagger.md"

generateDocs :: FilePath -> FilePath -> IO ()
generateDocs base src = do
  contents <- readFile ("docs" </> src)
  let name = moduleName src
      dest = base </> (moduleName src <> ".hs")
  createDirectoryIfMissing True base
  let out =
        Text.unlines
          [ "module Brig.Docs." <> Text.pack name <> " where",
            "",
            "import Imports",
            "",
            "contents :: Text",
            "contents = " <> Text.pack (show contents)
          ]
  writeFileIfChanged dest out

writeFileIfChanged :: FilePath -> Text -> IO ()
writeFileIfChanged fp c' = do
  changed <- handle @IOException (const (pure True)) $ do
    c <- Text.readFile fp
    pure $ c /= c'
  when changed $ do
    putStrLn ("Generating " <> fp <> " ...")
    Text.writeFile fp c'

moduleName :: String -> String
moduleName = go . dropExtension
  where
    go [] = []
    go (c : cs) = case break (== '-') cs of
      (w, rest) ->
        (toUpper c : w) <> case rest of
          ('-' : name) -> go name
          _ -> []
