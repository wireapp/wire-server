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

import Data.Char
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = \desc info hooks flags -> do
          withLibLBI desc info $ \_ lib -> do
            let base = autogenComponentModulesDir info lib </> "Brig" </> "Docs"
            generateDocs base "swagger.md"
          buildHook simpleUserHooks desc info hooks flags
      }

generateDocs :: FilePath -> FilePath -> IO ()
generateDocs base src = do
  contents <- readFile ("docs" </> src)
  let name = moduleName src
      dest = base </> (moduleName src <> ".hs")
  createDirectoryIfMissing True base
  putStrLn ("Generating " <> dest <> " ...")
  let out =
        unlines
          [ "module Brig.Docs." <> name <> " where",
            "",
            "import Imports",
            "",
            "contents :: Text",
            "contents = " ++ show contents
          ]
  writeFile dest out

moduleName :: String -> String
moduleName = go . dropExtension
  where
    go [] = []
    go (c : cs) = case break (== '-') cs of
      (w, rest) ->
        (toUpper c : w) <> case rest of
          ('-' : name) -> go name
          _ -> []
