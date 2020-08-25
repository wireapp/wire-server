{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module System.MakeDeb
  ( makeDeb,
    options,
    MakeDebOpts (..),
  )
where

import Data.Text (pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString)
import Imports hiding (FilePath)
import Options.Applicative
import Shelly hiding (FilePath)
import System.MakeDeb.FileUtils

data MakeDebOpts = MakeDebOpts
  { name :: !Text,
    version :: !Text,
    build :: !Text,
    arch :: !Text,
    deb :: !FilePath,
    out :: !FilePath
  }
  deriving (Eq, Show)

options :: Parser MakeDebOpts
options =
  MakeDebOpts
    <$> optName
    <*> optVersion
    <*> optBuild
    <*> optArch
    <*> optDeb
    <*> optOut
  where
    optName =
      txtOption $
        long "name"
          <> short 'n'
          <> metavar "STRING"
          <> help "artifact name"
    optVersion =
      txtOption $
        long "version"
          <> short 'v'
          <> metavar "STRING"
          <> help "artifact version"
    optBuild =
      txtOption $
        long "build"
          <> short 'b'
          <> metavar "STRING"
          <> help "build number"
          <> value "0"
          <> showDefault
    optArch =
      txtOption $
        long "architecture"
          <> short 'a'
          <> metavar "STRING"
          <> help "architecture"
    optDeb =
      fileOption $
        long "debian-dir"
          <> short 'd'
          <> metavar "PATH"
          <> help "debian directory"
    optOut =
      fileOption $
        long "output-dir"
          <> short 'o'
          <> metavar "PATH"
          <> help "output directory"
    txtOption = fmap pack . strOption
    fileOption = fmap decodeString . strOption

makeDeb :: MakeDebOpts -> IO ()
makeDeb opts = shelly . silently . withTmpDir $ \tmp -> do
  void . escaping False $ run "cp" ["-R", "-L", Text.pack $ encodeString (deb opts) </> "*", Text.pack tmp]
  let opts' = opts {deb = decodeString tmp}
  substitute opts'
  package opts'

package :: MakeDebOpts -> Sh ()
package MakeDebOpts {..} = do
  let f = name <> "_" <> version <> "+" <> build <> "_" <> arch
  run_ "dpkg-deb" ["-b", Text.pack $ encodeString deb, Text.pack $ encodeString out </> fromText f <.> "deb"]

substitute :: MakeDebOpts -> Sh ()
substitute MakeDebOpts {..} = flip traverseFiles (encodeString deb) $ \fname -> do
  mime <- run "file" ["--brief", "--mime", Text.pack fname]
  when ("text/plain" `Text.isPrefixOf` mime) $
    replace
      [ ("<<VERSION_NUMBER>>", version),
        ("<<BUILD_NUMBER>>", build),
        ("<<ARCHITECTURE>>", arch)
      ]
      fname

replace :: (MonadIO m) => [(Text, Text)] -> String -> m ()
replace maps fname = liftIO $ do
  txt <- Text.readFile fname
  Text.writeFile fname $ foldl (\x (a, b) -> Text.replace a b x) txt maps
