{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Imports
import Options.Applicative hiding (header)
import Options.Applicative qualified as O
import Options.Applicative.Types
import Sodium.Crypto.Sign

data Mode
  = GenKeyPair
  deriving (Eq, Show, Enum)

main :: IO ()
main = do
  mode <- execParser (info (helper <*> options) desc)
  go mode
  where
    desc = O.header "Generate key-pair used for creating/validation access tokens." <> fullDesc

go :: Mode -> IO ()
go GenKeyPair = do
  (p, s) <- newKeyPair
  putStrLn $ "public: " <> show p
  putStrLn $ "secret: " <> show s

options :: Parser Mode
options =
  -- Seems absurd to have only one mode, this only exist for backwards
  -- compatibility. There used to be a lot of modes, but they were never used.
  -- There was a different mode than 'GenKeyPair' as the deafult, so making this
  -- the new default will be a breaking change which could fly under the radar.
  option toMode $
    long "mode"
      <> short 'm'
      <> metavar "STRING"
      <> help "gen-keypair"
  where
    toMode =
      readerAsk >>= \case
        "gen-keypair" -> pure GenKeyPair
        other -> readerError $ "invalid mode: " <> other
