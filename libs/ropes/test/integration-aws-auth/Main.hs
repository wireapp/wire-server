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

module Main where

import Data.Time.Clock
import Imports
import Network.HTTP.Client
import Ropes.Aws
import qualified System.Logger as Logger

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  l <- Logger.new Logger.defSettings -- TODO: use mkLogger'?
  m <- newManager defaultManagerSettings
  e <- newEnv l m Nothing
  forever $ do
    now <- getCurrentTime
    crd <- getCredentials e
    putStrLn $ "Time: " ++ show now ++ " - Credentials: " ++ showCreds crd
    threadDelay $ 60 * 1000 * 1000 -- every minute
  where
    showCreds (Credentials k s _ t) =
      "AccessKeyId: " ++ show k ++ " - "
        ++ "SecretAccessKey: "
        ++ show s
        ++ " - "
        ++ "SessionToken: "
        ++ show t
