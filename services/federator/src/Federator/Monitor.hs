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

module Federator.Monitor where

import Control.Exception (bracket)
import Control.Lens (view)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as Set
import Federator.Env
import Federator.Options (RunSettings (..))
import Imports
import Polysemy (Embed, Members, Sem, embed)
import qualified Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import System.FilePath (takeDirectory)
import System.INotify
import qualified System.Logger.Message as Log

monitorEvents :: [EventVariety]
monitorEvents = [CloseWrite, MoveIn, Create]

withMonitor :: Env -> RunSettings -> IO a -> IO a
withMonitor env rs action =
  bracket
    (run (monitorCertificates rs))
    (run . stopMonitoringCertificates :: [WatchDescriptor] -> IO ())
    (const action)
  where
    run :: Sem '[TinyLog, Embed IO] a -> IO a
    run = Polysemy.runM . Log.runTinyLog (view applog env)

stopMonitoringCertificates ::
  (Members '[TinyLog, Embed IO] r) =>
  [WatchDescriptor] ->
  Sem r ()
stopMonitoringCertificates = traverse_ stop
  where
    stop wd = do
      embed $ removeWatch wd
      Log.debug $
        Log.msg ("stopped watching file" :: Text)
          . Log.field "descriptor" (show wd)

monitorCertificates ::
  (Members '[TinyLog, Embed IO] r) =>
  RunSettings ->
  Sem r [WatchDescriptor]
monitorCertificates rs = do
  inotify <- embed initINotify
  let watch path = do
        wd <- embed . addWatch inotify monitorEvents (B8.pack path) $ \e ->
          print e
        Log.debug $
          Log.msg ("watching file" :: Text)
            . Log.field "descriptor" (show wd)
            . Log.field "file" path
        pure wd
  Log.debug $
    Log.msg ("inotify initialized" :: Text)
      . Log.field "inotify" (show inotify)
  traverse watch (toList (certificatePaths rs))

certificatePaths :: RunSettings -> Set FilePath
certificatePaths rs =
  Set.fromList $
    concat
      [ cert1 =<< maybeToList (remoteCAStore rs),
        cert1 (clientCertificate rs),
        cert1 (clientPrivateKey rs)
      ]
  where
    cert1 :: FilePath -> [FilePath]
    cert1 path = [path, takeDirectory path]
