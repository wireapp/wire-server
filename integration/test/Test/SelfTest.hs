-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.SelfTest where

import Control.Concurrent
import Data.Function
import Data.Maybe
import Testlib.ModService.ServiceHandles
import Testlib.Prelude
import UnliftIO.Directory

testServiceHandles :: App ()
testServiceHandles = do
  -- The name was generated with a roll of a fair dice
  let exe = "/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b"
      execName = "test-exec"
      dom = "test-domain"

  writeFile
    exe
    "#!/usr/bin/env bash\n\
    \echo errmsg >&2\n\
    \for i in `seq 0 100`; do\n\
    \  echo $i\n\
    \  sleep 0.1\n\
    \done\n"
  perms <- getPermissions exe
  setPermissions exe (setOwnerExecutable True perms)
  serviceInstance <- liftIO $ startServiceInstance exe [] Nothing exe execName dom
  liftIO $ threadDelay 1_000_000
  cleanupService serviceInstance
  processState <- liftIO $ flushProcessState serviceInstance
  processState
    `shouldContainString` "=== stdout: ============================================\n\
                          \[test-exec@test-domain] 0\n\
                          \[test-exec@test-domain] 1\n\
                          \[test-exec@test-domain] 2\n\
                          \[test-exec@test-domain] 3\n\
                          \[test-exec@test-domain] 4\n\
                          \[test-exec@test-domain] 5\n\
                          \[test-exec@test-domain] 6\n\
                          \[test-exec@test-domain] 7\n"
  processState
    `shouldContainString` "=== stderr: ============================================\n\
                          \[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] errmsg\n"
