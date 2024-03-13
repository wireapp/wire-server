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
import Control.Monad.Extra
import Data.Function
import Data.Maybe
import System.Process
import System.Timeout
import Testlib.ModService.ServiceHandles
import Testlib.Prelude

testServiceHandles :: App ()
testServiceHandles = do
  (stdo, stde) <- liftIO runTestServiceHandles
  stdo
    `shouldContainString` "[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] 0\n\
                          \[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] 1\n\
                          \[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] 2\n\
                          \[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] 3\n\
                          \[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] 4\n\
                          \[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] 5\n\
                          \[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] 6\n\
                          \[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] 7\n"
  stde `shouldMatch` "[/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b@local] errmsg\n"

runTestServiceHandles :: IO (String, String)
runTestServiceHandles = fmap (fromMaybe (error "*** timeout")) . timeout 4_000_000 $ do
  let exe = "/tmp/tmp-42956614-e50a-11ee-8c4b-6b596d54b36b"
      dom = "local"
      noisy = False

  writeFile exe "#!/bin/bash\necho errmsg >&2\nfor i in `seq 0 100`; do echo $i; sleep 0.1; done\n"
  _ <- system $ "chmod +x " <> exe
  (_, Just stdoutHdl, Just stderrHdl, ph) <- createProcess (proc exe []) {std_out = CreatePipe, std_err = CreatePipe}

  (out1, out2) <- mkChans stdoutHdl
  (err1, err2) <- mkChans stderrHdl

  when noisy $ do
    void $ forkIO $ logChanToConsole exe dom out1
    void $ forkIO $ logChanToConsole exe dom err1

  threadDelay 1_000_000
  terminateProcess ph

  (,)
    <$> flushChan exe dom out2
    <*> flushChan exe dom err2
