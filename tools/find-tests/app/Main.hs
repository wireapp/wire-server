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
import GHC.Hs
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

{-

/integration/test/Test/AccessUpdate.hs,testAccessUpdateGuestRemoved
/libs/zauth/test/ZAuth.hs,testExpired
/services/brig/test/integration/API/User/Account.hs,testCreateUserWithInvalidVerificationCode
/services/brig/test/integration/API/User/Account.hs,testCreateUserEmptyName
/services/brig/test/integration/API/User/Account.hs,testCreateUserLongName
/services/brig/test/integration/API/User/Account.hs,testCreateUserConflict
/services/brig/test/integration/API/User/Account.hs,testCreateUserInvalidEmailOrPhone
/services/brig/test/integration/API/User/Auth.hs,testLoginFailure
/services/brig/test/integration/API/User/Auth.hs,testLimitRetries
/services/brig/test/integration/API/User/Auth.hs,testInvalidCookie
/services/brig/test/integration/API/User/Auth.hs,testTooManyCookies
/services/brig/test/integration/API/User/Client.hs,testAddGetClientMissingCode
/services/brig/test/integration/API/User/Client.hs,testAddGetClientWrongCode
/services/brig/test/integration/API/User/Client.hs,testAddGetClientCodeExpired
/services/brig/test/integration/API/User/Client.hs,testTooManyClients
/services/brig/test/integration/API/User/Client.hs,testRemoveClient
/services/brig/test/integration/API/User/Client.hs,testRemoveClientShortPwd
/services/brig/test/integration/API/User/Client.hs,testRemoveClientIncorrectPwd
/services/brig/test/integration/API/User/Client.hs,testAddMultipleTemporary

-- hm...  maybe it's easier to extract this list from the html audit report?

/services/federator/test/integration/Test/Federator/IngressSpec.hs,
/services/federator/test/integration/Test/Federator/InwardSpec.hs,
/services/federator/test/unit/Test/Federator/InternalServer.hs,
/services/federator/test/unit/Test/Federator/Options.hs,
/services/federator/test/unit/Test/Federator/Remote.hs,
/services/federator/test/unit/Test/Federator/Validation.hs,
/services/galley/test/integration/API.hs,
/services/galley/test/integration/API/Teams.hs,
/services/galley/test/integration/API/Federation.hs,
/services/spar/test-integration/Test/Spar/APISpec.hs,
/services/spar/test-integration/Test/Spar/Scim/AuthSpec.hs,
/services/spar/test-integration/Test/Spar/Scim/UserSpec.hs,

file not found:
/services/brig/test/integration/API/UserHandles.hs,

-}

main :: IO ()
main = do
  [targetFile, targetFunction] <- getArgs

  runApp $ \dflags -> do
    parsed <- loadHsModule targetFile

    let docs = {- fmap (anchor . getLoc) $ priorComments $ comments $ hsmodAnn $ unLoc $ -} parsed

    -- I.putStrLn . showSDoc dflags . showAstDataFull $ docs
    let mapping = parseTestCases dflags targetFile parsed

    -- TODO: lookup failure msg: "if you think the function should be found, this could be
    -- because the parser is ignoring the function due to some peculiarity in the syntax of
    -- the target function.  please open a ticket."

    liftIO $ BS.putStrLn $ encode mapping
    print (Map.lookup targetFunction mapping)
