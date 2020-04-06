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

module API.User
  ( tests,
    ConnectionLimit (..),
  )
where

import qualified API.User.Account
import qualified API.User.Auth
import qualified API.User.Client
import qualified API.User.Connection
import qualified API.User.Handles
import qualified API.User.PasswordReset
import qualified API.User.Property
import qualified API.User.RichInfo
import API.User.Util
import Bilge hiding (accept, timeout)
import qualified Brig.AWS as AWS
import qualified Brig.Options as Opt
import qualified Brig.ZAuth as ZAuth
import Data.List.NonEmpty (NonEmpty ((:|)))
import Imports
import Test.Tasty hiding (Timeout)
import Util
import Util.Options.Common

tests :: Opt.Opts -> Manager -> Brig -> Cannon -> CargoHold -> Galley -> Nginz -> AWS.Env -> IO TestTree
tests conf p b c ch g n aws = do
  let cl = ConnectionLimit $ Opt.setUserMaxConnections (Opt.optSettings conf)
  let at = Opt.setActivationTimeout (Opt.optSettings conf)
  z <- mkZAuthEnv (Just conf)
  return $
    testGroup
      "user"
      [ API.User.Client.tests cl at conf p b c g,
        API.User.Account.tests cl at conf p b c ch g aws,
        API.User.Auth.tests conf p z b g n,
        API.User.Connection.tests cl at conf p b c g,
        API.User.Handles.tests cl at conf p b c g,
        API.User.PasswordReset.tests cl at conf p b c g,
        API.User.Property.tests cl at conf p b c g,
        API.User.RichInfo.tests cl at conf p b c g
      ]

mkZAuthEnv :: Maybe Opt.Opts -> IO ZAuth.Env
mkZAuthEnv config = do
  Just (sk :| sks) <- join $ optOrEnv (ZAuth.readKeys . Opt.privateKeys . Opt.zauth) config ZAuth.readKeys "ZAUTH_PRIVKEYS"
  Just (pk :| pks) <- join $ optOrEnv (ZAuth.readKeys . Opt.privateKeys . Opt.zauth) config ZAuth.readKeys "ZAUTH_PUBKEYS"
  ZAuth.mkEnv (sk :| sks) (pk :| pks) ZAuth.defSettings
