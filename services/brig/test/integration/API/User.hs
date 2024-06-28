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

module API.User
  ( tests,
    ConnectionLimit (..),
  )
where

import API.User.Account qualified
import API.User.Auth qualified
import API.User.Client qualified
import API.User.Connection qualified
import API.User.Handles qualified
import API.User.PasswordReset qualified
import API.User.Property qualified
import API.User.RichInfo qualified
import API.User.Util
import Bilge hiding (accept, timeout)
import Brig.Options qualified as Opt
import Brig.ZAuth qualified as ZAuth
import Cassandra qualified as DB
import Data.List.NonEmpty (NonEmpty ((:|)))
import Imports
import Test.Tasty hiding (Timeout)
import Util
import Util.AWS (UserJournalWatcher)
import Util.Options.Common
import Wire.API.AWS qualified as AWS
import Wire.API.Federation.Component

tests ::
  Opt.Opts ->
  FedClient 'Brig ->
  Manager ->
  Brig ->
  Cannon ->
  CargoHold ->
  Galley ->
  Nginz ->
  AWS.Env ->
  DB.ClientState ->
  UserJournalWatcher ->
  IO TestTree
tests conf fbc p b c ch g n aws db userJournalWatcher = do
  let cl = ConnectionLimit $ Opt.setUserMaxConnections (Opt.optSettings conf)
  let at = Opt.setActivationTimeout (Opt.optSettings conf)
  z <- mkZAuthEnv (Just conf)
  pure $
    testGroup
      "user"
      [ API.User.Client.tests cl at conf p db n b c g,
        API.User.Account.tests cl at conf p b c ch g aws userJournalWatcher,
        API.User.Auth.tests conf p z db b g n,
        API.User.Connection.tests cl at p b c g fbc db,
        API.User.Handles.tests cl at conf p b c g,
        API.User.PasswordReset.tests db cl at conf p b c g,
        API.User.Property.tests cl at conf p b c g,
        API.User.RichInfo.tests cl at conf p b c g
      ]

mkZAuthEnv :: Maybe Opt.Opts -> IO ZAuth.Env
mkZAuthEnv config = do
  Just (sk :| sks) <- join $ optOrEnv (ZAuth.readKeys . Opt.privateKeys . Opt.zauth) config ZAuth.readKeys "ZAUTH_PRIVKEYS"
  Just (pk :| pks) <- join $ optOrEnv (ZAuth.readKeys . Opt.privateKeys . Opt.zauth) config ZAuth.readKeys "ZAUTH_PUBKEYS"
  ZAuth.mkEnv (sk :| sks) (pk :| pks) ZAuth.defSettings
