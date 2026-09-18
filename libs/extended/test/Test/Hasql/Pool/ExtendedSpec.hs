-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Hasql.Pool.ExtendedSpec where

import Control.Exception (try)
import Data.Map qualified as Map
import Hasql.Pool.Extended (runConnStrParser)
import Imports
import PostgresqlConnectionString qualified
import System.IO.Error (ioeGetErrorString)
import Test.Hspec

spec :: Spec
spec =
  describe "runConnStrParser / fromKeyValueParams" $ do
    it "parses a valid key/value connection string" $ do
      let params =
            Map.fromList
              [ ("host", "localhost"),
                ("port", "5432"),
                ("dbname", "wire-server"),
                ("user", "wire")
              ]
      connStr <- runConnStrParser $ PostgresqlConnectionString.fromKeyValueParams params
      PostgresqlConnectionString.toUrl connStr `shouldBe` "postgresql://wire@localhost:5432/wire-server"

    it "applies a single port to every host" $ do
      let params =
            Map.fromList
              [ ("host", "IP1,IP2,IP3"),
                ("port", "5000")
              ]
      connStr <- runConnStrParser $ PostgresqlConnectionString.fromKeyValueParams params
      PostgresqlConnectionString.toUrl connStr `shouldBe` "postgresql://IP1:5000,IP2:5000,IP3:5000"

    it "applies a single port to every host and keeps the dbname" $ do
      let params =
            Map.fromList
              [ ("host", "IP1,IP2,IP3"),
                ("port", "5000"),
                ("dbname", "wire-server")
              ]
      connStr <- runConnStrParser $ PostgresqlConnectionString.fromKeyValueParams params
      PostgresqlConnectionString.toUrl connStr `shouldBe` "postgresql://IP1:5000,IP2:5000,IP3:5000/wire-server"

    it "surfaces a mismatched host/port count as an exception with the parse error" $ do
      let params =
            Map.fromList
              [ ("host", "host1,host2,host3"),
                ("port", "5432,5433")
              ]
      result <- try @IOException $ runConnStrParser (PostgresqlConnectionString.fromKeyValueParams params)
      case result of
        Left e -> ioeGetErrorString e `shouldBe` "could not match 2 port numbers to 3 hosts"
        Right _ -> expectationFailure "expected a parse failure"
