-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.

module Wire.PostgresSpec where

import Data.HashMap.Strict qualified as HashMap
import Imports
import OpenTelemetry.Trace (SpanArguments (..), toAttribute)
import Test.Hspec
import Wire.Postgres (dbStatementSpanArguments)

spec :: Spec
spec =
  describe "dbStatementSpanArguments" $ do
    it "stores the query template in db.statement" $ do
      attributes (dbStatementSpanArguments "SELECT 1")
        `shouldBe` HashMap.fromList @Text [("db.statement", toAttribute ("SELECT 1" :: Text))]
