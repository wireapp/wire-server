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

-- | A set of acceptance tests that you can use to test that your server is
module Test.AcceptanceSpec where

import Test.Hspec (Spec, describe)
import Web.Scim.Capabilities.MetaSchema (empty)
import Web.Scim.Server (app)
import Web.Scim.Server.Mock
import Web.Scim.Test.Acceptance (defAcceptanceConfig, microsoftAzure, responsesFullyKnown)

spec :: Spec
spec = do
  let app' = do
        storage <- emptyTestStorage
        pure (app @Mock empty (nt storage))
  describe "Azure" $ microsoftAzure ((defAcceptanceConfig @Mock app') {responsesFullyKnown = True})
