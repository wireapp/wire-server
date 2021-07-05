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

module Test.Wire.API.Federation.Golden.GoldenSpec where

import Imports
import Test.Hspec
import qualified Test.Wire.API.Federation.Golden.MessageSendResponse as MessageSendResponse
import Test.Wire.API.Federation.Golden.Runner (testObjects)

spec :: Spec
spec =
  describe "Golden tests" $
    testObjects
      [ (MessageSendResponse.testObject_MessageSendReponse1, "testObject_MessageSendReponse1.json"),
        (MessageSendResponse.testObject_MessageSendReponse2, "testObject_MessageSendReponse2.json"),
        (MessageSendResponse.testObject_MessageSendReponse3, "testObject_MessageSendReponse3.json"),
        (MessageSendResponse.testObject_MessageSendReponse4, "testObject_MessageSendReponse4.json"),
        (MessageSendResponse.testObject_MessageSendReponse5, "testObject_MessageSendReponse5.json")
      ]
