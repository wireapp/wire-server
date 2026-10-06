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

module Wire.EmailSending.SESSpec (spec) where

import Imports
import Network.HTTP.Types (status400, status500)
import Network.Wai.Utilities.Error qualified as Wai
import Test.Hspec
import Wire.EmailSending.SES
import Wire.Error (httpErrorToWaiError)

spec :: Spec
spec = do
  describe "emailSendingErrorToHttpError" $ do
    it "maps an SES invalid-domain error to invalid-email" $ do
      let err = httpErrorToWaiError $ emailSendingErrorToHttpError SESInvalidDomain
      Wai.code err `shouldBe` status400
      Wai.label err `shouldBe` "invalid-email"

    it "maps an unexpected SES error to server-error" $ do
      let err = httpErrorToWaiError $ emailSendingErrorToHttpError EmailSendingAWSGeneralError
      Wai.code err `shouldBe` status500
      Wai.label err `shouldBe` "server-error"
