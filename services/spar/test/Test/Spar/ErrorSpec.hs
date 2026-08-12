{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Spar.ErrorSpec where

import Data.Aeson (eitherDecode')
import Data.Aeson.QQ (aesonQQ)
import Imports
import qualified SAML2.WebSSO as SAML
import Servant (ServerError (..))
import Spar.Error
import Test.Hspec
import qualified Web.Scim.Schema.Error as Scim

spec :: Spec
spec = describe "sparToServerError" $ do
  -- RFC 7644 section 3.12 requires that the response body of a SCIM error *is*
  -- the SCIM error object, not a wire-server 'Wai.Error' with the SCIM error
  -- object nested (double-encoded) into its 'message' field.
  it "renders a SCIM error as the bare RFC 7644 error object" $ do
    let scimErr =
          Scim.badRequest
            Scim.InvalidValue
            (Just "Could not process externalId.")
        serverErr = sparToServerError (SAML.CustomError (SparScimError scimErr))
    eitherDecode' (errBody serverErr)
      `shouldBe` Right
        [aesonQQ|
                  {
                    "detail": "Could not process externalId.",
                    "schemas": [
                      "urn:ietf:params:scim:api:messages:2.0:Error"
                    ],
                    "scimType": "invalidValue",
                    "status": "400"
                  }|]
    errHTTPCode serverErr `shouldBe` 400
    lookup "Content-Type" (errHeaders serverErr)
      `shouldBe` Just "application/scim+json;charset=utf-8"
