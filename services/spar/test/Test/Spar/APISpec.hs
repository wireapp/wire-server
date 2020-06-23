{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Test.Spar.APISpec where

import Arbitrary ()
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Metrics.Servant (routesToPaths)
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Proxy (Proxy (Proxy))
import Imports
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Spar.Types (IdPMetadataInfo (IdPMetadataValue), SsoSettings)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM'.
  validateEveryToJSON (Proxy @API.OutsideWorldAPI)
  it "api consistency" $ do
    pathsConsistencyCheck (routesToPaths @API.API) `shouldBe` mempty
  it "roundtrip: IdPMetadataInfo" . property $ \(val :: IdPMetadataInfo) -> do
    let withoutRaw (IdPMetadataValue _ x) = x
    (withoutRaw <$> (Aeson.eitherDecode . Aeson.encode) val) `shouldBe` Right (withoutRaw val)
  describe "SsoSettings JSON instance" $ do
    it "always has and requires the field default_sso_code" $
      property $
        \(ssoSettings :: SsoSettings) -> do
          let object = Aeson.toJSON ssoSettings
          let objectWithoutKey = Lens.over Aeson._Object (HM.delete "default_sso_code") $ object
          (HM.lookup "default_sso_code" =<< Lens.preview Aeson._Object object)
            `shouldSatisfy` isJust
          Aeson.parseMaybe (Aeson.parseJSON @SsoSettings) objectWithoutKey
            `shouldSatisfy` isNothing
