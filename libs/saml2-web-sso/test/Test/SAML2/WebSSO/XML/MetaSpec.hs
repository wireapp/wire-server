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

module Test.SAML2.WebSSO.XML.MetaSpec
  ( spec,
  )
where

import Control.Lens
import Data.EitherR
import Data.String.Conversions
import SAML2.WebSSO
import SAML2.WebSSO.Test.Util
import Test.Hspec
import Text.XML
import URI.ByteString.QQ

spec :: Spec
spec = do
  describe "spDesc" $ do
    it "does not smoke" $ do
      testCtx1 <- mkTestCtxSimple
      have <-
        ioFromTestSP testCtx1 $
          mkSPMetadata
            "drnick"
            [uri|http://example.com/|]
            [uri|http://example.com/sso/login|]
            [fallbackContact]
      let want = testSPMetadata (have ^. spID)
      have `shouldBe` want
  describe "spMeta" $ do
    it "does not smoke" $ do
      let given = testSPMetadata $ ID "_e3a565aa-1392-4446-a4d6-3771453808f0"
          want = renderToDocument given
      have :: Either String Document <- fmapL show . parseText def . cs <$> readSampleIO "our-spssodescriptor.xml"
      have `shouldBe` Right want

testSPMetadata :: ID SPMetadata -> SPMetadata
testSPMetadata mid =
  SPMetadata
    { _spID = mid,
      _spValidUntil = fromTime $ addTime (60 * 60 * 24 * 365) timeNow,
      _spCacheDuration = 2592000,
      _spOrgName = "drnick",
      _spOrgDisplayName = "drnick",
      _spOrgURL = [uri|http://example.com/|],
      _spResponseURL = [uri|http://example.com/sso/login|],
      _spContacts = [fallbackContact]
    }
