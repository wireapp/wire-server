{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-orphans #-}

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

module Test.SAML2.UtilSpec
  ( spec,
  )
where

import SAML2.Util
import SAML2.WebSSO.Test.Arbitrary ()
import Test.Hspec
import URI.ByteString.QQ

-- | @uriSegments "/one/two" == uriSegments "one/two/" == uriSegments "///one//two///" == ["one", "two"]@
spec :: Spec
spec = do
  describe "(-/)" $ do
    it "a)" $ "one" -/ "two" `shouldBe` "/one/two"
    it "b)" $ "/one" -/ "two" `shouldBe` "/one/two"
    it "c)" $ "/one/" -/ "two" `shouldBe` "/one/two"
    it "d)" $ "/one" -/ "/two" `shouldBe` "/one/two"
    it "e)" $ "/one/" -/ "/two" `shouldBe` "/one/two"
    it "f)" $ "/one/" -/ "/two/" `shouldBe` "/one/two"
    it "g)" $ "///one///" -/ "///two///" `shouldBe` "/one/two"
  describe "(=/)" $ do
    it "a)" $ [uri|http://a.b/ef|] =/ "gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "b)" $ [uri|http://a.b/ef/|] =/ "gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "c)" $ [uri|http://a.b/ef|] =/ "/gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "d)" $ [uri|http://a.b/ef/|] =/ "/gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "e)" $ [uri|http://a.b/ef////|] =/ "gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "f)" $ [uri|http://a.b/ef|] =/ "//////gh" `shouldBe` [uri|http://a.b/ef/gh|]
