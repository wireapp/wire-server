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

module Test.Spar.TypesSpec where

import Data.Id
import Data.UUID
import Imports
import Spar.Types
import Test.Hspec
import Test.Spar.Roundtrip.ByteString (testRoundTrip)
import URI.ByteString
import URI.ByteString.QQ
import Web.Cookie

spec :: Spec
spec = do
  describe "mkVerdictGrantedFormatMobile" $ do
    it "1" $ do
      mkVerdictGrantedFormatMobile [uri|wire://granted/$cookie/$userid|] def (Id nil)
        `shouldBe` Right [uri|wire://granted/name=value/00000000-0000-0000-0000-000000000000|]
    it "2" $ do
      mkVerdictGrantedFormatMobile ([uri|http://$cookie:1039/granted|] :: URI) def (Id nil)
        `shouldBe` Right [uri|http://name=value:1039/granted|]
  describe "mkVerdictDeniedFormatMobile" $ do
    it "1" $ do
      mkVerdictDeniedFormatMobile [uri|wire://$label|] "forbidden"
        `shouldBe` Right [uri|wire://forbidden|]
    it "2" $ do
      mkVerdictDeniedFormatMobile [uri|http://bad/?label=$label|] "forbidden"
        `shouldBe` Right [uri|http://bad/?label=forbidden|]
  describe "(To/From)Bytestring Roundtrips" $ do
    testRoundTrip @ScimTokenHash
